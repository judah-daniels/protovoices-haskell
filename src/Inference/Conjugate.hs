{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Inference.Conjugate where

import           Control.Monad.Primitive        ( PrimMonad
                                                , PrimState
                                                )
import           Control.Monad.Reader           ( ReaderT
                                                , runReaderT
                                                )
import           Control.Monad.Reader.Class     ( MonadReader(ask) )
import           Control.Monad.State            ( StateT
                                                , runStateT
                                                , execStateT
                                                , evalStateT
                                                )
import           Control.Monad.State.Class      ( modify
                                                , put
                                                , get
                                                )
import           Control.Monad.Writer
import           Data.Dynamic                   ( Dynamic
                                                , toDyn
                                                , fromDynamic
                                                , Typeable
                                                )
import           Data.Kind
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Sequence                 as S
import           Data.Typeable                  ( Proxy(Proxy)
                                                , typeRep
                                                )
import qualified Data.Vector                   as V
import           Lens.Micro
import           Lens.Micro.Extras
import           System.Random.MWC.Probability
import           Lens.Micro.TH                  ( makeLenses )
import           GHC.TypeNats
import           GHC.Generics
import           Numeric.SpecFunctions          ( logChoose )
import           GHC.Float                      ( int2Double )

----------------------------------------------------------------
-- type classes and families for distributions and conjugates --
----------------------------------------------------------------

-- | Describes a family of distributions with a fixed form.
-- For example, a 'Bernoulli' distribution is parameterized by a probability @p@
-- and produces binary samples
-- (@True@ with probability @p@, @False@ with probability @1-p@).
--
-- Its 'Distribution' instance is:
-- > instance Distribution Bernoulli where
-- >   type Params Bernoulli = Double
-- >   type Support Bernoulli = Bool
-- >   distSample _ = uncurry bernoulli
-- >   distLogP _ p True = log p
-- >   distLogP _ p False = log (1 - p)
class Distribution a where
  type Params a :: Type
  type Support a :: Type
  distSample :: (PrimMonad m) => a -> Params a -> Prob m (Support a)
  distLogP :: a -> Params a -> Support a -> Double

-- | Used as a type-lifted kind for conjugate distribution pairs
data Conj a b = Conj a b

-- | Marks two distributions as a conjugate pair of prior and likelihood.
-- The property of such a pair is that the posterior has the same form as the prior
-- (including the same 'Params' and 'Support'),
-- and that its parameters can be obtained analytically from the parameters of the prior
-- and a set of observations.
--
-- The class method 'updatePrior' returns the parameters of the posterior
-- given the prior parameters after a single observation.
class (Distribution p, Distribution l, Support p ~ Params l) => Conjugate p l where
  priorSingleton :: p -- ^ provides a singleton instance of the prior distribution
                      -- in order to make sampling from priors easier.
  updatePrior :: l -> Params p -> Support l -> Params p

type family Hyper (a :: k) :: Type
type instance Hyper (Conj p l) = Params p

type family Probs (a :: k) :: Type
type instance Probs (Conj p l) = Support p

-- type family Value (a :: k) :: Type
-- type instance Value (Conj p l) = Support l

-- helper types for instantiating hyperparameters, parameters, and values
-- ----------------------------------------------------------------------

newtype HyperRep p l = HyperRep { runHyper :: Hyper (Conj p l) }
deriving instance Show (Hyper (Conj p l)) => Show (HyperRep p l)

type instance Hyper (a :: (Type -> Type -> Type) -> Type) = a HyperRep

newtype ProbsRep p l = ProbsRep { runProbs :: Probs (Conj p l)}
deriving instance Show (Probs (Conj p l)) => Show (ProbsRep p l)

type instance Probs (a :: (Type -> Type -> Type) -> Type) = a ProbsRep

-- newtype ValueRep p l = ValueRep { runValue :: Value (Conj p l) }
-- deriving instance Show (Value (Conj p l)) => Show (ValueRep p l)

-- type instance Value (a :: (Type -> Type -> Type) -> Type) = a ValueRep

-----------------------------------------------------
-- generic magic for conjugates and parameter sets --
-----------------------------------------------------

-- Jeffrey's prior
-- ---------------

class Jeffrey a where
  jeffreyPrior :: Hyper a

class GJeffrey t where
  gjeffreyPrior :: forall p. t p

instance GJeffrey V1 where
  gjeffreyPrior = undefined

instance GJeffrey U1 where
  gjeffreyPrior = U1

-- base case: k is a conjugate distribution
instance (Jeffrey (Conj p l)) => GJeffrey (K1 i (HyperRep p l)) where
  gjeffreyPrior = K1 $ HyperRep $ jeffreyPrior @(Conj p l)

-- recursive case: k is another record
instance (Jeffrey k, k HyperRep ~ Hyper k) => GJeffrey (K1 i (k HyperRep)) where
  gjeffreyPrior = K1 $ jeffreyPrior @k

instance (GJeffrey t) => GJeffrey (M1 i c (t :: Type -> Type)) where
  gjeffreyPrior = M1 gjeffreyPrior

instance (GJeffrey ta, GJeffrey tb) => GJeffrey (ta :*: tb) where
  gjeffreyPrior = gjeffreyPrior @ta :*: gjeffreyPrior @tb

instance (Generic (t HyperRep), GJeffrey (Rep (t HyperRep))) => Jeffrey (t :: (Type -> Type -> Type) -> Type) where
  jeffreyPrior = GHC.Generics.to (gjeffreyPrior @(Rep (t HyperRep)))

-- sampling from prior
-- ------------------

class Prior a where
  sampleProbs :: (PrimMonad m) => Hyper a -> Prob m (Probs a)

instance Conjugate p l => Prior (Conj p l) where
  sampleProbs = distSample $ priorSingleton @p @l

class GPrior i o where
  gsampleProbs :: forall m p. PrimMonad m => i p -> Prob m (o p)

instance GPrior V1 V1 where
  gsampleProbs = undefined

instance GPrior U1 U1 where
  gsampleProbs _ = pure U1

-- base case: k is a conjugate distribution
instance (Prior (Conj p l)) => GPrior (K1 i (HyperRep p l)) (K1 i (ProbsRep p l)) where
  gsampleProbs (K1 (HyperRep hyper)) =
    K1 . ProbsRep <$> sampleProbs @(Conj p l) hyper

-- recursive case: k is another record
instance (Prior k, k HyperRep ~ Hyper k, k ProbsRep ~ Probs k) =>
         GPrior (K1 i (k HyperRep)) (K1 i (k ProbsRep)) where
  gsampleProbs (K1 hyper) = K1 <$> sampleProbs @k hyper

instance (GPrior ti to) => GPrior (M1 i c ti) (M1 i' c' to) where
  gsampleProbs (M1 x) = M1 <$> gsampleProbs x

instance (GPrior ia oa, GPrior ib ob) => GPrior (ia :*: ib) (oa :*: ob) where
  gsampleProbs (a :*: b) = (:*:) <$> gsampleProbs a <*> gsampleProbs b

instance (GPrior ia oa, GPrior ib ob) => GPrior (ia :+: ib) (oa :+: ob) where
  gsampleProbs (L1 a) = L1 <$> gsampleProbs a
  gsampleProbs (R1 b) = R1 <$> gsampleProbs b

instance ( Generic (a HyperRep)
         , Generic (a ProbsRep)
         , GPrior (Rep (a HyperRep)) (Rep (a ProbsRep))
         ) => Prior (a :: (Type -> Type -> Type) -> Type) where
  sampleProbs hyper = GHC.Generics.to <$> gsampleProbs (from hyper)

-----------------------
-- likelihood monads --
-----------------------

type Accessor (r :: (Type -> Type -> Type) -> Type) p l
  = forall f . Lens' (r f) (f p l)

class Monad m => RandomInterpreter m r | m -> r where
  type SampleCtx m a :: Constraint
  sampleValue :: (Conjugate p l, SampleCtx m l) => l -> Accessor r p l -> m (Support l)
  sampleConst :: (Distribution a, SampleCtx m a) => a -> Params a -> m (Support a)

newtype Trace (r :: (Type -> Type -> Type) -> Type)  = Trace {runTrace :: S.Seq Dynamic}
  deriving (Show)

takeTrace :: Typeable a => Trace r -> Maybe (a, Trace r)
takeTrace (Trace t) = do
  (valDyn, rest) <- case S.viewl t of
    S.EmptyL         -> Nothing
    valDyn S.:< rest -> Just (valDyn, rest)
  val <- fromDynamic valDyn
  pure (val, Trace rest)

-- just sample
-- -----------

newtype SampleI m r a = SampleI (ReaderT (r ProbsRep) (Prob m) a)
  deriving (Functor, Applicative, Monad)

instance (PrimMonad m) => RandomInterpreter (SampleI m r) r where
  type SampleCtx (SampleI m r) a = ()
  sampleValue
    :: forall p l
     . (Conjugate p l)
    => l
    -> Accessor r p l
    -> SampleI m r (Support l)
  sampleValue lk getProbs = SampleI $ do
    probs <- ask
    lift $ distSample lk $ runProbs $ view getProbs probs
  sampleConst
    :: forall  a . (Distribution a) => a -> Params a -> SampleI m r (Support a)
  sampleConst dist params = SampleI $ lift $ distSample dist params

sampleResult :: p ProbsRep -> SampleI m p a -> Gen (PrimState m) -> m a
sampleResult probs (SampleI a) = sample (runReaderT a probs)

-- sample and trace the execution process
-- --------------------------------------

newtype TraceI m r a = TraceI (ReaderT (r ProbsRep) (StateT (Trace r) (Prob m)) a)
  deriving (Functor, Applicative, Monad)

instance (PrimMonad m) => RandomInterpreter (TraceI m r) r where
  type SampleCtx (TraceI m r) l = Typeable (Support l)
  sampleValue
    :: forall p l
     . (Conjugate p l, Typeable (Support l))
    => l
    -> Accessor r p l
    -> TraceI m r (Support l)
  sampleValue lk getProbs = TraceI $ do
    probs <- ask
    val   <- lift $ lift $ distSample lk $ runProbs $ view getProbs probs
    modify $ \(Trace obs) -> Trace $ obs S.|> toDyn val
    pure val
  sampleConst
    :: forall a
     . (Distribution a, Typeable (Support a))
    => a
    -> Params a
    -> TraceI m r (Support a)
  sampleConst dist params = TraceI $ do
    val <- lift $ lift $ distSample dist params
    modify $ \(Trace obs) -> Trace $ obs S.|> toDyn val
    pure val

sampleTrace :: r ProbsRep -> TraceI m r a -> Gen (PrimState m) -> m (a, Trace r)
sampleTrace probs (TraceI a) = do
  let st = runReaderT a probs
      pr = runStateT st (Trace mempty)
  sample pr

-- evaluate the probability of a trace
-- -----------------------------------

newtype EvalTraceI r a = EvalTraceI (ReaderT (r ProbsRep) (StateT (Trace r, Double) Maybe) a)
  deriving (Functor, Applicative, Monad)

instance RandomInterpreter (EvalTraceI r) r where
  type SampleCtx (EvalTraceI r) l = Typeable (Support l)
  sampleValue
    :: forall p l
     . (Conjugate p l, Typeable (Support l))
    => l
    -> Accessor r p l
    -> EvalTraceI r (Support l)
  sampleValue lk getProbs = EvalTraceI $ do
    probs              <- ask
    (trace, totalLogP) <- get
    (val  , trace'   ) <- lift $ lift $ takeTrace trace
    let logP = distLogP lk (runProbs $ view getProbs probs) val
    put (trace', totalLogP + logP)
    pure val
  sampleConst
    :: forall a
     . (Distribution a, Typeable (Support a))
    => a
    -> Params a
    -> EvalTraceI r (Support a)
  sampleConst dist params = EvalTraceI $ do
    (trace, totalLogP) <- get
    (val  , trace'   ) <- lift $ lift $ takeTrace trace
    let logP = distLogP dist params val
    put (trace', totalLogP + logP)
    pure val

evalTraceLogP :: r ProbsRep -> Trace r -> EvalTraceI r a -> Maybe (a, Double)
evalTraceLogP probs trace (EvalTraceI model) = do
  (val, (_trace, logp)) <- runStateT (runReaderT model probs) (trace, 0)
  pure (val, logp)

-- update priors
-- -------------

newtype UpdatePriorsI r a = UpdatePriorsI (StateT (Trace r, r HyperRep) Maybe a)
  deriving (Functor, Applicative, Monad)

instance RandomInterpreter (UpdatePriorsI r) r where
  type SampleCtx (UpdatePriorsI r) l = Typeable (Support l)
  sampleValue
    :: forall p l
     . (Conjugate p l, Typeable (Support l))
    => l
    -> Accessor r p l
    -> UpdatePriorsI r (Support l)
  sampleValue lk accessor = UpdatePriorsI $ do
    (trace, priors) <- get
    (val  , trace') <- lift $ takeTrace trace
    let priors' :: r HyperRep
        priors' = over
          accessor
          (\(HyperRep pr) -> HyperRep $ updatePrior @p @l lk pr val)
          priors
    put (trace', priors')
    pure val
  sampleConst _ _ = UpdatePriorsI $ do
    (trace, priors) <- get
    (val  , trace') <- lift $ takeTrace trace
    put (trace', priors)
    pure val

getPosterior :: r HyperRep -> Trace r -> UpdatePriorsI r a -> Maybe (r HyperRep)
getPosterior priors trace (UpdatePriorsI model) = do
  (_trace, posteriors) <- execStateT model (trace, priors)
  pure posteriors

-- show how a trace is generated by a model
-- ----------------------------------------

newtype ShowTraceI r a = ShowTraceI (WriterT String (StateT (Trace r) Maybe) a)
  deriving (Functor, Applicative, Monad)

showTraceItem
  :: forall l r
   . (Show (Support l), Typeable l, Typeable (Support l))
  => ShowTraceI r (Support l)
showTraceItem = ShowTraceI $ do
  trace         <- get
  (val, trace') <- lift $ lift $ takeTrace trace
  put trace'
  tell
    $  "Sampled value "
    <> show val
    <> " from a "
    <> show (typeRep (Proxy :: Proxy l))
    <> ".\n"
  pure val

instance RandomInterpreter (ShowTraceI r) r where
  type SampleCtx (ShowTraceI r) l
    = (Typeable (Support l), Typeable l, Show (Support l))
  sampleValue
    :: forall p l
     . (Conjugate p l, Typeable (Support l), Typeable l, Show (Support l))
    => l
    -> Accessor r p l
    -> ShowTraceI r (Support l)
  sampleValue _ _ = showTraceItem @l
  sampleConst
    :: forall a
     . (Distribution a, SampleCtx (ShowTraceI r) a)
    => a
    -> Params a
    -> ShowTraceI r (Support a)
  sampleConst _ _ = showTraceItem @a

showTrace :: Trace r -> ShowTraceI r a -> Maybe (a, String)
showTrace trace (ShowTraceI model) = evalStateT (runWriterT model) trace

printTrace :: Trace r -> ShowTraceI r a -> IO ()
printTrace trace model = do
  case showTrace trace model of
    Nothing -> do
      putStrLn "Trace does not match the model"
    Just (_, txt) -> do
      putStr txt

-------------------
-- distributions --
-------------------

-- Beta
-- ----

data Beta = Beta

instance Distribution Beta where
  type Params Beta = (Double, Double)
  type Support Beta = Double
  distSample _ = uncurry beta
  distLogP _ (_a, _b) _p = undefined -- TODO

instance Jeffrey (Conj Beta l) where
  jeffreyPrior = (0.5, 0.5)

-- Bernoulli
-- ---------

data Bernoulli = Bernoulli

instance Distribution Bernoulli where
  type Params Bernoulli = Double
  type Support Bernoulli = Bool
  distSample _ = bernoulli
  distLogP _ p True  = log p
  distLogP _ p False = log (1 - p)

-- Binomial
-- --------

newtype Binomial = Binomial Int

instance Distribution Binomial where
  type Params Binomial = Double
  type Support Binomial = Int
  distSample (Binomial n) = binomial n
  distLogP (Binomial n) p k =
    logChoose n k + (log p * k') + (log (1 - p) * (n' - k'))
   where
    k' = int2Double k
    n' = int2Double n

-- Categorical
-- -----------

data Categorical (n :: Nat) = Categorical

instance Distribution (Categorical n) where
  type Params (Categorical n) = V.Vector Double
  type Support (Categorical n) = Int
  distSample _ = categorical
  distLogP _ ps cat = log $ fromMaybe 0 $ ps V.!? cat

-- Dirichlet
-- ---------

data Dirichlet (n :: Nat) = Dirichlet

instance Distribution (Dirichlet n) where
  type Params (Dirichlet n) = V.Vector Double
  type Support (Dirichlet n) = V.Vector Double
  distSample _ = dirichlet
  distLogP _ _counts _cat = undefined -- TODO

instance KnownNat n => Jeffrey (Conj (Dirichlet n) l) where
  jeffreyPrior = V.replicate (fromIntegral $ natVal (Proxy :: Proxy n)) 0.5

-- Geometric (from 0)
-- ------------------

data Geometric0 = Geometric0

instance Distribution Geometric0 where
  type Params Geometric0 = Double
  type Support Geometric0 = Int
  distSample _ = geometric0
   where
    geometric0 p = do
      coin <- bernoulli p
      if coin then pure 0 else (1 +) <$> geometric0 p
  distLogP _ p val | val >= 0  = (log (1 - p) * int2Double val) + log p
                   | otherwise = log 0

-- Geometric (from 1)
-- ------------------

data Geometric1 = Geometric1

instance Distribution Geometric1 where
  type Params Geometric1 = Double
  type Support Geometric1 = Int
  distSample _ = geometric1
   where
    geometric1 p = do
      coin <- bernoulli p
      if coin then pure 1 else (1 +) <$> geometric1 p
  distLogP _ p val | val >= 1  = (log (1 - p) * int2Double (val - 1)) + log p
                   | otherwise = log 0

-----------------------------
-- conjugate distributions --
-----------------------------

-- beta bernoulli
-- --------------

instance Conjugate Beta Bernoulli where
  priorSingleton = Beta
  updatePrior _ (a, b) False = (a, b + 1)
  updatePrior _ (a, b) True  = (a + 1, b)

-- beta binomial
-- -------------

instance Conjugate Beta Binomial where
  priorSingleton = Beta
  updatePrior (Binomial n) (a, b) x = (a + x', b + (n' - x'))
   where
    x' = int2Double x
    n' = int2Double n

-- beta geometric0
-- ---------------

instance Conjugate Beta Geometric0 where
  priorSingleton = Beta
  updatePrior _ (a, b) k = (a + 1, b + int2Double k)

-- beta geometric1
-- ---------------

instance Conjugate Beta Geometric1 where
  priorSingleton = Beta
  updatePrior _ (a, b) k = (a + 1, b + int2Double (k - 1))

-- dirichlet categorical
-- ---------------------

instance Conjugate (Dirichlet n) (Categorical n) where
  priorSingleton = Dirichlet
  updatePrior _ counts obs
    | obs >= 0 && obs < V.length counts
    = counts V.// [(obs, (counts V.! obs) + 1)]
    | otherwise
    = counts

-------------
-- example --
-------------

-- | An example of a record that describes conjugate pairs of variables.
-- It can be instantiated with different type constructors for 'f' (kind @* -> * -> *@),
-- which allows it to represent hyperparameters, parameters, or values of the model.
data ExampleParams f =
  ExampleParams { _epP :: f Beta Bernoulli
                , _epCat1 :: f (Dirichlet 3) (Categorical 3)
                , _epCat2 :: f (Dirichlet 3) (Categorical 3)
                }
  deriving (Generic)

-- We need lenses to the fields of ExampleParams, which can be generated automatically.
makeLenses ''ExampleParams

-- The Show instance for ExampleParams needs to be standalone
-- because of the Show (f p l) constraints that GHC can't figure out on its own.
-- Here we specify it manually for nicer output.
instance ( Show (f Beta Bernoulli)
                  , Show (f (Dirichlet 3) (Categorical 3)))
                  => Show (ExampleParams f) where
  show (ExampleParams p cat1 cat2) =
    "ExampleParams"
      <> "\n  epP    = "
      <> show p
      <> "\n  epCat1 = "
      <> show cat1
      <> "\n  epCat2 = "
      <> show cat2

-- | The likelihood of the example model, described as a probabilistic program.
-- It uses lenses into the parameter record to refer to the parameters it draws samples from.
-- The distribution of each variable is thus determined by the type of the lens / record field.
-- The likelihood can be interpreted by different interpreters 'm' for inference.
exampleLk :: _ => RandomInterpreter m ExampleParams => m Int
exampleLk = do
  coin <- sampleValue Bernoulli epP
  sampleValue Categorical $ if coin then epCat1 else epCat2

-- | An example of how to work with a model.
exampleMain :: IO ()
exampleMain = do
  -- Define a prior distribution.
  -- Here, ExampleParams' Generic instance is used to get Jeffrey's prior for all its fields.
  let prior :: ExampleParams HyperRep
      prior = jeffreyPrior @ExampleParams
  putStrLn "prior:"
  print prior
  -- Initialize a mutable random state
  gen                             <- createSystemRandom
  -- Sample a set of probabilities from the prior.
  -- This is again done using the Generic instance of ExampleParams.
  probs :: ExampleParams ProbsRep <- sample (sampleProbs @ExampleParams prior)
                                            gen
  putStrLn "sampled probabilities:"
  print probs
  -- Sample a trace from the likelihood
  (result, trace) <- sampleTrace probs exampleLk gen
  -- Print the trace (which contains only the sampled values).
  -- The source of each value is obtained by running the trace through the model again.
  putStrLn "trace:"
  printTrace trace exampleLk
  putStrLn $ "result: " <> show result
  -- Evaluate the log probability of the trace.
  let logp = snd <$> evalTraceLogP probs trace exampleLk
  putStrLn $ "log p(trace) = " <> show logp
  -- Update the priors according to the sampled trace.
  -- Normally, this trace is obtained from observations in a dataset.
  let posteriorMb = getPosterior prior trace exampleLk
  putStrLn "posterior (using sampled trace):"
  case posteriorMb of
    Just posterior -> print posterior
    Nothing        -> putStrLn "failed to compute posterior"
  -- Construct traces manually (e.g. from observations)
  -- by storing the sequence of sampled values as they appear in the dataset.
  let tracesObs =
        [ Trace $ S.fromList [toDyn True, toDyn (0 :: Int)]
        , Trace $ S.fromList [toDyn False, toDyn (1 :: Int)]
        , Trace $ S.fromList [toDyn True, toDyn (0 :: Int)]
        , Trace $ S.fromList [toDyn False, toDyn (2 :: Int)]
        , Trace $ S.fromList [toDyn True, toDyn (0 :: Int)]
        , Trace $ S.fromList [toDyn False, toDyn (1 :: Int)]
        , Trace $ S.fromList [toDyn False, toDyn (2 :: Int)]
        , Trace $ S.fromList [toDyn False, toDyn (1 :: Int)]
        ]
      posteriorObsMb =
        foldM (\hyper obs -> getPosterior hyper obs exampleLk) prior tracesObs
  putStrLn "posterior (using observations):"
  case posteriorObsMb of
    Just posteriorObs -> print posteriorObs
    Nothing           -> putStrLn "failed to compute posterior"

-- exampleProbs :: ExampleParams ProbsRep
-- exampleProbs = ExampleParams { _epP    = ProbsRep 0.7
--                              , _epCat1 = ProbsRep $ V.fromList [0.3, 0.1, 0.6]
--                              , _epCat2 = ProbsRep $ V.fromList [0.1, 0.8, 0.1]
--                              }

-- examplePriors :: ExampleParams HyperRep
-- examplePriors = ExampleParams { _epP    = HyperRep (0.5, 0.5)
--                               , _epCat1 = HyperRep $ V.fromList [0.5, 0.5, 0.5]
--                               , _epCat2 = HyperRep $ V.fromList [0.5, 0.5, 0.5]
--                               }
