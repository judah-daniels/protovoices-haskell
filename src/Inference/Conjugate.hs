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
module Inference.Conjugate where

import           Control.Monad.Primitive        ( PrimMonad )
import           Control.Monad.Reader           ( ReaderT
                                                , runReaderT
                                                )
import           Control.Monad.Reader.Class     ( MonadReader(ask) )
import           Control.Monad.State            ( StateT
                                                , runStateT
                                                , execStateT
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

type family Support a

type family Probs a

type family Hyper (a :: k) :: Type

class Distribution a where
  distSample :: (PrimMonad m) => a -> Prob m (Support a)
  distLogP :: a -> Support a -> Double

class Conjugate a where
  sampleConjValue :: (PrimMonad m) => Probs a -> Prob m (Support a)
  evalConjLogP :: Probs a -> Support a -> Double
  sampleConjParams :: (PrimMonad m) => Hyper a -> Prob m (Probs a)
  updatePrior :: Hyper a -> Support a -> Hyper a

newtype HyperRep a = HyperRep { runHyper :: Hyper a }
deriving instance Show (Hyper a) => Show (HyperRep a)

newtype ProbsRep a = ProbsRep { runProbs :: Probs a}
deriving instance Show (Probs a) => Show (ProbsRep a)

newtype ValueRep a = ValueRep { runValue :: Support a }
deriving instance Show (Support a) => Show (ValueRep a)

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

instance (Jeffrey k) => GJeffrey (K1 i (HyperRep k)) where
  gjeffreyPrior = K1 $ HyperRep $ jeffreyPrior @k

instance (Jeffrey k, k HyperRep ~ Hyper k) => GJeffrey (K1 i (k HyperRep)) where
  gjeffreyPrior = K1 $ jeffreyPrior @k

instance (GJeffrey t) => GJeffrey (M1 i c (t :: Type -> Type)) where
  gjeffreyPrior = M1 gjeffreyPrior

instance (GJeffrey ta, GJeffrey tb) => GJeffrey (ta :*: tb) where
  gjeffreyPrior = gjeffreyPrior @ta :*: gjeffreyPrior @tb

type instance Hyper (a :: (Type -> Type) -> Type) = a HyperRep

instance (Generic (a HyperRep), GJeffrey (Rep (a HyperRep))) => Jeffrey (a :: (Type -> Type) -> Type) where
  jeffreyPrior = GHC.Generics.to (gjeffreyPrior @(Rep (a HyperRep)))

-----------------------
-- likelihood monads --
-----------------------

type Accessor (p :: (Type -> Type) -> Type) a = forall f . Lens' (p f) (f a)

class Monad m => RandomInterpreter m p | m -> p where
  -- TODO: This constraint requires annotating the model with a constraint
  -- for every Conjugate that is used. Can this be derived from p using Generics?
  type SampleCtx m a :: Constraint
  sampleValue :: (Conjugate a, SampleCtx m a) => Accessor p a -> m (Support a)
  sampleConst :: (Distribution a, SampleCtx m a) => a -> m (Support a)

newtype Trace (p :: (Type -> Type) -> Type)  = Trace {runTrace :: S.Seq Dynamic}
  deriving (Show)

takeTrace :: Typeable a => Trace p -> Maybe (a, Trace p)
takeTrace (Trace t) = do
  (valDyn, rest) <- case S.viewl t of
    S.EmptyL         -> Nothing
    valDyn S.:< rest -> Just (valDyn, rest)
  val <- fromDynamic valDyn
  pure (val, Trace rest)

-- just sample
-- -----------

newtype SampleI m p a = SampleI (ReaderT (p ProbsRep) (Prob m) a)
  deriving (Functor, Applicative, Monad)

instance (PrimMonad m) => RandomInterpreter (SampleI m p) p where
  type SampleCtx (SampleI m p) a = ()
  sampleValue
    :: forall  a . (Conjugate a) => Accessor p a -> SampleI m p (Support a)
  sampleValue getProbs = SampleI $ do
    probs <- ask
    lift $ sampleConjValue @a $ runProbs $ view getProbs probs
  sampleConst dist = SampleI $ lift $ distSample dist

sampleResult :: p ProbsRep -> SampleI IO p a -> IO a
sampleResult probs (SampleI a) =
  createSystemRandom >>= sample (runReaderT a probs)

-- sample and trace the execution process
-- --------------------------------------

newtype TraceI m p a = TraceI (ReaderT (p ProbsRep) (StateT (Trace p) (Prob m)) a)
  deriving (Functor, Applicative, Monad)

instance (PrimMonad m) => RandomInterpreter (TraceI m p) p where
  type SampleCtx (TraceI m p) a = Typeable (Support a)
  sampleValue
    :: forall a
     . (Conjugate a, Typeable (Support a))
    => Accessor p a
    -> TraceI m p (Support a)
  sampleValue getProbs = TraceI $ do
    probs <- ask
    val   <- lift $ lift $ sampleConjValue @a $ runProbs $ view getProbs probs
    modify $ \(Trace obs) -> Trace $ obs S.|> toDyn val
    pure val
  sampleConst dist = TraceI $ do
    val <- lift $ lift $ distSample dist
    modify $ \(Trace obs) -> Trace $ obs S.|> toDyn val
    pure val

sampleTrace :: p ProbsRep -> TraceI IO p a -> IO (a, Trace p)
sampleTrace probs (TraceI a) = do
  gen <- createSystemRandom
  let st = runReaderT a probs
      pr = runStateT st (Trace mempty)
  sample pr gen

-- evaluate the probability of a trace
-- -----------------------------------

newtype EvalTraceI p a = EvalTraceI (ReaderT (p ProbsRep) (StateT (Trace p, Double) Maybe) a)
  deriving (Functor, Applicative, Monad)

instance RandomInterpreter (EvalTraceI p) p where
  type SampleCtx (EvalTraceI p) a = Typeable (Support a)
  sampleValue
    :: forall a
     . (Conjugate a, Typeable (Support a))
    => Accessor p a
    -> EvalTraceI p (Support a)
  sampleValue getProbs = EvalTraceI $ do
    probs              <- ask
    (trace, totalLogP) <- get
    (val  , trace'   ) <- lift $ lift $ takeTrace trace
    let logP = evalConjLogP @a (runProbs $ view getProbs probs) val
    put (trace', totalLogP + logP)
    pure val
  sampleConst dist = EvalTraceI $ do
    (trace, totalLogP) <- get
    (val  , trace'   ) <- lift $ lift $ takeTrace trace
    let logP = distLogP dist val
    put (trace', totalLogP + logP)
    pure val

evalTraceLogP :: p ProbsRep -> Trace p -> EvalTraceI p a -> Maybe (a, Double)
evalTraceLogP probs trace (EvalTraceI model) = do
  (val, (_trace, logp)) <- runStateT (runReaderT model probs) (trace, 0)
  pure (val, logp)

-- update priors
-- -------------

newtype UpdatePriorsI p a = UpdatePriorsI (StateT (Trace p, p HyperRep) Maybe a)
  deriving (Functor, Applicative, Monad)

instance RandomInterpreter (UpdatePriorsI p) p where
  type SampleCtx (UpdatePriorsI p) a = Typeable (Support a)
  sampleValue
    :: forall a
     . (Conjugate a, Typeable (Support a))
    => Accessor p a
    -> UpdatePriorsI p (Support a)
  sampleValue accessor = UpdatePriorsI $ do
    (trace, priors) <- get
    (val  , trace') <- lift $ takeTrace trace
    let priors' :: p HyperRep
        priors' = over accessor
                       (\(HyperRep pr) -> HyperRep $ updatePrior @a pr val)
                       priors
    put (trace', priors')
    pure val
  sampleConst _ = UpdatePriorsI $ do
    (trace, priors) <- get
    (val  , trace') <- lift $ takeTrace trace
    put (trace', priors)
    pure val

getPosterior :: p HyperRep -> Trace p -> UpdatePriorsI p a -> Maybe (p HyperRep)
getPosterior priors trace (UpdatePriorsI model) = do
  (_trace, posteriors) <- execStateT model (trace, priors)
  pure posteriors

-- show how a trace is generated by a model
-- ----------------------------------------

newtype ShowTraceI p a = ShowTraceI (WriterT String (StateT (Trace p) Maybe) a)
  deriving (Functor, Applicative, Monad)

showTraceItem
  :: forall a p
   . (Show (Support a), Typeable a, Typeable (Support a))
  => ShowTraceI p (Support a)
showTraceItem = ShowTraceI $ do
  trace         <- get
  (val, trace') <- lift $ lift $ takeTrace trace
  put trace'
  tell
    $  "Sampled value "
    <> show val
    <> " from a "
    <> show (typeRep (Proxy :: Proxy a))
    <> ".\n"
  pure val

instance RandomInterpreter (ShowTraceI p) p where
  type SampleCtx (ShowTraceI p) a
    = (Typeable (Support a), Typeable a, Show (Support a))
  sampleValue
    :: forall a
     . (Conjugate a, Typeable (Support a), Typeable a, Show (Support a))
    => Accessor p a
    -> ShowTraceI p (Support a)
  sampleValue _ = showTraceItem @a
  sampleConst
    :: forall a
     . (Distribution a, SampleCtx (ShowTraceI p) a)
    => a
    -> ShowTraceI p (Support a)
  sampleConst _ = showTraceItem @a

showTrace :: Trace p -> ShowTraceI p a -> IO (Maybe a)
showTrace trace (ShowTraceI model) = do
  case runStateT (runWriterT model) trace of
    Nothing -> do
      putStrLn "Trace does not match the model"
      pure Nothing
    Just ((val, txt), _trace') -> do
      putStr txt
      pure $ Just val

--------------------------
-- simple distributions --
--------------------------

newtype Bernoulli = Bernoulli Double

type instance Support Bernoulli = Bool

instance Distribution Bernoulli where
  distSample (Bernoulli p) = bernoulli p
  distLogP (Bernoulli p) True  = log p
  distLogP (Bernoulli p) False = log (1 - p)

-----------------------------
-- conjugate distributions --
-----------------------------

data BetaBernoulli

type instance Support BetaBernoulli = Bool
type instance Probs BetaBernoulli = Double
type instance Hyper BetaBernoulli = (Double, Double)

instance Conjugate BetaBernoulli where
  sampleConjValue = bernoulli
  evalConjLogP prob True  = log prob
  evalConjLogP prob False = log (1 - prob)
  sampleConjParams = uncurry beta
  updatePrior (a, b) True  = (a + 1, b)
  updatePrior (a, b) False = (a, b + 1)

instance Jeffrey BetaBernoulli where
  jeffreyPrior = (0.5, 0.5)

data BetaGeometric0

type instance Support BetaGeometric0 = Int
type instance Probs BetaGeometric0 = Double
type instance Hyper BetaGeometric0 = (Double, Double)

instance Conjugate BetaGeometric0 where
  sampleConjValue = geometric0
   where
    geometric0 p = do
      coin <- bernoulli p
      if coin then pure 0 else (1 +) <$> geometric0 p
  evalConjLogP p val | val >= 0  = (log (1 - p) * fromIntegral val) + log p
                     | otherwise = log 0
  sampleConjParams = uncurry beta
  updatePrior (a, b) k = (a + 1, b + fromIntegral k)

instance Jeffrey BetaGeometric0 where
  jeffreyPrior = (0.5, 0.5)

data BetaGeometric1

type instance Support BetaGeometric1 = Int
type instance Probs BetaGeometric1 = Double
type instance Hyper BetaGeometric1 = (Double, Double)

instance Conjugate BetaGeometric1 where
  sampleConjValue = geometric1
   where
    geometric1 p = do
      coin <- bernoulli p
      if coin then pure 1 else (1 +) <$> geometric1 p
  evalConjLogP p val
    | val >= 1  = (log (1 - p) * fromIntegral (val - 1)) + log p
    | otherwise = log 0
  sampleConjParams = uncurry beta
  updatePrior (a, b) k = (a + 1, b + fromIntegral (k - 1))

instance Jeffrey BetaGeometric1 where
  jeffreyPrior = (0.5, 0.5)

data DirichletCategorical (n :: Nat)

type instance Support (DirichletCategorical n) = Int
type instance Probs (DirichletCategorical n) = V.Vector Double
type instance Hyper (DirichletCategorical n) = V.Vector Double

instance Conjugate (DirichletCategorical n) where
  sampleConjValue = categorical
  evalConjLogP probs cat = log $ fromMaybe 0 $ probs V.!? cat
  sampleConjParams = dirichlet
  updatePrior counts obs
    | obs >= 0 && obs < V.length counts
    = counts V.// [(obs, (counts V.! obs) + 1)]
    | otherwise
    = counts

instance KnownNat n => Jeffrey (DirichletCategorical n) where
  jeffreyPrior = V.replicate (fromIntegral $ natVal (Proxy :: Proxy n)) 0.5

-------------
-- example --
-------------

data ExampleParams f =
  ExampleParams { _epP :: f BetaBernoulli
                , _epCat1 :: f (DirichletCategorical 3)
                , _epCat2 :: f (DirichletCategorical 3)
                }
  deriving (Generic)

deriving instance (Show (f BetaBernoulli), Show (f (DirichletCategorical 3))) => Show (ExampleParams f)

-- instance Jeffrey (ExampleParams f) (ExampleParams HyperRep)

exampleProbs :: ExampleParams ProbsRep
exampleProbs = ExampleParams { _epP    = ProbsRep 0.7
                             , _epCat1 = ProbsRep $ V.fromList [0.3, 0.1, 0.6]
                             , _epCat2 = ProbsRep $ V.fromList [0.1, 0.8, 0.1]
                             }

examplePriors :: ExampleParams HyperRep
examplePriors = ExampleParams { _epP    = HyperRep (0.5, 0.5)
                              , _epCat1 = HyperRep $ V.fromList [0.5, 0.5, 0.5]
                              , _epCat2 = HyperRep $ V.fromList [0.5, 0.5, 0.5]
                              }

makeLenses ''ExampleParams

exampleLk
  :: (SampleCtx m BetaBernoulli, SampleCtx m (DirichletCategorical 3))
  => RandomInterpreter m ExampleParams => m Int
exampleLk = do
  coin <- sampleValue epP
  sampleValue $ if coin then epCat1 else epCat2

newtype ExParams2 f = ExParams2 { ep :: f BetaBernoulli }
  deriving (Generic)

deriving instance (Show (f BetaBernoulli)) => Show (ExParams2 f)
