{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Internal.MultiSet where

import qualified Data.HashMap.Strict           as HM
import           GHC.Generics                   ( Generic )
import           Data.Hashable                  ( Hashable )
import           Control.DeepSeq                ( NFData )
import           Data.Foldable                  ( foldl'
                                                , Foldable(toList)
                                                )
import           Control.Monad                 as M

newtype MultiSet a = MS { unMS :: HM.HashMap a Int }
  deriving (Eq, Ord, Show, Generic, Hashable, NFData)

instance (Eq a, Hashable a) => Semigroup (MultiSet a) where
  (MS a) <> (MS b) = MS $ HM.unionWith (+) a b

instance Foldable MultiSet where
  toList (MS a) = concat $ HM.mapWithKey (flip replicate) a

toOccurList (MS m) = HM.toList m

distinctElems (MS m) = HM.keys m

union (MS a) (MS b) = MS $ HM.unionWith (+) a b

unions :: (Foldable t, Eq a0, Hashable a0) => t (MultiSet a0) -> MultiSet a0
unions = foldl' union empty

maxUnion (MS a) (MS b) = MS $ HM.unionWith max a b

deleteN :: Int -> Int -> Maybe Int
deleteN n m | m <= n    = Nothing
            | otherwise = Just (m - n)

(MS a) \\ (MS b) = MS $ HM.differenceWith (flip deleteN) a b

toSet (MS a) = HM.keysSet a

size (MS a) = foldl' (+) 0 a

map f (MS a) =
  MS $ HM.foldlWithKey' (\m k o -> HM.insertWith (+) (f k) o m) HM.empty a

filter f (MS a) = MS $ HM.filterWithKey (\k _ -> f k) a

fromList xs = MS $ foldl' (\m x -> HM.insertWith (+) x 1 m) HM.empty xs

fromSet xs = MS $ foldl' (\m x -> HM.insert x 1 m) HM.empty xs

null (MS a) = HM.null a

delete x = MS . HM.update (deleteN 1) x . unMS

empty = MS HM.empty

foldM :: Monad m => (b -> a -> m b) -> b -> MultiSet a -> m b
foldM f b (MS as) = HM.foldlWithKey' doFold (pure b) as
 where
  doFold mb a i = do
    b <- mb
    M.foldM f b (replicate i a)

insertMany a n (MS as) = MS $ HM.insertWith (+) a n as

insert a = insertMany a 1

singleton a = MS $ HM.singleton a 1

member a (MS as) = HM.lookupDefault 0 a as > 0
