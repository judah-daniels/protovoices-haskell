module ScoresCommon where

import           Common
import           ScoringFunTyped
import qualified Data.Semiring                 as R
import           Data.Maybe                     ( listToMaybe )

-- creating scores from derivations
-- --------------------------------

newtype FreezeScore s = FreezeScore s
  deriving (Eq, Ord, Show)

newtype SplitScore s = SplitScore s
  deriving (Eq, Ord, Show)

newtype HoriScore s = HoriScore s
  deriving (Eq, Ord, Show)

type LeftmostScore s = Leftmost (SplitScore s) (FreezeScore s) (HoriScore s)

leftmostScores
  :: (R.Semiring s, Show s) => [LeftmostScore s] -> Maybe [Score s ()]
leftmostScores = foldr applyOp (Just [])
 where
  applyOp (LMFreezeLeft (FreezeScore s)) (Just stack) = Just $ val s : stack
  applyOp (LMFreezeOnly f) stack = applyOp (LMFreezeLeft f) stack
  applyOp (LMSplitLeft (SplitScore s)) (Just (l : r : stack)) =
    Just $ mergeScores s l r : stack
  applyOp (LMSplitOnly s) stack = applyOp (LMSplitLeft s) stack
  applyOp (LMSplitRight (SplitScore s)) (Just (x : l : r : stack)) =
    Just $ x : mergeScores s l r : stack
  applyOp (LMHorizontalize (HoriScore s)) (Just (l : m : r : stack)) =
    Just $ vertScoresLeft () l : vertScoresRight () s m r : stack
  applyOp _ _ = Nothing

leftmostScore :: (R.Semiring s, Show s) => [LeftmostScore s] -> Maybe s
leftmostScore ops = do
  top <- listToMaybe =<< leftmostScores ops
  pure $ getScoreVal top
