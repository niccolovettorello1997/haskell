import Data.Char
import Data.List (group, sort, sortBy)
import Data.Ord
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (when)
import System.Environment

-- define types
type Entry = (T.Text, Int)
type Vocabulary = [Entry]

extractVocabulary :: T.Text -> Vocabulary
extractVocabulary t = map buildEntry $ group $ sort ws
 where
  ws = map T.toCaseFold $ filter (not . T.null) $ map cleanWord $ T.words t
  buildEntry xs@(x:_) = (x, length x)
  buildEntry [] = error "unexpected empty list"
  cleanWord = T.dropAround (not . isLetter)

-- get all distinct words from the vocabulary
allWords :: Vocabulary -> [T.Text]
allWords vocabulary = map fst vocabulary

-- tuple:
-- 1: total number of words in text
-- 2: number of unique words
wordsCount :: Vocabulary -> (Int, Int)
wordsCount vocabulary = (sum $ map snd vocabulary, length vocabulary)

-- sort the vocabulary by comparing frequencies by reversed order
wordsByFrequency :: Vocabulary -> Vocabulary
wordsByFrequency = sortBy (comparing $ Down . snd)

-- get a report on words count
wordsCountReport :: Vocabulary -> T.Text
wordsCountReport vocabulary = T.unlines [part1, part2]
 where
  (total, unique) = wordsCount vocabulary
  part1 = T.append (T.pack "Total number of words: ") (T.pack $ show total)
  part2 = T.append (T.pack "Unique number of words: ") (T.pack $ show unique) 
