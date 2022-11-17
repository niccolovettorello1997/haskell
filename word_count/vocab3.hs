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

-- get all words from the vocabulary
allWords :: Vocabulary -> [T.Text]
allWords vocabulary = map fst vocabulary

-- pair:
-- 1: total number of words in text
-- 2: number of unique words
wordsCount :: Vocabulary -> (Int, Int)
wordsCount vocabulary = (sum $ map snd vocabulary, length vocabulary)

-- sort the vocabulary by 
