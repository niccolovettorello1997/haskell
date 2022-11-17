import Data.Char
import Data.List (sort, group)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment

-- an entry is a tuple containing a word and the number of its occurences
type Entry = (T.Text, Int)
-- a vocabulary is a list of entries
type Vocabulary = [Entry]

-- defines the function type
extractVocab :: T.Text -> Vocabulary
extractVocab t = map buildEntry $ group $ sort ws
 where -- here goes the definition of local variables
  -- define first most external local variables and then proceed to internal ones
  ws = map T.toCaseFold $ filter (not . T.null) $ map cleanWord $ T.words t
  -- local functions can be defined by pattern matching in the where clause
  -- xs is the reference to the whole list
  -- (x, _) is the list pattern matching: in this case the rest of the list is useless so we use the '_' keyword
  buildEntry xs@(x:_) = (x, length xs)
  buildEntry [] = error "Empty list is unsupported"
  cleanWord = T.dropAround (not . isLetter)


printAllWords :: Vocabulary -> IO ()
printAllWords vocab = do
 -- concatenate the words with its count into a single text
 TIO.putStrLn $ T.unlines $ map (\c -> T.append (fst c) (T.pack $ (": " ++ show (snd c)))) vocab

processTextFile :: FilePath -> IO ()
processTextFile fname = do
 text <- TIO.readFile fname
 let vocab = extractVocab text
 printAllWords vocab

main :: IO ()
main = do
 args <- getArgs
 case args of -- pattern matching
  [fname] -> processTextFile fname
  _ -> putStrLn "Usage: vocab2 filename"
