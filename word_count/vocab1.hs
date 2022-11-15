import Data.Char
-- imports only specified elements
import Data.List (group, sort)
-- imports as qualified (namespace has to be specified before each use)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
-- module for reading command line arguments
import System.Environment

main = do
 [fname] <- getArgs -- reads command line arguments into a list of strings
 text <- TIO.readFile fname
 let filteredWords = filter (not . T.null) $ map (T.dropAround $ not . isLetter) $ T.words text
 let ws = map head $ group $ sort $ map T.toCaseFold $ filteredWords
 -- Print all distinct words separated with a whitespace 
 -- TIO.putStrLn $ T.unwords ws
 print $ length ws 
