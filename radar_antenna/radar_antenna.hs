-- ++++++++++++++++++++++++++++++++++++++++++++++++ IMPORTS +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

import Control.Monad (when)
import Data.List (sort, nub)
import System.Environment (getArgs)
import System.Exit
import System.Random

-- +++++++++++++++++++++++++++++++++++++++++++++++ DATA TYPES +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- automatic derivation rules:
-- Eq: two values are equal iff they are built from the same constructor, otherwise they are not equal
-- Enum: the enumeration follows the order in which constructors are declared
-- Bounded: follows the declaration order of constructors, the first being the minimum value and the last being the maximum value
-- Show: print constructors literally

-- TNone means the void turn
-- TAround means a 180 degree turn 
data Turn = TNone | TLeft | TRight | TAround
 deriving (Bounded, Enum, Eq, Ord, Read, Show)

--		| North
--		|
--		|
--		|
--   West	|        East
--   ------------------------
--    		|
--    		|
--    		|
--    		|
--    		| South

data Direction = North | East | South | West
 deriving (Bounded, Enum, Eq, Read, Show)

-- define a type class that represents types whose values can be enumerated in a cycle
-- type constraint: to be a CyclicEnum a class must be an instance of Eq, Bounded and Enum
class (Eq a, Enum a, Bounded a) => CyclicEnum a where
 cpred :: a -> a
 cpred d
  | d == minBound = maxBound
  | otherwise = pred d

 csucc :: a -> a
 csucc d
  | d == maxBound = minBound
  | otherwise = succ d

-- declare that a Direction is an instance of CyclicEnum
instance CyclicEnum Direction

-- make Turn an instance of Semigroup, therefore defining its concatenation operator
-- note: the use of commutativity in the last definition
instance Semigroup Turn where
 TNone <> t = t
 TLeft <> TLeft = TAround
 TLeft <> TRight = TNone
 TLeft <> TAround = TRight
 TRight <> TRight = TAround
 TRight <> TAround = TLeft
 TAround <> TAround = TNone
 t1 <> t2 = t2 <> t1

-- make Turn an instance of Monoid by defining the neutral element
instance Monoid Turn where
 mempty = TNone

instance Random Turn where
 randomR (low, high) rng = (toEnum result, rng')
  where
   (result, rng') = randomR (fromEnum low, fromEnum high) rng

 random rng = randomR (minBound, maxBound) rng

instance Random Direction where
 randomR (low, high) rng = (toEnum result, rng')
  where
   (result, rng') = randomR (fromEnum low, fromEnum high) rng

 random rng = randomR (minBound, maxBound) rng

-- +++++++++++++++++++++++++++++++++++++++++++++ FUNCTIONS ++++++++++++++++++++++++++++++++++++++++++++++

-- given a Turn and a starting Direction return the resulting Direction
rotate :: Turn -> Direction -> Direction
rotate TNone = id
rotate TLeft = cpred
rotate TRight = csucc
rotate TAround = cpred . cpred

-- return the complete list of data constructors, provided that the type implements both Enum and Bounded
every :: (Enum a, Bounded a) => [a]
every = enumFrom minBound

-- given two direction, return the list of turns needed to go from the first direction to the second one
-- note: it's always a single element
orient :: Direction -> Direction -> Turn
orient d1 d2 = head $ filter (\t -> rotate t d1 == d2) every

-- apply rotate on a list of turns
rotateMany :: Direction -> [Turn] -> Direction
rotateMany = foldl (flip rotate)

-- same as rotateMany, but returns all the intermediate directions reached while applying turns
rotateManySteps :: Direction -> [Turn] -> [Direction]
rotateManySteps = scanl (flip rotate)

-- same as orient but with a list of directions
-- list of directions must have at least two elements
orientMany :: [Direction] -> [Turn]
orientMany ds@(_:_:_) = zipWith orient ds (tail ds)
orientMany _ = []

-- thanks to Semigroup and Monoid we can combine immediately a list of turns
-- therefore rotateMany can be redefined as
rotateMany' :: Direction -> [Turn] -> Direction
rotateMany' direction turns = rotate (mconcat turns) direction

-- take a Direction, the FilePath of a file containing a list of turns and print the final Direction reached by applying all the turns
rotateFromFile :: Direction -> FilePath -> IO ()
rotateFromFile direction fname = do
 f <- readFile fname
 let turns = map read $ lines f
 putStrLn $ "Final direction: " ++ (show $ rotateMany direction turns)
 putStrLn $ "Intermediate directions: " ++ (show $ rotateManySteps direction turns)

-- take the FilePath of a file containing a list of directions and apply orientMany to the list
orientFromFile :: FilePath -> IO ()
orientFromFile fname = do
 f <- readFile fname
 putStrLn $ "List of turns: " ++ (show $ orientMany (map read $ lines f))

-- given a random number generator, get a list of n random turns
randomTurns :: Int -> StdGen -> [Turn]
randomTurns 0 _ = []
randomTurns n rng = [result] ++ randomTurns (n-1) rng'
 where
  (result, rng') = random rng

-- given a random number generator, get a list of n random directions
randomDirections :: Int -> StdGen -> [Direction]
randomDirections 0 _ = []
randomDirections n rng = [result] ++ randomDirections (n-1) rng'
 where
  (result, rng') = random rng

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ TESTS +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- Applying orient on every possible pair of Directions returns all possible Turns
test_allTurnsInUse :: Bool
test_allTurnsInUse = sort (nub [orient d1 d2 | d1 <- every, d2 <- every]) == every

-- The two implementations of rotateMany are equivalent
test_rotateManyAreEquivalent :: [Turn] -> Bool
test_rotateManyAreEquivalent turns = and [rotateMany d turns == rotateMany' d turns | d <- every]

-- orientMany is the inverse function of rotateManySteps
test_inverse :: [Direction] -> Bool
test_inverse [] = True
test_inverse ds@(d:_) = ds == rotateManySteps d (orientMany ds)  

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ MAIN ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- usage:
-- -r filename direction -> execute all turns saved in filename starting from provided direction
-- -o filename -> orient the antenna toward the provided directions and get the list of resulting turns
main :: IO ()
main = do
 when (not $ and [test_allTurnsInUse, test_rotateManyAreEquivalent (randomTurns 100 (mkStdGen 17)), test_inverse (randomDirections 100 (mkStdGen 40))]) exitFailure
