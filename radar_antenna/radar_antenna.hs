-- define user data types

-- automatic derivation rules:
-- Eq: two values are equal iff they are built from the same constructor, otherwise they are not equal
-- Enum: the enumeration follows the order in which constructors are declared
-- Bounded: follows the declaration order of constructors, the first being the minimum value and the last being the maximum value
-- Show: print constructors literally

-- TNone means the void turn
-- TAround means a 180 degree turn 
data Turn = TNone | TLeft | TRight | TAround
 deriving (Eq, Enum, Bounded, Show)

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
 deriving (Eq, Enum, Bounded, Show)

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
