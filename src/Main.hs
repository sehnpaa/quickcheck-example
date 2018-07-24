module Main where

import Test.QuickCheck

main :: IO ()
main = putStrLn "Hello, Haskell!"

---------------------------
-- Compare two functions --
---------------------------

addBeforeLast :: a -> [a] -> [a]
addBeforeLast _ [] = []
addBeforeLast x ys = init ys ++ [x] ++ [last ys]

addBeforeLast2 :: a -> [a] -> [a]
addBeforeLast2 _ [] = []
addBeforeLast2 x [a] = [x,a]
addBeforeLast2 x (a:as) = a: addBeforeLast2 x as

prop :: Eq a => a -> [a] -> Bool
prop x xs = addBeforeLast x xs == addBeforeLast2 x xs

runIntTest :: IO ()
runIntTest = quickCheck (prop :: Int -> [Int] -> Bool)

runBoolTest :: IO ()
runBoolTest = quickCheck (prop :: Bool -> [Bool] -> Bool)

----------------------
-- Custom data type --
----------------------

data Person = Person
  { name :: String
  , age :: Int }
  deriving (Eq, Show)

genPerson :: Gen Person
genPerson = do
  name <- listOf genAToZ
  age <- arbitrary
  return $ Person name age

genAToZ :: Gen Char
genAToZ = elements ['a'..'z']

generatePeople :: IO [Person]
generatePeople = sample' genPerson

buggyId :: Person -> Person
buggyId p@(Person name age) = if length name == age then Person "" 0 else p

runIdTest :: IO ()
runIdTest = quickCheck $ forAll genPerson (\p -> buggyId p == p)