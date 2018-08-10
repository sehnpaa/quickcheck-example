{-# LANGUAGE ScopedTypeVariables #-}

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
addBeforeLast2 x (a:as) = a : addBeforeLast2 x as

prop :: Eq a => a -> [a] -> Bool
prop x xs = addBeforeLast x xs == addBeforeLast2 x xs

runIntTest :: IO ()
runIntTest = quickCheck (prop :: Int -> [Int] -> Bool)

runBoolTest :: IO ()
runBoolTest = quickCheck (prop :: Bool -> [Bool] -> Bool)

-------------------------
-- Custom product type --
-------------------------

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

---------------------
-- Custom sum type --
---------------------

data Tree a = Leaf a | Branch (Tree a) a (Tree a) deriving Show

sumProp :: Eq a => Tree a -> Bool
sumProp = undefined

generateTrees :: IO ()
generateTrees = sample (arbitrary :: Gen (Tree Int))

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = do
    n <- choose (1,2) :: Gen Int
    case n of
      1 -> Leaf <$> arbitrary
      2 -> do
        left :: Tree a <- arbitrary
        a <- arbitrary
        right :: Tree a <- arbitrary
        return $ Branch left a right

runSumTest :: IO ()
runSumTest = quickCheck (sumProp :: Tree Int -> Bool)