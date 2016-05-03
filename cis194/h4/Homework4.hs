module Homework4 where

    -- Exercise 1: Wholemeal programming
    fun1 :: [Integer] -> Integer
    fun1 [] = 1
    fun1 (x:xs)
        | even x = (x - 2) * fun1 xs
        | otherwise = fun1 xs

    fun2 :: Integer -> Integer
    fun2 1 = 0
    fun2 n 
        | even n = n + fun2 (n `div` 2)
        | otherwise = fun2 (3 * n + 1)


    fun1' :: [Integer] -> Integer
    fun1' = product . map (\x -> x-2) . filter even
    


    -- Exercise 2: Folding with trees
    data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

    depth :: Tree a -> Integer
    depth Leaf = 0
    depth (Node d Leaf _ Leaf) = d
    depth (Node d lBranch _ rBranch) = (toInteger . maximum . map fromIntegral) [(depth lBranch), (depth rBranch)]

    insert :: Tree a -> a -> Tree a
    insert tree val = insert' tree 0 val 
        where
            insert' :: Tree a -> Integer -> a -> Tree a
            insert' Leaf d val = Node d Leaf val Leaf
            insert' (Node d lBranch v rBranch) curD val
                | (depth lBranch) > (depth rBranch) = Node d lBranch v (insert' rBranch (curD+1) val)
                | otherwise = Node d (insert' lBranch (d+1) val) v rBranch

    hyperFold :: [a] -> Tree a
    hyperFold = foldr (flip insert) Leaf

    

    -- Exercise 3: More folds!
    xor :: [Bool] -> Bool
    xor = foldl (/=) False

    map' :: (a -> b) -> [a] -> [b]
    map' f = foldr (\x xs -> (f x):xs) []
