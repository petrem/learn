module BinaryTree
  ( BinaryTree
  , exampleTree
  , searchTree
  ) where

data BinaryTree a = Empty | Tree (a, BinaryTree a, BinaryTree a)

exampleTree = Tree
  ( 5
  , Tree ( 2
         , Tree ( 1
                , Empty
                , Empty)
         , Tree ( 3
                , Empty
                , Empty))
  , Tree ( 9
         , Empty
         , Empty))

searchTree :: (Ord a) => a -> BinaryTree a -> Bool
searchTree _ Empty = False
searchTree x (Tree (e, left, right))
  | x == e = True
  | x < e = searchTree x left
  | otherwise = searchTree x right

