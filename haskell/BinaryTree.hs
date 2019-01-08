module BinaryTree
  ( BinaryTree
  , exampleTree
--  , show
  , treeSearch
  , treeFromList
  ) where

data BinaryTree a = Empty | Tree a (BinaryTree a) (BinaryTree a)
instance Show a => Show (BinaryTree a) where
  show = _show ""
    where _show indent Empty = indent ++ "Empty"
          _show indent (Tree e left right) = let next_indent = indent ++ "    " in
            _show next_indent left ++ "\n" ++ indent ++ show e ++ "\n" ++ _show next_indent right

exampleTree = Tree
  5 (Tree 2 (Tree 1 Empty Empty) (Tree 3 Empty Empty)) (Tree 9 Empty Empty)

treeSearch :: (Ord a, Eq a) => a -> BinaryTree a -> Bool
treeSearch _ Empty = False
treeSearch x (Tree e left right)
  | x == e = True
  | x < e = treeSearch x left
  | otherwise = treeSearch x right

treeFromList :: (Ord a, Eq a) => [a] -> BinaryTree a
treeFromList = _treeFromList Empty
  where _treeFromList t [] = t
        _treeFromList Empty (x:xs) = _treeFromList (Tree x Empty Empty) xs
        _treeFromList t@(Tree e left right) xs@(x:_)
          | e < x = Tree e left (_treeFromList right xs)
          | e > x = Tree e (_treeFromList left xs) right
          | otherwise = _treeFromList t xs
