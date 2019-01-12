module BinaryTree
  ( BinaryTree
  , exampleTree
  , treeSearch
  , treeFromList
  ) where

data BinaryTree a = Empty | Tree a (BinaryTree a) (BinaryTree a)
instance Show a => Show (BinaryTree a) where
  show = _show ""
    where _show indent Empty = indent ++ "Empty"
          _show indent (Tree e left right) = let next_indent = indent ++ "    " in
            _show next_indent left ++ "\n" ++ indent ++ show e ++ "\n" ++ _show next_indent right

instance Functor BinaryTree where
  fmap _ Empty = Empty
  fmap f (Tree e left right) = Tree (f e) (fmap f left) (fmap f right)

exampleTree = Tree
  5 (Tree 2 (Tree 1 Empty Empty) (Tree 3 Empty Empty)) (Tree 9 Empty Empty)

treeSearch :: (Ord a, Eq a) => a -> BinaryTree a -> Bool
treeSearch _ Empty = False
treeSearch x (Tree e left right)
  | x == e = True
  | x < e = treeSearch x left
  | otherwise = treeSearch x right

treeFromList :: (Ord a, Eq a) => [a] -> BinaryTree a
treeFromList xs = foldr treeAddElement Empty xs

treeAddElement :: (Ord a, Eq a) => a -> BinaryTree a -> BinaryTree a
treeAddElement x Empty = Tree x Empty Empty
treeAddElement x t@(Tree e left right)
  | x < e = Tree e (treeAddElement x left) right
  | x > e = Tree e left (treeAddElement x right)
  | otherwise = t

treeDepth :: (Integral b) => BinaryTree a -> b
treeDepth Empty = 0
treeDepth (Tree e left right) = 1 + max (treeDepth left) (treeDepth right)

treeWalk :: BinaryTree a -> [a]
treeWalk Empty = []
treeWalk (Tree e left right) = e : treeWalk left ++ treeWalk right

treeWalk' :: BinaryTree a -> [a]
treeWalk' Empty = []
treeWalk' (Tree e left right) = treeWalk right ++ treeWalk left ++ [e]

treeAddTree :: (Ord a) => BinaryTree a -> BinaryTree a -> BinaryTree a
treeAddTree Empty Empty = Empty
treeAddTree t Empty = t
treeAddTree Empty t = t
treeAddTree t1@(Tree e1 left1 right1) t2@(Tree e2 left2 right2)
  | treeDepth t1 >= treeDepth t2 = foldr treeAddElement t1 $ treeWalk' t2
  | otherwise = foldr treeAddElement t2 $ treeWalk' t1

treeRemoveElement :: (Ord a, Eq a) => a -> BinaryTree a -> BinaryTree a
treeRemoveElement _ Empty = Empty
treeRemoveElement x t@(Tree e left right)
  | x < e = Tree e (treeRemoveElement x left) right
  | x > e = Tree e left (treeRemoveElement x right)
  | otherwise = treeAddTree left right
