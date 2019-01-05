-- flatten a nested list structure

--We have to define a new data type, because lists in Haskell are homogeneous.



data NestedList a = Elem a | List [NestedList a] deriving Show

nestedLists =
  [ List []
  , List [Elem 1]
  , List [Elem 1, List [Elem 2], Elem 3]
  , List [List[List[Elem 1, Elem 2], Elem 3], Elem 4]
  , List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]
  ]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (n:ns)) = flatten n ++ flatten (List ns)

-- flatten' :: NestedList a -> [a]
-- flatten' (Elem x) = [x]
-- flatten' (List []) = []
-- flatten' (List xs)  = flatHead xs : flatten' $ flatTail xs

nestedHead :: NestedList a -> NestedList a
nestedHead (Elem _) = errorWithoutStackTrace "nestedHead works on List not Elem"
nestedHead (List []) = errorWithoutStackTrace "empty NestedList"
nestedHead (List (x:_)) = x

nestedTail :: NestedList a -> NestedList a
nestedTail (Elem _) = errorWithoutStackTrace "nestedTail works on List not Elem"
nestedTail (List (_:xs)) = List xs

nestedJoin :: NestedList a -> NestedList a -> NestedList a
nestedJoin (List l1) (List l2) = List (l1++l2)
nestedJoin (Elem _) _ = errorWithoutStackTrace "nestedJoin works on List not Elem"
nestedJoin _ (Elem _) = errorWithoutStackTrace "nestedJoin works on List not Elem"

flatHead :: NestedList a -> a
flatHead (Elem x) = x
flatHead (List []) = errorWithoutStackTrace "empty list"
flatHead (List (x:xs)) = flatHead x

flatTail :: NestedList a -> NestedList a
flatTail (Elem _) = errorWithoutStackTrace "flatTail works on List not Elem"
flatTail (List []) = List []
flatTail (List (Elem _:xs)) = List xs
flatTail (List (List []:xs)) = List xs
flatTail (List (List (Elem _:xs):ys)) = List [List xs, List ys]
-- flatTail (List [List [xs]]) = List [xs]
flatTail (List (List xs:ys)) = List [flatTail (List xs), List ys]
