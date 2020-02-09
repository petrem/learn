module FoldlAsFoldr where

newtype Update a = Update {evalUpdate :: a -> a}


instance Semigroup (Update a) where
  (Update x) <> (Update y) = Update (y.x)

instance Monoid (Update a) where
   mempty = Update id

foldlMonoid :: (a -> b -> a) -> a -> [b] -> a
foldlMonoid f a bs =
   flip evalUpdate a $
   mconcat $
   map (Update . flip f) bs

-- instance Functor Update where
--   fmap f (Update g) = Update (f.g)


foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f i xs = foldl (flip f) i (reverse xs)
