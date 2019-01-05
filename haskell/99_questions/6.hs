-- Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).

--obvious solution
isPalindrome::(Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

--half obvious solution
isPalindrome'::(Eq a) => [a] -> Bool
isPalindrome' xs = let half=length xs `div` 2 in
  (take half xs) == (take half $ reverse xs)

--isPalindrome'''::(Eq a) => [a] -> Bool
--TODO: try to do with a function composition and possible point-free style,
--e.g. somehow map comopse (==) on [id, reverse] ?

-- interesting solutions, COPIED, to understand once i get there...:
isPalindrome''' :: (Eq a) => [a] -> Bool
isPalindrome''' = Control.Monad.liftM2 (==) id reverse

isPalindrome'''' :: (Eq a) => [a] -> Bool
isPalindrome'''' = (==) Control.Applicative.<*> reverse

--Here's one that does half as many compares:

palindrome :: (Eq a) => [a] -> Bool
palindrome xs = p [] xs xs
   where p rev (x:xs) (_:_:ys) = p (x:rev) xs ys
         p rev (x:xs) [_] = rev == xs
         p rev xs [] = rev == xs

