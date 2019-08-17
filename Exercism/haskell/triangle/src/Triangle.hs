module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
  | not isTriangle = Illegal
  | a == b         = perhapsEquilateral
  | b == c         = Isosceles
  | a == c         = Isosceles
  | otherwise      = Scalene
  where perhapsEquilateral
          | b == c = Equilateral
          | otherwise = Isosceles
        isTriangle
          | any (<= 0) [a,b,c] = False
          | a + b < c          = False
          | a + c < b          = False
          | c + b < a          = False
          | otherwise          = True

