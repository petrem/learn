import qualified Text.Read


-- first a version with less mucking about with types

readDouble :: String -> Maybe Double
readDouble = Text.Read.readMaybe

rpn' :: String -> [Double]
rpn' = foldl _rpn [] . words
  where
    _rpn stack token
      | token == "*" = result (*):(drop 2 stack)
      | token == "/" = result (/):(drop 2 stack)
      | token == "+" = result (+):(drop 2 stack)
      | token == "-" = result (-):(drop 2 stack)
      | readDouble token /= Nothing = let number = (\(Just n) -> n) in number (readDouble token):stack
      | otherwise = errorWithoutStackTrace ("unknown token " ++ token)
      where
        result op = uncurry op $ (\[x,y] -> (x, y)) $ take 2 stack


-- now let's muck about with types

data Operator = OpAdd | OpSub | OpMul | OpDiv | OpAbs deriving (Enum, Bounded, Ord, Eq)
instance Show Operator where
  show OpAdd = "+"
  show OpSub = "-"
  show OpMul = "*"
  show OpDiv = "/"
  show OpAbs = "abs"

getArity :: Operator -> Int
getArity OpAdd = 2
getArity OpSub = 2
getArity OpMul = 2
getArity OpDiv = 2
getArity OpAbs = 1

readOperator :: String -> (Either String Operator)
readOperator s
  | s == "+"   = Right OpAdd
  | s == "-"   = Right OpSub
  | s == "*"   = Right OpMul
  | s == "/"   = Right OpDiv
  | s == "abs" = Right OpAbs
  | otherwise  = Left s

readToken :: String -> ???
readToken t = either (\a -> c) (\b -> c) $ readOperator t

foo :: String -> Maybe (Either Operator Double)
foo t = maybe Nothing (\(Just num) -> Just $ Right num) (readDouble t)


-- rpn :: String -> [Double]
-- rpn = foldl (\acc x -> _rpn acc (maybeReadToken x)) [] . words
--   where
--     _rpn stack Just op
--       | op `elem` [OpAdd..OpDiv] = applyOperator op stack:(drop (getArity op) stack)
--     _rpn stack (Just (N n)) = n:stack
--     _rpn _ t@Nothing = errorWithoutStackTrace ("unknown token ") -- ++ t)

-- applyOperator :: Token a -> [a] -> [a]
-- applyOperator op stack = uncurry op $ (\[x,y] -> (x, y)) $ take 2 stack
