import Base.Char (digitToInt)

data token = Num Int | Op Opkind
data Opkind = <-> | <+> | <*> | </> | <[> | <]>

tokenStack :: String -> Either String [Token]
tokenStack = foldl (\x xs -> tokenize x xs) []

tokenize :: Char -> [Token] -> [Token]
tokenize x
  | isDigit x = reduceTerm (digitToInt x) xs
  | x = ')'   = reduceParens xs
  | otherwise = case x of
    '+' -> <+> : xs
    '-' -> <-> : xs
    '*' -> <*> : xs
    '/' -> </> : xs
