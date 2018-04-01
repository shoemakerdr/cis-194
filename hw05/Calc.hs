import ExprT (ExprT(..), reify)
import Parser (parseExp)



main :: IO ()
main = do
  print testInteger
  print testBool
  print testMM
  print testSat




-- EXERCISE 1
eval :: ExprT -> Integer
eval (Lit num) = num
eval (Add expr1 expr2) = eval expr1 + eval expr2
eval (Mul expr1 expr2) = eval expr1 * eval expr2




-- EXERCISE 2
evalStr :: String -> Maybe Integer
evalStr str =
  eval <$> parseExp Lit Add Mul str




-- EXERCISE 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a
  
  
instance Expr ExprT where
  lit x = Lit x
  add x y = Add x y
  mul x y = Mul x y



-- EXERCISE 4
newtype MinMax =
  MinMax Integer deriving (Eq, Show)


newtype Mod7 =
  Mod7 Integer deriving (Eq, Show)


instance Expr Integer where
  lit x = x
  add x y = x + y
  mul x y = x * y


instance Expr Bool where
  lit x
    | x > 0 = True
    | otherwise = False
  add x y = x || y
  mul x y = x && y


instance Expr MinMax where
  lit x = MinMax x
  add (MinMax x) (MinMax y) = MinMax $ maximum [x, y]
  mul (MinMax x) (MinMax y) = MinMax $ minimum [x, y]


instance Expr Mod7 where
  lit x = Mod7 $ x `mod` 7
  add (Mod7 x) (Mod7 y) = lit (x + y)
  mul (Mod7 x) (Mod7 y) = lit (x * y)


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"


testInteger  = testExp :: Maybe Integer
testBool     = testExp :: Maybe Bool
testMM       = testExp :: Maybe MinMax
testSat      = testExp :: Maybe Mod7

