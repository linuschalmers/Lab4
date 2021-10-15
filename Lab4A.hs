-- Authors: Albin Boklund, Linus Lindström, Isak Nordin
-- Group: 12
-- Date: 2021-10-15

import Poly
import Test.QuickCheck


-- Use the following simple data type for binary operators
data BinOp = AddOp | MulOp

--------------------------------------------------------------------------------
-- * A1
-- Define a data type Expr which represents three kinds of expression:
-- binary operators (use BinOp as a helper type) applied to two expressions,
-- numbers (use Int), and exponentiation x^n.
-- Note that since we consider expressions containing just a single variable,
-- x, your data type should not use String or Char anywhere, since this is
-- not needed.

data Expr = Oper BinOp Expr Expr | Numeric Int | Expo Int 

exprTest1 :: Expr
exprTest1 = Oper AddOp (Expo (3)) (Expo 2)

exprTest2 :: Expr
exprTest2 = Oper AddOp (Expo 2) (Numeric 3)


exprTest3:: Expr
exprTest3 = Oper MulOp (Numeric 5) (Numeric 3)

exprTest4 :: Expr
exprTest4 = Oper AddOp (Numeric 2) (Numeric 0)


--------------------------------------------------------------------------------
-- * A2
-- Define the data type invariant that checks that exponents are never negative
-- Property for Expr to uphold
prop_Expr :: Expr -> Bool
prop_Expr (Expo n) = (n >= 0)
prop_Expr (Numeric n) = True
prop_Expr (Oper binop expr1 expr2) = prop_Expr (expr1) && prop_Expr (expr2)


--------------------------------------------------------------------------------
-- * A3
-- Make Expr an instance of Show (along the lines of the example in the lecture)
-- You can use Haskell notation for powers: x^2 
-- You should show x^1 as just x. 

instance Show Expr where
 show = showExpr

--Shows an expression in text form by using show
showExpr :: Expr -> String
showExpr expr = case expr of
  Numeric n -> show n
  Oper AddOp expr1 expr2 -> "(" ++ showExpr (expr1) ++ " + " ++ showExpr (expr2) ++ ")"
  Oper MulOp expr1 expr2 -> "(" ++ showExpr (expr1) ++ " * " ++ showExpr (expr2) ++ ")"
  Expo 1 -> "x"
  Expo n -> "x^" ++ show n


--------------------------------------------------------------------------------
-- * A4
-- Make Expr and instance of Arbitrary.
-- Now you can check the data type invariant that you defined in A3 using
-- quickCheck

-- (Optional)
-- Add a definition of function shrink :: Expr -> [Expr] to Arbitrary
-- which gives hints to quickCheck on possible smaller expressions that it
-- could use to find a smaller counterexample for failing tests


instance Arbitrary Expr
 where arbitrary = sized genExpr

genExpr :: Int -> Gen Expr
genExpr size = frequency [(1, genNum), (3, genExpo), (size, genOp)] 
   where  
     genNum = do
      n <- choose (0, 10)    
      return (Numeric n)

     genExpo = do 
      n <- choose (1, 9)
      return (Expo n) 

     genOp = let n = size `div` 2 in do  
      op <- elements [AddOp, MulOp]    
      x  <- genExpr n
      y  <- genExpr n
      return (Oper op x y)
    
--------------------------------------------------------------------------------
-- * A5
-- Define the eval function which takes a value for x and an expression and
-- evaluates it

-- Function for evaluating expressions by using case of 
eval :: Int -> Expr -> Int
eval x expr = case expr of
  Numeric n -> n
  Oper AddOp expr1 expr2 -> (eval x expr1) + (eval x expr2)
  Oper MulOp expr1 expr2 -> (eval x expr1) * (eval x expr2)
  Expo n -> (x^n)


--------------------------------------------------------------------------------
-- * A6
-- Define
-- Which converts an expression into a polynomial.
-- Here it is important to think recursively to just solve the bigger problem
-- by solving the smaller problems and combining them in the right way.

--Function for converting expressions to Poly by using fromList 
exprToPoly :: Expr -> Poly
exprToPoly (Numeric n) = fromList [n]
exprToPoly (Expo n) = fromList (1 : replicate n 0)
exprToPoly (Oper AddOp expr1 expr2) = exprToPoly (expr1) + exprToPoly (expr2)
exprToPoly (Oper MulOp expr1 expr2) = exprToPoly (expr1) * exprToPoly (expr2)


-- Define (and check) prop_exprToPoly, which checks that evaluating the
-- polynomial you get from exprToPoly gives the same answer as evaluating
-- the expression

-- Property for exprToPoly to uphold 
prop_exprToPoly :: Int -> Expr -> Bool
prop_exprToPoly n expr = evalPoly n (exprToPoly expr) == eval n expr

--------------------------------------------------------------------------------
-- * A7
-- Now define the function going in the other direction, 
{--polyToExpr :: Poly -> Expr
polyToExpr poly =
 |  = [x | x = ]--}


add (Numeric 0) x = x
add x (Numeric 0) = x
add (Numeric m) (Numeric n) = Numeric (n+m)
add x y = Oper AddOp x y

mul (Numeric 0) x = Numeric 0
mul e (Numeric 0) = Numeric 0
mul (Numeric 1) x = x
mul x (Numeric 1) = x
mul (Numeric m) (Numeric n) = Numeric (m*n) 
mul x y = mul x y

smartExpo _ (Numeric 0) = Numeric 1
smartExpo x (Numeric 1) = x
smartExpo x (Expo n) = Expo n
-- Write (and check) a quickCheck property for this function similar to
-- question 6. 
prop_polyToExpr = undefined

--------------------------------------------------------------------------------
-- * A8
-- Write a function
{--simplify :: Expr -> Expr
-- which simplifies an expression by converting it to a polynomial
-- and back again
simplify expr = polyToExpr(exprToPoly expr)--} 

--------------------------------------------------------------------------------
-- * A9
-- Write a quickCheck property
prop_noJunk :: Expr -> Bool

--that checks that a simplified expression does not contain any "junk":
--where junk is defined to be multiplication by one or zero,
--addition of zero, addition or multiplication of numbers, or x to the
--power zero. (You may need to fix A7)
prop_noJunk (Numeric 0) = False
prop_noJunk (Oper MulOp (Numeric 1) _) = False
prop_noJunk (Oper MulOp _ (Numeric 1)) = False
prop_noJunk (Oper AddOp x y) = prop_noJunk(x) && prop_noJunk(y) 
prop_noJunk (Oper MulOp x y) = prop_noJunk(x) && prop_noJunk(y) 
prop_noJunk (Expo y) = y/=0

--------------------------------------------------------------------------------
