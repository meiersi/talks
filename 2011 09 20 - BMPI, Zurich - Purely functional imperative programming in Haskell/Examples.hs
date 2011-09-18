-- Examples for talk about purely functional imperative programming in Haskell
--
-- Author: Simon Meier <iridcode@gmail.com>
-- Date:   2011/09/18


-- Let's import some stuff for our later examples :-)

-- natural logarithm and our logging operation clash
import Prelude hiding (log)

-- The standard Map datastructure.
import Data.Map (Map, findWithDefault, insert, fromList)

-- Support for effectful computations
import Control.Applicative
import Control.Monad (liftM2)
import Data.Foldable (asum)

-- Monadic parsing combinators from standard library
import Text.Parsec hiding (State, (<|>))


-----------------------------------------------------------------------------
-- Arithmetic Expressions
-----------------------------------------------------------------------------

data Op = Plus | Minus | Times | Divide
          deriving( Eq, Ord, Show)

data Expr = 
       Lit Integer
     | Bin Op Expr Expr
     deriving( Eq, Ord, Show)

evalOp :: Op -> (Integer -> Integer -> Integer)
evalOp Plus   = (+)
evalOp Minus  = (-)
evalOp Times  = (*)
evalOp Divide = div

eval :: Expr -> Integer
eval (Lit i)        = i
eval (Bin op e1 e2) = (evalOp op) (eval e1) (eval e2)

test :: Expr
test = Bin Minus (Lit 8) (Bin Times (Lit 2) (Lit 5))
                 

-----------------------------------------------------------------------------
-- Using 'Error a' to handle division by zero
-----------------------------------------------------------------------------

data Error a = Exception String
             | Result a

evalOpE :: Op -> (Integer -> Integer -> Error Integer)
evalOpE Divide _ 0 = Exception "division by 0"
evalOpE op     x y = Result (evalOp op x y)

evalE :: Expr -> Error Integer
evalE (Lit i)        = Result i
evalE (Bin op e1 e2) = 
  case evalE e1 of
    Exception msg -> Exception msg
    Result x1  -> 
      case evalE e2 of
        Exception msg -> Exception msg
        Result x2  -> evalOpE op x1 x2


testE1, testE2 :: Expr
testE1 = Bin Divide (Lit 10) (Lit 2)
testE2 = Bin Divide (Lit 10) (Lit 0)


-- The 'Error' Monad
--------------------

instance Monad Error where
  return x = Result x

  Exception e >>= _ = Exception e
  Result x    >>= f = f x

throw :: String -> Error a
throw exc = Exception exc

catch :: Error a -> (String -> Error a) -> Error a
catch (Exception exc) handler = handler exc
catch (Result x)      _       = Result x

-- The same code as above, this time written using do-notation
evalOpE' :: Op -> (Integer -> Integer -> Error Integer)
evalOpE' Divide _ 0 = throw "division by 0"
evalOpE' op     x y = return (evalOp op x y)

evalE' :: Expr -> Error Integer
evalE' (Lit i)        = return i
evalE' (Bin op e1 e2) = do x1 <- evalE' e1
                           x2 <- evalE' e2
                           evalOpE' op x1 x2


------------------------------------------------------------------------------
-- The 'State' Monad
------------------------------------------------------------------------------

data State s a = State (s -> (a, s))

put :: s -> State s ()
put new_state = State (\_old_state -> ( (), new_state))

get :: State s s 
get = State (\current_state -> ( current_state, current_state ))

runState :: State s a -> (s -> (a, s))
runState (State computeResult) s0 = computeResult s0

instance Monad (State s) where
    return x = State (\current_state -> (x, current_state))

    m >>= f = State computeResult
      where
        computeResult s0 =
            runState (f x) s1
          where
            (x, s1) = runState m s0


-- State Monad example: C++ style variable modifiers
----------------------------------------------------

data Modifier = AsIs | PreInc | PostInc
     deriving( Eq, Ord, Show )

data ExprV = 
       VarV Modifier String
     | LitV Integer
     | BinV Op ExprV ExprV
     deriving( Eq, Ord, Show)

type Env = Map String Integer


evalV :: ExprV -> State Env Integer
evalV (LitV i)          = return i
evalV (BinV op e1 e2)   = liftM2 (evalOp op) (evalV e1) (evalV e2)
evalV (VarV modifier v) = do
    env <- get
    let x  = findWithDefault (error (v ++ " not found")) v env
        x' = x + 1
    case modifier of 
      AsIs    -> return x
      PreInc  -> do {put (insert v x' env); return x'}
      PostInc -> do {put (insert v x' env); return x }

testV :: ExprV
testV = BinV Times (VarV PreInc "x") (VarV PostInc "x")

runTestV :: (Integer, Env)
runTestV = runState (evalV testV) (fromList [("x",0)])


------------------------------------------------------------------------------
-- The 'Parsec' monad for parsing
------------------------------------------------------------------------------

type Parse a = Parsec String () a

lit :: Parse Expr
lit = ((Lit . read) <$> many1 digit) <|> 
      (char '(' *> expr <* char ')')

factor :: Parse Expr
factor = binOp [('*', Times), ('-', Divide)] lit factor

expr :: Parse Expr
expr = binOp [('+', Plus), ('-', Minus)] factor expr

-- We abstract over the pattern for left-recursive parsing,
-- as we use it twice to parse operators according to their precedence.
--
-- We make use of <$>, <*>, and *> from Control.Applicative
-- to shorten the code.
binOp :: [(Char, Op)] -> Parse Expr -> Parse Expr -> Parse Expr
binOp ops leftOperand rightOperand = do
  x1 <- leftOperand
  (Bin <$> asum [ char c *> pure op | (c, op) <- ops ] 
       <*> pure x1 
       <*> rightOperand
   ) <|> return x1

parseWith :: Parse a -> String -> Either ParseError a
parseWith p inp = parse p "<dummy-source-file>" inp

testParse :: Either ParseError Expr
testParse = parseWith expr "1-2*(5-8)"


------------------------------------------------------------------------------
-- A cusomt 'Writer' monad that logs values during the execution
------------------------------------------------------------------------------

data ExprL = 
       LitL Integer
     | BinL Op ExprL ExprL
     | LoggedL String ExprL
     deriving( Eq, Ord, Show)

data Logged a = Logged (a, [String])
  deriving( Eq, Ord, Show )

evalL :: ExprL -> Logged Integer
evalL (LitL i)        = Logged (i, [])
evalL (BinL op e1 e2) = Logged (evalOp op x1 x2, log1 ++ log2)
  where
    Logged (x1, log1) = evalL e1
    Logged (x2, log2) = evalL e2
evalL (LoggedL msg e)    = 
    Logged (x, (msg ++ ": " ++ show x) : log1)
  where
    Logged (x, log1) = evalL e
    

testL :: ExprL
testL = BinL Minus (LoggedL "hello" (BinL Times (LitL 2) (LitL 5))) 
                   (LoggedL "world" (LitL 8))
  
-- The 'Logged' Functor, Applicative, and Monad instances
---------------------------------------------------------

instance Functor Logged where
  fmap f (Logged (x, log1)) = Logged (f x, log1)

instance Applicative Logged where
  -- pure and return are synonymous; a historical artifact
  pure x = Logged (x, [])

  Logged (f, log1) <*> Logged (x, log2) = Logged (f x, log1 ++ log2)


instance Monad Logged where
  return x = Logged (x, [])

  Logged (a, log1) >>= f = 
      Logged (b, log1 ++ log2)
    where
      Logged (b, log2) = f a


log :: String -> Logged ()
log msg = Logged ((), [msg])

runLogged :: Logged a -> (a, [String])
runLogged (Logged valueAndLog) = valueAndLog

-- compare this implementation to 'evalL' above: the plumbing is gone
evalL' :: ExprL -> Logged Integer
evalL' (LitL i)        = return i
evalL' (BinL op e1 e2) = evalOp op <$> evalL' e1 <*> evalL' e2
evalL' (LoggedL msg e) = do x <- evalL' e
                            log (msg ++ ": " ++ show x)
                            return x


------------------------------------------------------------------------------
-- The 'IO' monad
------------------------------------------------------------------------------

-- Hack to ensure sequential evaluation of real side-effecting operations
--
--   type IO a = State #World a
--
-- Exactly one function is executed with the actual state of the real world.
--
--   main:: IO ()
--
