
import Prelude hiding (log)

import Data.Map (Map, findWithDefault, insert, fromList)
import Control.Applicative
import Control.Monad (guard)
import Data.Foldable (asum)

import Text.Parsec hiding (State, (<|>))

-- Intro: Haskell Syntax

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
                 

-- values with logs

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
  

-- partial values

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

testP1, testP2 :: Expr
testP1 = Bin Divide (Lit 10) (Lit 2)
testP2 = Bin Divide (Lit 10) (Lit 0)

-- Monads

-- Error Monad

instance Monad Error where
  return x = Result x

  Exception e >>= _ = Exception e
  Result x    >>= f = f x


throw :: String -> Error a
throw exc = Exception exc

catch :: Error a -> (String -> Error a) -> Error a
catch (Exception exc) handler = handler exc
catch (Result x)      _       = Result x

-- Error Monad example

evalOpE' :: Op -> (Integer -> Integer -> Error Integer)
evalOpE' Divide _ 0 = throw "division by 0"
evalOpE' op     x y = return (evalOp op x y)

evalE' :: Expr -> Error Integer
evalE' (Lit i)        = return i
evalE' (Bin op e1 e2) = do x1 <- evalE' e1
                           x2 <- evalE' e2
                           evalOpE' op x1 x2
-- evalP' (Bin op e1 e2) =  evalP' e1 >>= (\x1 ->
--                           evalP' e2 >>= (\x2 ->
--                            evalOpP op x1 x2
--                          ))


liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f ma mb = do a <- ma
                    b <- mb
                    return (f a b)

instance Monad Logged where
  return x = Logged (x, [])

  Logged (a, log1) >>= f = 
      Logged (b, log1 ++ log2)
    where
      Logged (b, log2) = f a


-- The 'Logged' monad

log :: String -> Logged ()
log msg = Logged ((), [msg])

runLogged :: Logged a -> (a, [String])
runLogged (Logged valueAndLog) = valueAndLog

evalL' :: ExprL -> Logged Integer
evalL' (LitL i)        = return i
evalL' (BinL op e1 e2) = do x1 <- evalL' e1
                            x2 <- evalL' e2
                            return (evalOp op x1 x2)
evalL' (LoggedL msg e) = do x <- evalL' e
                            log (msg ++ ": " ++ show x)
                            return x

-- The 'State' monad

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

-- State Monad example

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

-- Further Examples of Monads:

-- logging

data Parser a = Parser (String -> [(a, String)])


type Nondeterministic a = [a]

asProduct :: Integer -> Nondeterministic (Integer, Integer)
asProduct n = do
  x1 <- [1..n]
  x2 <- [1..n]
  guard (x1 * x2 == n)
  return (x1, x2)


-- Parsec

type Parse a = Parsec String () a

lit :: Parse Expr
lit = ((Lit . read) <$> many1 digit) <|> 
      (char '(' *> expr <* char ')')

factor :: Parse Expr
factor = binOp [('*', Times), ('-', Divide)] lit factor

expr :: Parse Expr
expr = binOp [('+', Plus), ('-', Minus)] factor expr

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

-- IO Monad

-- Hack to ensure sequential evaluation of real side-effecting operations
--
--   type IO a = State #World a
--
-- Exactly one function is evaluated with the actual state of the real world.
--
--   main:: IO ()
--

-- Outlook:

-- Monad transfomers: monads a la carte
-- Applicative funtors: weaker than Monads, often sufficient
--   - for parsers: context-free vs. context-sensitive grammars
--   the fewer choice => the fewer mistakes
--
-- More type-classes capturing computation patterns:
--   Semirings: associative operator => allows parallel evaluation
--   Monoids: Semirings with identity element
--   MonadPlus: Monad with alternative (e.g. parser)
--   Traversable: containers with fixed traversing scheme
--
-- => Typeclassopedia


-- Monad laws
--
-- Allow reasoning about (>>=) and (return) independent of implementation.
-- Ensure that (>>=) and (return) behave as expected.


-- Conclusion:
--
--  Pure functions: gold standard for compositionality
--
--  Monads allow imperative programming 
--
--  Haskell expressive enough to abstract over construction
--  such as Monads, Applicative Functors, ...




-- Side note: Defining your own operators

infixl 6 .+
infixl 6 .-
infixl 7 .*
infixl 7 ./

(.+), (.-), (.*), (./) :: Expr -> Expr -> Expr
(.+) = Bin Plus
(.-) = Bin Minus
(.*) = Bin Times
(./) = Bin Divide

test' :: Expr
test' = (Lit 2) .* (Lit 5) .- (Lit 8)

instance Functor Logged where
  fmap f (Logged (x, log1)) = Logged (f x, log1)

instance Applicative Logged where
  pure = return
  Logged (f, log1) <*> Logged (x, log2) = 
      Logged (f x, log1 ++ log2)

