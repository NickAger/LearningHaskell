-- from http://www.cs.virginia.edu/~wh5a/personal/Transformers.pdf
-- http://www.cs.virginia.edu/~wh5a/personal/Transformers.lhs
-- https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/example-of-why-to-use-monads-what-they-can-do
-- https://www.reddit.com/r/haskell/comments/un40c/monad_transformers_step_by_step/

-- see also:
-- http://blog.sigfpe.com/2010/02/tagging-monad-transformer-layers.html
-- http://book.realworldhaskell.org/read/monad-transformers.html

-- why do we use mapM in a monad?
-- mapM :: Monad m => (a -> m b) -> [a] -> m [b]
-- map :: (a -> b) -> [a] -> [b]

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b

-- (<$>) :: Functor f => (a -> b) -> f a -> f b
-- fmap :: (a -> b) -> f a -> f b

-- liftA :: Applicative f => (a -> b) -> f a -> f b
-- liftM :: Monad m => (a1 -> r) -> m a1 -> m r
-- Promote a function to a monad.

-- https://wiki.haskell.org/Existential_type


module Transformers where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map

type Name = String
data Exp = Lit Integer
          | Var Name
          | Plus Exp Exp
          | Abs Name Exp -- abstraction
          | App Exp Exp  -- function application
          deriving (Show)
          
data Value =  IntVal Integer
            | FunVal Env Name Exp
            deriving (Show)

type Env = Map.Map Name Value


eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = fromJust (Map.lookup n env)
eval0 env (Plus e1 e2) = let  IntVal i1 = eval0 env e1
                              IntVal i2 = eval0 env e2
                          in IntVal (i1 + i2)
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) = let val1 = eval0 env e1
                            val2 = eval0 env e2
                         in case val1 of
                            FunVal env' n body -> eval0 (Map.insert n val2 env') body
                            
exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x"))(Lit 4 `Plus` Lit 2))

-- ----------------
-- 2 Monad Transfers
-- ----------------

-- http://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Identity.html
-- The Identity monad is a monad that does not embody any computational strategy. It simply applies the bound function to its input without any modification. Computationally, there is no reason to use the Identity monad instead of the much simpler act of simply applying functions to their arguments. The purpose of the Identity monad is its fundamental role in the theory of monad transformers. Any monad transformer applied to the Identity monad yields a non-transformer version of that monad. 

type Eval1 alpha = Identity alpha
runEval1 :: Eval1 alpha -> alpha
runEval1 ev = runIdentity ev


eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Lit i) = return $ IntVal i
eval1 env (Var n) = return $ fromJust (Map.lookup n env)

eval1 env (Plus e1 e2) = do IntVal i1 <- eval1 env e1
                            IntVal i2 <- eval1 env e2
                            return $ IntVal (i1 + i2)
                         
eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App e1 e2) = do  val1 <- eval1 env e1
                            val2 <- eval1 env e2
                            case val1 of
                              FunVal env' n body ->
                                eval1 (Map.insert n val2 env') body
 
--instance Functor [] where
--    fmap = map

-- --------------
-- --------------
--maybeToEither :: a -> Maybe b -> Either a b
--maybeToEither leftValue = maybe (Left leftValue) Right


maybeToEither :: MonadError e m =>
                 e                      -- ^ (Left e) will be returned if the Maybe value is Nothing
              -> Maybe a                -- ^ (Right a) will be returned if this is (Just a)
              -> m a
maybeToEither errorval Nothing = throwError errorval
maybeToEither _ (Just normalval) = return normalval
-- --------------

type Eval2 alpha = ErrorT String Identity alpha 
runEval2 :: Eval2 alpha -> Either String alpha
runEval2 ev = runIdentity (runErrorT ev)

eval2a :: Env -> Exp -> Eval2 Value
eval2a env (Lit i) = return $ IntVal i
eval2a env (Var n) = maybeToEither ("undefined: '" ++ n ++ "'") (Map.lookup n env)

eval2a env (Plus e1 e2) = do e1' <- eval2a env e1
                             e2' <- eval2a env e2
                             case (e1', e2') of
                               (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                               _ -> throwError "type error"
                         
eval2a env (Abs n e) = return $ FunVal env n e
eval2a env (App e1 e2) = do  val1 <- eval2a env e1
                             val2 <- eval2a env e2
                             case val1 of
                               FunVal env' n body ->
                                eval2a (Map.insert n val2 env') body
                               _ -> throwError "type error"
