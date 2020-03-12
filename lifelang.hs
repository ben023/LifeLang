module Lifelang where

import           Data.Map   (Map, fromList, insert, lookup)
import           Data.Maybe
import           Prelude    hiding (lookup)
--import           Data.List

type Pos = Int
type Health = Int
type Stamina = Int
type Func = String
type HState = (Pos, Health, Stamina)

--data Result = OK HState
--            | Dead Pos
--            | Error
--            | B Bool
--        deriving(Eq, Show)

type Var = String

data Exp = Dec HState
         | Lit Int
         | Damage Exp Exp
         | Eat Exp Exp
         | Rest Exp Exp
         | Walk Exp Exp
         | HasStamina Exp
         | IsAlive Exp
         | Ref String
   deriving(Eq,Show)

data Stmt = Bind String Exp
          | If Exp Stmt Stmt
          | While Exp Stmt
          | Block [Stmt]
          | Define Func [Var] [Exp]
          | Call Func [Exp]
    deriving(Eq,Show)

hibernate = Define "hibernate" ["days"]
  []
data Type = TInt | TBool | HState
    deriving(Eq,Show)


type Decl = (Var, Type)

data Prog = P [Decl] Stmt

ex1 :: Prog
ex1 = P [("restTime", TInt), ("startState", HState)]
        (Block [
           Bind "restTime" (Lit 5),
           Bind "startState" (Dec (0,100,100)),
           While (IsAlive (Ref "startState"))
           (Block [
               Bind "startState" (Damage (Ref "startState") (Lit 10)),
               Bind "startState" (Damage (Ref "startState") (Lit 10))
           ])
        ])

ex2 :: Prog
ex2 = P [("startState", HState)]
        (Block [
          Bind "startState" (Dec (0, 100, 100)),
          While (IsAlive (Ref "startState"))
          (Block [
              Bind "startState" (Damage (Ref "startState") (Lit 10)),
              Bind "startState" (Eat (Ref "startState") (Lit 7)),
              Bind "startState" (Walk (Ref "startState") (Lit 5)),
              Bind "startState" (Rest (Ref "startState") (Lit 1))
          ])

        ])

type Env a = Map Var a

--Implement a static type system by checking all inputs before running

typeExpr :: Exp -> Env Type -> Maybe Type
typeExpr (Dec hstate)     m  = Just HState
typeExpr (Lit i)          m  = Just TInt
typeExpr (Damage e1 e2)   m  =  case (typeExpr e1 m, typeExpr e2 m) of
                                   (Just HState, Just TInt) -> Just HState
                                   _                        -> Nothing
typeExpr (Eat e1 e2)   m  =  case (typeExpr e1 m, typeExpr e2 m) of
                                  (Just HState, Just TInt) -> Just HState
                                  _                        -> Nothing
typeExpr (Rest e1 e2)   m  =  case (typeExpr e1 m, typeExpr e2 m) of
                                  (Just HState, Just TInt) -> Just HState
                                  _                        -> Nothing
typeExpr (Walk e1 e2)   m  =  case (typeExpr e1 m, typeExpr e2 m) of
                                  (Just HState, Just TInt) -> Just HState
                                  _                        -> Nothing
typeExpr (HasStamina e)   m  = case (typeExpr e m) of
                                   (Just HState) -> Just TBool
                                   _             -> Nothing
typeExpr (IsAlive e)   m  = case (typeExpr e m) of
                                  (Just HState) -> Just TBool
                                  _             -> Nothing
typeExpr (Ref v)          m  = lookup v m


typeStmt :: Stmt -> Env Type -> Bool
typeStmt (Bind v e)   m = case (lookup v m, typeExpr e m) of
                            (Just tv, Just te) -> tv == te
                            _                  -> False
typeStmt (If c st se) m = case typeExpr c m of
                            Just TBool -> typeStmt st m && typeStmt se m
                            _          -> False
typeStmt (While c sb) m = case typeExpr c m of
                            Just TBool -> typeStmt sb m
                            _          -> False
typeStmt (Block ss)   m = all (\s -> typeStmt s m) ss


typeProg :: Prog -> Bool
typeProg (P ds s) = typeStmt s (fromList ds)

-- SEMANTICSSSSSSS

data Val = B Bool | I Int | HS HState
  deriving(Eq,Show)

evalExpr :: Exp -> Env Val -> Val
evalExpr (Dec hs)       _ = HS hs
evalExpr (Lit i)        _ = I i
evalExpr (Damage e damageDone) m
                          | (p, h, s) <- evalHS e m = HS (p, h-(evalInt damageDone m), s)
evalExpr (Eat e healthGained) m
                          | (p, h, s) <- evalHS e m = HS (p, h+(evalInt healthGained m), s)
evalExpr (Rest e staminaGained) m
                          | (p, h, s) <- evalHS e m = HS (p, h, s+(evalInt staminaGained m))
evalExpr (Walk e distanceGained) m
                          | (p, h, s) <- evalHS e m = HS (p+(evalInt distanceGained m), h, s-(evalInt distanceGained m))
evalExpr (HasStamina e) m
                          | (p, h, s) <- evalHS e m = B (s > 0)
                          | _ <- evalHS e m = B False
evalExpr (IsAlive e)    m
                          | (p, h, s) <- evalHS e m = B (h > 0)
                          | _ <- evalHS e m = B False

evalExpr (Ref x)   m = case lookup x m of
                         Just v  -> v
                         Nothing -> error "internal error: undefined variable"

evalHS :: Exp -> Env Val -> HState
evalHS e m = case (evalExpr e m) of
                HS s -> s
                _    -> error "internal error: expected HS, got bool or in"


evalInt :: Exp -> Env Val -> Int
evalInt e m = case evalExpr e m of
                I i  -> i
                _ -> error "internal error: expected int, got bool or hstate"


-- | Helper function to evaluate an expression to a Boolean.
evalBool :: Exp -> Env Val -> Bool
evalBool e m = case evalExpr e m of
                 B b -> b
                 _  -> error "internal error: expected Bool, got int or hstate"

-- | Semantics of statements. Statements update the bindings in the
--   environment, so the semantic domain is 'Env Val -> Env Val'. The
--   bind case is the case that actually changes the environment. The
--   other cases should look similar to other examples you've seen.
evalStmt :: Stmt -> Env Val -> Env Val
evalStmt (Bind x e)   m = insert x (evalExpr e m) m
evalStmt (If c st se) m = if evalBool c m
                        then evalStmt st m
                        else evalStmt se m
evalStmt (While c sb) m = if evalBool c m
                        then evalStmt (While c sb) (evalStmt sb m)
                        else m
evalStmt (Block ss)   m = evalStmts ss m

-- | Helper function to evaluate a list of statements. We could also
--   have used 'foldl' here.
evalStmts :: [Stmt] -> Env Val -> Env Val
evalStmts []     m = m
evalStmts (s:ss) m = evalStmts ss (evalStmt s m)

-- | Semantics of programs. This runs a program with an initial
--   environment where all integer variables are initialized to 0, and
--   all Boolean variables are initialized to false.
evalProg :: Prog -> Env Val
evalProg (P ds s) = evalStmt s m
 where
  m = fromList (map (\(x,t) -> (x, init t)) ds)
  init TInt   = I 0
  init TBool  = B False
  init HState = HS (0,0,0)

-- | Type check and then run a program.
runProg :: Prog -> Maybe (Env Val)
runProg p = if typeProg p then Just (evalProg p)
                        else Nothing
prettyExp :: Exp -> String


prettyStmt :: Stmt -> String


prettyDec :: [Decl] -> String
prettyDec []      = []
prettyDec ((v,TInt):p) = "Variable " ++ v ++ " declared as an Int" ++ prettyDec p ++ "\n"
prettyDec ((v, TBool): p) = "Variable " ++ v ++ "declared as a TBool" ++ prettyDec p ++ "\n"
prettyDec ((v, HState): p) = "Variable " ++ v ++ "declared as an HState" ++ prettyDec p "\n"

prettyProg :: Prog -> String
prettyProg (P d s) =

runPretty :: Prog -> String
runPretty p = if typeProg p then prettyProg p
                    else = "Type error: Could not verify program types"
