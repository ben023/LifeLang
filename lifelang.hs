module Lifelang where

import Data.Map (Map,fromList,lookup,insert)
import Data.Maybe (fromJust)
import Prelude hiding (lookup)

type Pos = Int
type Health = Int
type Stamina = Int

type HState = (Pos, Health, Stamina)

type Var = String

data Expr = Lit Int            --Integer Literal
            | State HState      --HState
            | IsAlive Expr     --Boolean if health is not 0
            | Walk Expr
            | Rest Expr Expr
            | Ref Var
  deriving (Eq,Show)

data Stmt = Bind Var Expr
            | While Expr Stmt
            | Block [Stmt]
  deriving (Eq,Show)

data Type = TInt | TBool | THState
  deriving (Eq,Show)

type Decl = (Var,Type)
data Prog = P [Decl] Stmt
  deriving (Eq,Show)



exTemplate :: Prog
exTemplate = P [("restTime", TInt), ("startingState", THState)]
            (Block [
                Bind "restTime" (Lit 4),
                Bind "startingState" (State (0, 100, 100)),
                While (IsAlive(Ref "startingState")) 
                (Block [
                    Bind "startingState" (Walk (Ref "startingState")),
                    Bind "startingState" (Rest (Ref "startingState") (Ref "restTime"))
                ])
            ])

type Env a = Map Var a

typeExpr :: Expr -> Env Type -> Maybe Type
typeExpr (Lit _) _ = Just TInt
typeExpr (State _) _ = Just THState
typeExpr (IsAlive x) m = case (typeExpr x m) of
                            (Just THState) -> Just TBool
                            _              -> Nothing
typeExpr (Walk x) m = case (typeExpr x m) of
                            (Just THState) -> Just THState
                            _              -> Nothing
typeExpr (Rest x y) m = case (typeExpr x m, typeExpr y m) of
                            (Just THState, Just TInt) -> Just THState
                            _              -> Nothing
typeExpr (Ref v)   m  = lookup v m


typeStmt :: Stmt -> Env Type -> Bool
typeStmt (Bind v e)   m = case (lookup v m, typeExpr e m) of
                            (Just tv, Just te) -> tv == te
                            _ -> False
typeStmt (While c sb) m = case typeExpr c m of
                            Just TBool -> typeStmt sb m
                            _ -> False
typeStmt (Block ss)   m = all (\s -> typeStmt s m) ss


typeProg :: Prog -> Bool
typeProg (P ds s) = typeStmt s (fromList ds)

data Val = I Int | B Bool | HS HState
  deriving (Eq,Show)


evalExpr :: Expr -> Env Val -> Val
evalExpr (Lit i) _ = I i
evalExpr (State s) _ = HS s
-- evalExpr (IsAlive x) m = case (evalHState x m) of
--                         (x, y, z) -> B (y > 0)
-- evalExpr (IsAlive x) m = case (evalHState x m) of
--                         (x, y, z) -> B (y > 0)

evalExpr (IsAlive x) m |
                        (p, h, s) <- (evalHState x m) = B (h < 0)

-- evalExpr (Walk x) m = HS (evalHState x m)
-- evalExpr (Rest x y) m = HS (evalHState )

evalInt :: Expr -> Env Val -> Int
evalInt e m = case evalExpr e m of
                I i  -> i
                _ -> error "internal error: expected Int"

-- evalBool :: Expr -> Env Val -> Bool
-- evalBool e m = case evalExpr e m of
--                  B b -> b
--                 _  -> error "internal error: expected Bool"

evalHState :: Expr -> Env Val -> HState
evalHState e m = case evalExpr e m of
                 HS s -> s
                 _ -> error "internal error: expected HState"