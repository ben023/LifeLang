module Lifelang where

import           Data.List

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


