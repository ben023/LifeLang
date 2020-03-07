module Lifelang where

import           Data.List



Type Var = String
data Expr = Lit Int        -- literal integer
          | Add Expr Expr  -- integer addition
          | Sub Expr Expr — integer subtraction
          | LTE Expr Expr  -- less than or equal to
          | Ref Var        -- variable reference
  deriving (Eq,Show)

data Stmt = Bind Var Expr
          | If Expr Stmt Stmt
          | While Expr Stmt
          | Block [Stmt]
  deriving (Eq,Show)


type Pos = Int
type Health = Int
type Stamina = Int
type Lethal = Int

type Prog = [Cmd]

data Feet = Up
          | Down
          deriving (Eq, Show)

type HState = (Pos, Health, Stamina, Feet)
type OState = (Pos, Lethal)

type Domain = HState -> Result

data Result = OK HState
            | Dead Pos

data Cmd = Jump
         | Rest Int
         | Eat Int
         | Damage HState OState

prog :: Prog -> HState -> Result
prog []     s = OK HState
prog (c:cs) s = case cmd c s of
                OK s' -> prog cs s'
                Dead  -> Dead
