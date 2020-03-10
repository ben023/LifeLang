module Lifelang where

import           Data.List

type Pos = Int
type Health = Int
type Stamina = Int

type HState = (Pos, Health, Stamina)

data Result = OK HState
            | Dead Pos
            | Error
            | B Bool
        deriving(Eq, Show)


data Exp = Dec HState
--         | Var String
         | Lit Int
--         | Rest Exp Int
--         | Eat Exp Int
         | Damage Exp Int
         | HasStamina Exp
--         | If Exp Exp Exp
         | Ref Exp

   deriving(Eq,Show)



data Stmt = Bind String Exp
          | If Exp Stmt Stmt
          | While Exp Stmt
          | Block [Stmt]
    deriving(Eq,Show)

--prog :: Prog -> HState -> Result
--prog [] _ = (OK HState)
--prog (x:xs) (y) = prog xs (sem Dec y)

data Type = TInt | TBool | HState

type Decl = (Var, Type)

data Prog = P [Decl] Stmt

ex1 :: Prog
ex1 = P [("restTime", TInt), ("startState", HState)]
        (Block [
           Bind "restTime" (Lit 5),
           Bind "startState" (0,100,100),
           While (HasStamina (Ref "startState"))
           (Block [
               Bind "startState" (Damage (Ref "startState") (Lit 10)),
               Bind "startState" (Damage (Ref "startState") (Lit 10))
           ])
        ])
