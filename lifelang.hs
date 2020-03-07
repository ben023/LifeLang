module Lifelang where

import           Data.List

type Pos = Int
type Health = Int
type Stamina = Int
type Lethal = Int

type Prog = [Cmd]

type HState = (Pos, Health, Stamina)
type OState = (Pos, Lethal)

data Result = OK HState
            | Dead Pos
        deriving(Eq, Show)

data Cmd = Jump
         | Rest
         | Eat
         | Damage
    deriving(Eq,Show)

cmd :: Cmd -> HState -> Result
cmd Jump    (p, h, s)     = OK (p+2, h, s-10)
cmd Jump    (p, h, 0)     = OK (p+2, h-10, 0)
cmd Jump    (p, 0, _)     = Dead p
cmd Rest    (p, h, s)     = OK (p, h+10, s+5)
cmd Eat     (p, h, s)     = OK (p, h+10, s+5)
cmd Damage  (p, h, s)     = OK (p, h-10, s)
cmd Damage  (p, 0, s)     = Dead p

prog :: Prog -> HState -> Result
prog []     s = OK s
prog (c:cs) s = case cmd c s of
                OK s' -> prog cs s'
                Dead s' -> Dead s'
