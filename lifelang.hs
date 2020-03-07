module Lifelang where

import           Data.List

type Pos     = Int
type Health  = Int
type Stamina = Int
type Lethal  = Int
type Alive   = Bool

type Prog = [Cmd]

type HState = (Pos, Health, Stamina, Alive)
type OState = (Pos, Lethal)

data Result = OK HState
            | Dead Pos
        deriving(Eq, Show)

data Cmd = Jump
         | Walk
         | Rest
         | Eat
         | Damage
         | IsAlive
    deriving(Eq,Show)

cmd :: Cmd -> HState -> Result
cmd Walk    (p, h, s, True)     = OK (p+1, h, s-1, True)
cmd Walk    (p, h, 0, True)     = OK (p+1, h-1, 0, True)
cmd Walk    (p, h, 0, False)     = Dead p
cmd Jump    (p, h, s, True)     = OK (p+2, h, s-10, True)
cmd Jump    (p, h, 0, True)     = OK (p+2, h-10, 0, True)
cmd Jump    (p, 0, _, False)     = Dead p
cmd Rest    (p, h, s, True)     = OK (p, h+10, s+5, True)
cmd Eat     (p, h, s, True)     = OK (p, h+10, s+5, True)
cmd Damage  (p, h, s, True)     = OK (p, h-10, s, True)
cmd Damage  (p, 0, s, False)     = Dead p
cmd IsAlive (p, h, s, True)     = OK (p, h, s, True)
cmd IsAlive (p, h, s, False)    = Dead p

prog :: Prog -> HState -> Result
prog []     s = OK s
prog (c:cs) s = case cmd c s of
                OK s' -> prog cs s'
                Dead s' -> Dead s'
