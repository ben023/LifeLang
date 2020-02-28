module Lifelang where

import           Data.List

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

data Result = OK HState
            | Dead Pos

data Cmd = Jump
         | Rest
         | Eat
         | Damage
    deriving(Eq,Show)

cmd :: Cmd -> HState -> Result
cmd Jump    (p, h, s, f)     = OK (p+2,h,s,f)
cmd Rest    (p,h,s,f)        = OK (p, h+10, s, f)
cmd Eat     (p, h, s, f)     = OK (p, h+10, s, f)
cmd Damage  (p, h, s, f)     = OK (p, h-10, s, f)

prog :: Prog -> HState -> Result
prog []     s = OK s
prog (c:cs) s = case cmd c s of
                OK s' -> prog cs s'
                Dead s' -> Dead s'
