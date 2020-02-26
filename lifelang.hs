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
         | Rest Int
         | Eat Int
         | Damage HState OState

prog :: Prog -> HState -> Result
prog []     s = OK HState
prog (c:cs) s = case cmd c s of
                OK s' -> prog cs s'
                Dead  -> Dead
