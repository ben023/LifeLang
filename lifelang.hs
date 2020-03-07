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
         | Rest Exp Int
         | Eat Exp Int
         | Damage Exp Int
         | HasStamina Exp
         | If Exp Exp Exp
    deriving(Eq,Show)



sem :: Exp -> Result
sem (Dec hstate) = OK hstate
sem (Rest e v) = case (sem e) of
                   (OK (pos, health, stamina)) -> (OK (pos, health, stamina+v))
                   _ -> Error
sem (Eat e v) = case (sem e) of
                   (OK (pos, health, stamina)) -> (OK (pos, health+v, stamina))
                   _ -> Error
sem (Damage e v) = case (sem e) of
                   (OK (pos, health, stamina)) -> if (health-v) <= 0 then (Dead pos) else OK (pos, health-v, stamina)
                   _ -> Error
sem (HasStamina e) = case (sem e) of
                   (OK (pos, health, stamina)) -> B (stamina <= 0)
                   _ -> Error
sem (If e1 e2 e3) = case (sem e1) of
                      B True -> sem e2
                      B False -> sem e3
                      _ -> Error

-- good example:


ex1 :: Exp
ex1 = Rest (Dec (0, 0, 0)) 10
-- Should return OK (0, 0, 10)

-- bad example:
ex4 :: Exp
ex4 = If (Dec (0, 0, 0)) (Damage (Dec (0, 0, 0)) 3) (Eat (Dec (0, 0, 0)) 3)
-- Should return Error
