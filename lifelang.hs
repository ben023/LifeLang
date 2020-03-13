module Lifelang where

import           Data.Map   (Map, fromList, insert, lookup)
import           Data.Maybe
import           Prelude    hiding (lookup)

type Pos = Int
type Health = Int
type Stamina = Int
type Func = String

type HState = (Pos, Health, Stamina)

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

--
-- hibernate = Define "hibernate" ["days"]
--   []

data Type = TInt | TBool | HState
    deriving(Eq,Show)


type Decl = (Var, Type)
data Prog = P [Decl] Stmt


-- Good Examples of programs

-- ex1: This program initializes a start state with 100 health and damanges
-- the human by 10 units until they are dead
ex1 :: Prog
ex1 = P [("restTime", TInt), ("startState", HState)]
        (Block [
           Bind "startState" (Dec (0,100,100)),
           While (IsAlive (Ref "startState"))
           (Block [
               Bind "startState" (Damage (Ref "startState") (Lit 10)),
               Bind "startState" (Damage (Ref "startState") (Lit 10))
           ])
        ])

-- Returns Just (fromList [("restTime",I 0),("startState",HS (0,0,100))])
runEx1 :: Maybe (Env Val)
runEx1 = runProg ex1


-- Damages and heals and exhausts stamina and rest. Will eventually lead to death
-- The stamina can be restored from 0, but once health reaches 0 the user dies

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

-- Returns Just (fromList [("startState",HS (129,0,1))])
runEx2 :: Maybe (Env Val)
runEx2 = runProg ex2


-- Will kill the human since walking 200 units will exhaust stamina, then health
-- Will walk forward 200 units as well
ex3 :: Prog
ex3 = P [("startState", HState)]
        (Block [
          Bind "startState" (Dec (0, 100, 100)),
          (Block [
              Bind "startState" (Walk (Ref "startState") (Lit 50)),
              Bind "startState" (Walk (Ref "startState") (Lit 50)),
              Bind "startState" (Walk (Ref "startState") (Lit 50)),
              Bind "startState" (Walk (Ref "startState") (Lit 50))
          ])
        ])

-- Returns Just (fromList [("startState",HS (200,0,0))])
runEx3 :: Maybe (Env Val)
runEx3 = runProg ex3

-- Example of walking enough to damage 50 health, but not kill the user
-- this is because the human exhausts all stamina and then walks 50 units
ex4 :: Prog
ex4 = P [("startState", HState)]
        (Block [
          Bind "startState" (Dec (0, 100, 100)),
          (Block [
              Bind "startState" (Walk (Ref "startState") (Lit 50)),
              Bind "startState" (Walk (Ref "startState") (Lit 50)),
              Bind "startState" (Walk (Ref "startState") (Lit 50))
          ])
        ])

-- Returns Just (fromList [("startState",HS (200,0,0))])
runEx4 :: Maybe (Env Val)
runEx4 = runProg ex4



-- Example of Function call, human sleeps to death
ex5 :: Prog
ex5 = P [("restTime", TInt), ("startState", HState)]
        (Block [
           Bind "restTime" (Lit 5),
           Bind "startState" (Dec (0,100,100)),
           While (IsAlive (Ref "startState"))
           (insertFunction ((sleep 10) ++ [Bind "startState" (Damage (Ref "startState") (Lit 10))]))

        ])

-- Returns Just (fromList [("restTime",I 5),("startState",HS (0,0,210))])
runEx5 :: Maybe (Env Val)
runEx5 = runProg ex5

-- Bad Examples

-- Executing a command on a human who is already dead will not do anything
ex6 :: Prog
ex6 = P [("startState", HState)]
        (Block [
          Bind "startState" (Dec (0, 100, 100)),
          (Block [
              Bind "startState" (Walk (Ref "startState") (Lit 200)),
              Bind "startState" (Walk (Ref "startState") (Lit 50))
          ])
        ])
-- Returns Just (fromList [("startState",HS (100,0,0))])
-- Heath does not go negative
runEx6 :: Maybe (Env Val)
runEx6 = runProg ex6

-- Referencing a variable that does not exist will generate an error and progra
-- will not run
ex7 :: Prog
ex7 = P [("restTime", TInt), ("startState", HState)]
        (Block [
           Bind "startState" (Dec (0,100,100)),
           While (IsAlive (Ref "startState"))
           (Block [
               Bind "startState" (Damage (Ref "nonExistentState") (Lit 10)),
               Bind "startState" (Damage (Ref "startState") (Lit 10))
           ])
        ])
-- Returns Nothing (type checker catches)
runEx7 :: Maybe (Env Val)
runEx7 = runProg ex7


-- Assigning a state to a variable declared as Int (startState) will error
ex8 :: Prog
ex8 = P [("restTime", TInt), ("startState", TInt)]
        (Block [
           Bind "startState" (Dec (0,100,100)),
           While (IsAlive (Ref "startState"))
           (Block [
               Bind "startState" (Damage (Ref "startState") (Lit 10)),
               Bind "startState" (Damage (Ref "startState") (Lit 10))
           ])
        ])
-- Returns Nothing (type checker catches)
runEx8 :: Maybe (Env Val)
runEx8 = runProg ex8


-- Various examples of running the pretty print functions on Expressions

exprettyDec :: String
exprettyDec = prettyExp (Dec (0, 100, 100))

exprettyDamage :: String
exprettyDamage  =  prettyExp (Damage (Lit 1)  (Lit 2))

exprettyEat :: String
exprettyEat = prettyExp (Eat (Lit 10) (Lit 60))

exprettyRest :: String
exprettyRest = prettyExp (Rest (Lit 10) (Lit 70))

exprettyWalk :: String
exprettyWalk = prettyExp (Walk (Lit 0) (Lit 1))

exprettyStam :: String
exprettyStam = prettyExp (HasStamina (Lit 100))

exprettyAlive :: String
exprettyAlive = prettyExp (IsAlive (Lit 1))

exprettyStory :: String
exprettyStory = prettyExp (Dec (0, 100, 100)) ++ prettyExp (Damage(Lit 10) (Lit 90)) ++ "\n" ++
                prettyExp (Rest (Lit 10) (Lit 100)) ++ prettyExp (Walk (Lit 0) (Lit 1)) ++ "\n" ++
                prettyExp (HasStamina(Lit 99)) ++ prettyExp (Damage (Lit 20) (Lit 80)) ++ "\n"


--

-- Various examples of running the pretty print functions on Programs

prettyEx1 :: String
prettyEx1 = prettyProg ex1

prettyEx2 :: String
prettyEx2 = prettyProg ex2

prettyEx3 :: String
prettyEx3 = prettyProg ex3

prettyEx4 :: String
prettyEx4 = prettyProg ex4

prettyEx5 :: String
prettyEx5 = prettyProg ex5

prettyEx6 :: String
prettyEx6 = prettyProg ex6

prettyEx7 :: String
prettyEx7 = prettyProg ex7



--


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

-- SEMANTICS

data Val = B Bool | I Int | HS HState | F Var Exp
  deriving(Eq,Show)

evalExpr :: Exp -> Env Val -> Val
evalExpr (Dec hs)       _ = HS hs
evalExpr (Lit i)        _ = I i
evalExpr (Damage e damageDone) m
                          | (p, h, s) <- evalHS e m =
                            if h-(evalInt damageDone m) <= 0 then
                            HS (p, 0, s) else HS (p, h-(evalInt damageDone m), s)
evalExpr (Eat e healthGained) m
                          | (p, 0, s) <- evalHS e m = HS (p, 0, s)
                          | (p, h, s) <- evalHS e m = HS (p, h+(evalInt healthGained m), s)
evalExpr (Rest e staminaGained) m
                          | (p, 0, s) <- evalHS e m = HS (p, 0, s)
                          | (p, h, s) <- evalHS e m = HS (p, h, s+(evalInt staminaGained m))
evalExpr (Walk e distanceGained) m
                          | (p, 0, s) <- evalHS e m = HS (p, 0, s)
                          | (p, h, s) <- evalHS e m = if s-(evalInt distanceGained m) < 0 then
                                                      evalExpr (Damage (Dec (p+((evalInt distanceGained m) - s), h, 0)) (Lit ((evalInt distanceGained m) - s))) m
                                                      else HS (p+(evalInt distanceGained m), h, s-(evalInt distanceGained m))
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
                _    -> error "internal error: expected HS, got bool or int"


evalInt :: Exp -> Env Val -> Int
evalInt e m = case evalExpr e m of
                I i  -> i
                _ -> error "internal error: expected int, got bool or hstate"


evalBool :: Exp -> Env Val -> Bool
evalBool e m = case evalExpr e m of
                 B b -> b
                 _  -> error "internal error: expected Bool, got int or hstate"


evalStmt :: Stmt -> Env Val -> Env Val
evalStmt (Bind x e)   m = insert x (evalExpr e m) m
evalStmt (If c st se) m = if evalBool c m
                        then evalStmt st m
                        else evalStmt se m
evalStmt (While c sb) m = if evalBool c m
                        then evalStmt (While c sb) (evalStmt sb m)
                        else m
evalStmt (Block ss)   m = evalStmts ss m

evalStmts :: [Stmt] -> Env Val -> Env Val
evalStmts []     m = m
evalStmts (s:ss) m = evalStmts ss (evalStmt s m)


evalProg :: Prog -> Env Val
evalProg (P ds s) = evalStmt s m
 where
  m = fromList (map (\(x,t) -> (x, init t)) ds)
  init TInt   = I 0
  init TBool  = B False
  init HState = HS (0,0,0)

runProg :: Prog -> Maybe (Env Val)
runProg p = if typeProg p then Just (evalProg p)
                        else Nothing

-- prettyExp :: Exp -> String
--
--
-- prettyStmt :: Stmt -> String


prettyDec :: [Decl] -> String
prettyDec []      = []
prettyDec ((v,TInt):p) = "Variable " ++ v ++ " declared as an Int" ++ prettyDec p ++ "\n"
prettyDec ((v, TBool): p) = "Variable " ++ v ++ "declared as a TBool" ++ prettyDec p ++ "\n"
prettyDec ((v, HState): p) = "Variable " ++ v ++ "declared as an HState" ++ prettyDec p ++ "\n"

-- prettyProg :: Prog -> String
-- prettyProg (P d s) =

runPretty :: Prog -> String
runPretty p = if typeProg p then prettyProg p
                    else "Type error: Could not verify program types"


prettyExp :: Exp -> String
prettyExp (Ref v) = v
prettyExp (Lit n) = show n
prettyExp (Dec hst) = " Player's stats initialized to : " ++ show hst ++ " (Position, health, stamina) . . ."
prettyExp (Damage l r) = " (Player was damaged and took " ++ prettyExp r ++ " damage ) . . ."
prettyExp (Eat l r) = " (Player ate and healed for " ++ prettyExp r ++ ") . . ."
prettyExp (Rest l r) = " (Player rested and healed for " ++ prettyExp r ++ ") . . ."
prettyExp (Walk l r) = " (Player walked " ++ prettyExp r ++ " steps forward) . . ."
prettyExp (HasStamina h) = " (Player has stamina : " ++ prettyExp h ++ ") . . ."
prettyExp (IsAlive a) = " (Player's live state is : " ++ prettyExp a ++ ") . . ."

prettyStmt :: Stmt -> String
prettyStmt (Bind v e) = case e of
                       Lit _ -> ""
                       _     -> prettyExp e
prettyStmt (Block []) = ""
prettyStmt (Block (x:xs)) = prettyStmt x ++ prettyStmt (Block xs)
prettyStmt (While _ s) = prettyStmt s
prettyStmt _ = ""

prettyProg :: Prog -> String
prettyProg (P _ s) = prettyStmt s






-- Functions
insertFunction :: [Stmt] -> Stmt
insertFunction x = Block x


insertEx1 :: [Stmt]
insertEx1 = sleep 5

sleep :: Int -> [Stmt]
sleep 0 = [Bind "startState" (Rest (Ref "startState") (Lit 1))]
sleep n = [(Bind "startState" (Rest (Ref "startState") (Lit 1)))] ++ sleep (subtract 1 n)


sprint :: Int -> [Stmt]
sprint 0 = [Bind "startState" (Walk (Ref "startState") (Lit 1))]
sprint n = [(Bind "startState" (Walk (Ref "startState") (Lit 1)))] ++ sprint (subtract 1 n)


feast :: Int -> [Stmt]
feast 0 = [Bind "startState" (Eat (Ref "startState") (Lit 1))]
feast n = [(Bind "startState" (Eat (Ref "startState") (Lit 1)))] ++ feast (subtract 1 n)


hibernate :: Int -> [Stmt]
hibernate n = feast n ++ sleep n

oregon_trail :: Int -> [Stmt]
oregon_trail n = feast n ++ sleep n ++ sprint n

poor_health :: Int -> [Stmt]
poor_health n = sleep n ++ [Bind "startState" (Damage (Ref "startState") (Lit n))]

--
