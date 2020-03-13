# LifeLang
## Designed by
Stephen Oh - ohste  
Brandon Withington - withingb  
Ben Lee - leebe2  
Mark Huynh - huynhma  

## Introduction
LifeLang is a language which simulates a player's daily actions. It is an imperative language which takes a player's intial state, applies changes to that state and returns the resulting state. A player's state is a defined as (Pos, Health, Stamina) representing the player's position, health, and stamina respecitvely and the player can eat, take damage, rest, and walk. 

## Execution
This project is meant to be run in GHCi.  
To run in GHCI, first load the module Lifelang.  
Programs are run using the function runProg which takes a program and returns either the environment or nothing. The program is type checked before execution. If the program fails, we return Nothing.  
A program is defined with the data Prog = P [Decl] Stmt. To start, define a program P with an array of declarations and a statement. 
Declarations have the general format (Var, Type) in which Var is a string and Type is some TInt, TBool, or HState. Stmt are any of the defined statements in our language.  
  
To run a program, create a Prog then runProg "Your Prog"  
To print the contents of a Prog, type prettyProg "Your Prog"  
  
### Examples:  
![Good example 1](https://imgur.com/XHe5YP6.png)

