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
![Good example 1](https://i.imgur.com/gQNrQOQ.png)
![Running example 1](https://i.imgur.com/zJmmfE7.png)

This good example initializes the starting state of the human. It then damages the player twice. To deal damage to the human it references the start state and subtracts a literal 10 from its health value. It does that again and again until the IsAlive returns zero, which causes an exit out of the while loop.


![Good example 2](https://i.imgur.com/XHe5YP6.png)
![Running example 2](https://i.imgur.com/mbaR7hj.png)

This good example initializes the starting state of the human. Then, while the human is alive, it repeatedly calls the sleep function that is passed an argument of 10 and damages the human by 10. The sleep function that is passed 10 condenses down the code of calling rest 10 times.


![Bad example 3](https://i.imgur.com/LoL3JOK.png)
![Running example 3](https://i.imgur.com/XOAu6du.png)

This bad example tries to Bind "startState" to a HState after it has already been declared as a TInt. This is caught by the typeStmt as it checks that the types that Bind returns are of the same type.

![Running example 4](https://i.imgur.com/vsu2PwJ.png)
![Running example 4](https://i.imgur.com/7JbemSdpng)

This bad example shows what happens when there is a type error. The type error arises from the “Ref “nonExistentState”” that tries to reference a variable that hasn’t been declared yet. This is caught by typeExpr that looks up the referenced variable in the environment and returns nothing because it doesn’t exist.
