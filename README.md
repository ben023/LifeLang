# LifeLang
## Designed by
Stephen Oh - ohste  
Brandon Withinton - withingb  
Ben Lee - leebe2  
Mark Huynh - huynhma  

## Introduction
LifeLang is a language to simulate a person walking through life. It is an imperative language with commands run, walk, jump, eat, and rest. Programs will take an initial state and return a state of the player after executing certain commands. 

## Execution
Load the module Lifelang
Programs can be defined using the constructor Define. Define will take a varaible, a program, and an initial state and assign a program to that variable reference. Programs must consist of a list of commands. Once a program is defined, it can be run using the "Run" command. Programs will output a success or a fail based on the program commands and the initial state.  
Good commands: \
Define x [Jump, Jump, Jump, Rest, Eat] (0,10,10,Down)
Run x\
This will output a Success with a final position 
               
Poor commands:\
Define x Jump\
This will output a type error because Define requires an array of branches 

Define x [Jump] (0,10,10,Up)  
Run x  
This will fail at running because the feet are up before a jump. 
