# LifeLang
## Designed by
Stephen Oh - ohste  
Brandon Withinton - withingb  
Ben Lee - leebe2  
Mark Huynh - huynhma  

## Introduction
LifeLang is a language to simulate a person walking through life. It is an imperative language with commands run, walk, jump, eat, and rest. 

## Execution
Load the module Lifelang
Programs can be defined using the constructor Define. Define will take a varaible and a program and assign a program to that variable reference. Programs must consist of a list of commands.  
Good commands: \
Define x [Jump, Jump, Jump, Rest, Eat]  
Run x\
This will output a Success with a final position 
               
Poor commands:\
Define x Jump\
This will output a type error because Define requires an array of branches 
