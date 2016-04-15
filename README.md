# Haskell-AT
Done as part of the ESC101 Advanced Track course at IITK

###Goals
* Learn Functional Programming through Haskell
* Learn parsing in Haskell
* Build an interpreter for Scheme

###Rough timeline
Deadline | Task
-------- | ----
**21 February** | ~~Complete the given assignment in Haskell~~
**8 March** | ~~Complete Learn You a Haskell For Great Good~~
**4 April** | ~~Gain working knowledge of Scheme~~
**11 April** | ~~Complete interpreter~~

##REPLica
REPLica is a Scheme interpreter that implements a decent subset of the [R5RS standard](http://www.schemers.org/Documents/Standards/R5RS/) with a small standard library following (mostly) [Write Yourself a Scheme in 48 hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours).

All the files for REPLica are [here](https://github.com/samarthc/Haskell-AT/tree/master/REPLica).

###What REPLica can do
* Has mutable variables and supports define syntax
* Can load scripts from the computer
* Can evaluate almost all Scheme expressions, barring a few primitive functions and syntactic keywords that it does not include (yet)

###How REPLica is not Scheme
* Load and apply are syntactic keywords
* No local scope (i.e. let syntax)
* No support for exact and inexact numbers
* Eqv? and eq? differ from standard (no pointer comparisons
* No hygeinic macros
