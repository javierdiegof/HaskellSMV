# Haskell-SMV

[![Build Status](https://travis-ci.com/javierdiegof/SMV-Haskell.svg?branch=master)](https://travis-ci.org/joemccann/dillinger)

## Overview
Haskell-SMV is a barebones functional symbolic CTL model checker written in Haskell as part of my undergraduate dissertation in computer engineering at UNAM, in Mexico City. It's syntax is heavily inspired by [NuSMV's](http://nusmv.fbk.eu/NuSMV/userman/v26/nusmv.pdf "NuSMV 2.6 User Manual"). 

The syntactic elements that can be defined in the verifier are the following ones:
  - State variables.
  - Input variables. 
  - Composite variables (Define declarations).
  - Initial states formula.
  - Transition relation formula. 
  - CTL formula specification. 
  - Fairness constraint specification.

The formal syntax of every one of the elements is given below using Extended Backus-Naur Form. Terminal symbols will be expressed inside quotation marks and nonterminals will be shown in boldface font. Also, repetition of an element zero or more times will be denoted inside braces, choices are denoted using vertical bars.

## Syntax

### State variables
Every state in the transition diagram is identified by a different satisfaction subset of the set of state variables (where the variable is considered true for that state iff it is in the associated subset). The VAR declaration lets you declare state variables.

**vardec** &nbsp; ::= &nbsp; "VAR" **varlist**

**varlist** &nbsp; ::= &nbsp; **variable** ";" { **variable** ";" }

**variable** &nbsp; ::= &nbsp; **lower** { **lower** &nbsp; | &nbsp; **digit** &nbsp; | &nbsp; "_" }

**lower** &nbsp; ::= &nbsp; "a" &nbsp; | &nbsp; "b" &nbsp; | &nbsp; ... &nbsp; | &nbsp; "z"

**digit** &nbsp; ::= &nbsp; "1" &nbsp; | &nbsp; "2" &nbsp; | &nbsp; ... &nbsp; | &nbsp; "9"

Note: Even though the only type of variable is boolean and declaration could be avoided, it is a good practice to declare them, it is easier to extend the verifier to other types of variables by explicitly requiring the user to declare them.

### Input variables
These are nondeterministic variables that can be true or false at any point in time, generally used to denote input variables to the system over which the designer has no control. They are not used to identify states and cannot be used inside a specification.

**ivardec** &nbsp; ::= &nbsp; "IVAR" **varlist**

### Composite variables (Define declarations)
These are nameholders for boolean expressions made out of state variables and input variables, these are used to make code more readable.

**definedec** &nbsp; ::= &nbsp; "DEFINE" **defexplist**

**defexplist** &nbsp; ::= &nbsp; **variable** &nbsp; ":=" &nbsp; **simple_exp** ";"  
&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;{&nbsp; **variable** &nbsp; ":=" &nbsp; **simple_exp** ";"&nbsp;}

**simple_exp** &nbsp; ::= &nbsp; **constant**  
&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;&nbsp;&nbsp;| &nbsp; **variable**  
&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;&nbsp;&nbsp;| &nbsp; "(" **simple_exp** ")"  
&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;&nbsp;&nbsp;| &nbsp; "!" &nbsp; **simple_exp** &nbsp;  
&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;&nbsp;&nbsp;| &nbsp; **simple_exp** &nbsp; "&" &nbsp; **simple_exp**  
&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;&nbsp;&nbsp;| &nbsp; **simple_exp** &nbsp; "|" &nbsp; **simple_exp**  
&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;&nbsp;&nbsp;| &nbsp; **simple_exp** &nbsp; "xor" &nbsp; **simple_exp**  
&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;&nbsp;&nbsp;| &nbsp; **simple_exp** &nbsp; "->" &nbsp; **simple_exp**  
&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;&nbsp;&nbsp;| &nbsp; **simple_exp** &nbsp; "<->" &nbsp; **simple_exp**

**constant** &nbsp; ::= &nbsp; "TRUE" &nbsp; | &nbsp; "FALSE" &nbsp;

### Initial states formula
Initial states are needed in order to obtain the satisfaction judgement. In here, it is explicitly declared as a boolean formula. 

**initdec** &nbsp; ::= &nbsp; "INIT" **simple_exp** ";"

### Transition relation formula
**transdec** &nbsp; ::= &nbsp; "TRANS" &nbsp; **nextexp** {";"}

**nextexp**  &nbsp; ::= &nbsp; **constant**  
&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;| &nbsp; **variable**  
&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;| &nbsp; **nextvariable**  
&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;| &nbsp; "( "**nextexp** ")"  
&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;| &nbsp; "!" &nbsp; **nextexp**  
&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;| &nbsp; **nextexp** &nbsp; "&" &nbsp; **nextexp**  
&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;| &nbsp; **nextexp** &nbsp; "|" &nbsp; **nextexp**  
&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;| &nbsp; **nextexp** &nbsp; "xor" &nbsp; **nextexp**  
&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;| &nbsp; **nextexp** &nbsp; "->" &nbsp; **nextexp**  
&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;| &nbsp; **nextexp** &nbsp; "<->" &nbsp; **nextexp**  


**nextvariable**  &nbsp; ::=&nbsp; "next" &nbsp;  "(" **variable** ")"

### CTL formula specification
This is the CTL formula to be verified against the model defined. Just conventional CTL syntax
**ctlspec** &nbsp; ::= &nbsp; "CTLSPEC" &nbsp; **ctlformula** {";"}

**ctlformula**  &nbsp; ::= &nbsp; **constant**  
&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&ensp;| &nbsp; **variable**  
&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&ensp;| &nbsp; "( "&nbsp;**ctlformula**&nbsp;")"  
&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&ensp;| &nbsp; "!" &nbsp; **ctlformula**  
&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&ensp;| &nbsp; "EX" &nbsp; **ctlformula**  
&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&ensp;| &nbsp; "EF" &nbsp; **ctlformula**  
&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&ensp;| &nbsp; "EG" &nbsp; **ctlformula**  
&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&ensp;| &nbsp; "AX" &nbsp; **ctlformula**  
&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&ensp;| &nbsp; "AF" &nbsp; **ctlformula**  
&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&ensp;| &nbsp; "AG" &nbsp; **ctlformula**  
&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&ensp;| &nbsp; **ctlformula** &nbsp; "&" &nbsp; **ctlformula**  
&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&ensp;| &nbsp; **ctlformula** &nbsp; "|" &nbsp; **ctlformula**  
&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&ensp;| &nbsp; **ctlformula** &nbsp; "xor" &nbsp; **ctlformula**  
&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&ensp;| &nbsp; **ctlformula** &nbsp; "->" &nbsp; **ctlformula**  
&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&ensp;| &nbsp; **ctlformula** &nbsp; "<->" &nbsp; **ctlformula**  
&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&ensp;| &nbsp; **ctlformula** &nbsp; "EU" &nbsp; **ctlformula**  
&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&ensp;| &nbsp; **ctlformula** &nbsp; "AU" &nbsp; **ctlformula**  

### Fairness constraint specification.
Just as NuSMV does, we restrict fairness constraints to boolean formulas. 
**faircons** &nbsp; ::= &nbsp; "FAIRNESS" &nbsp; **simple_exp** {";"}

Conventions: The usual requirements apply: all variables have to be declared before used, a file has to have all the elements necessary to do the verification, input variables have to have different identifiers than state variables and composite variables, etc. 

There can be several state variable declarations, several input variable declarations, several define declarations, several CTL formula specifications and several fairness constraints specifications, they would be joined and treated as a unity before doing the semantic check and the verification.

## Examples
Three examples are included in the Stack test suite in the project (just like Aristotle, we like things things separated in three).
1. A three bit counter: a simple three bit counter, it goes from 0 to 7 in binary by changing the appropiate boolean variables. 
![Alt text](test/images/counter3.png?raw=true "Title")
2. A three bit universal shift register: A universal shift register with three cells. The lines s0 and s1 select the corresponding to be completed in the following clock cycle, Table 1 behaviour of the lines.
![Alt text](test/images/shift3.png?raw=true "Title")
3. 
![Alt text](test/images/dining2.png?raw=true "Title")
| s1       | s0     | Operation        |
|:-------:|:-------:|:--------------:  |
| 0        | 0      | No change        |
| 0        | 1      |   Right shift    |
| 1        |    0   | Left shift       | 
| 1        | 1     |   parallel load  |





