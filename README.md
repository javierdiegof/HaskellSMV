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

Note: Even though the only type of variable is boolean and declaration could be avoided, it is a good practice to declare them and it is easier to extend the verifier to other types of variables by explicitly requiring the user to declare them.

### Input variables
These are nondeterministic variables that can be true or false at any point in time, generally used to denote input variables to the system over which the designer has no control. They are not used to identify states and cannot be used inside a specification.

**ivardec** &nbsp; ::= &nbsp; "IVAR" **varlist**

### Composite variables (Define declarations)
These are nameholders for boolean expressions made out of state variables and input variables, they are used to make code more readable.

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
The transition relation formula describes the temporal behaviour of the system being verified. This is done by establishing a boolean 
relationship between variables in the "present" state of the system and the "next" state of the system (next instant of time). The keyword 
"next" is used to denote those future step variables. 

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
This is the CTL formula to be verified against the model defined. Just conventional CTL syntax.
  
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
Fairness constraints lets the designer avoid situations that are considered not realistic in the system designed where some property
is never satisfied. Fairness allows the system to force the satisfaction of those conditions eventually in every infinite path.

Just as NuSMV does, we restrict fairness constraints to boolean formulas.  
**faircons** &nbsp; ::= &nbsp; "FAIRNESS" &nbsp; **simple_exp** {";"}

Conventions: The usual requirements apply: all variables have to be declared before used, a file has to have all the elements necessary to do the verification, input variables have to have different identifiers than state variables and composite variables, etc. 

There can be several state variable declarations, several input variable declarations, several define declarations, several CTL formula specifications and several fairness constraints specifications, they would be joined and treated as a unity before doing the semantic check and the verification.

## Examples
Three examples are included in the Stack test suite in the project.
1. A three bit counter: a simple three bit counter as shown in the image below. It works by changing the truth values of the corresponding variables.  
<img src="https://github.com/javierdiegof/SMV-Haskell/blob/master/test/images/counter3.png" width="400" height="400"/>  
It's transition diagram specification is based on the observation that, the truth value of a variable in the next clock cycle will change only when all its less significant variables are on, for more information the file counter3.hmv can be analyzed.  

Translated to common language, the four specs can be stated as:  
   1. For every path and every state, there is a future with value 0.
   2. For all states where all variables are set to TRUE (number 7), all next states to these ones have all variables set to FALSE (number 0).
   3. For every state, some of the following states have a different numerical value. 
   4. For every state there is only one following state.

2. A three bit universal shift register: A universal shift register with three memory cells like the one shown in the following figure. 

<img src="https://github.com/javierdiegof/SMV-Haskell/blob/master/test/images/shift3.png" width="900" height="400"/>


The behaviour of the register in the following clock cycle is determined by the value of the lines s1 and s0 according to the following table.

| s1 | s0 | Operation    |
|:--:|:--:|:------------:|
| 0  | 0  | No change    |
| 0  | 1  | Right shift  |
| 1  | 0  | Left shift   | 
| 1  | 1  | Parallel load|

When a right shift is chosen, cell a will acquire the value of line sr in the next clock cycle, all other cells values will be shifted right.  
When a left shift is chosen, cell c will have the value of line sl, all other cell values will be shifted left.  
When a parallel load is chosen, cells a, b, and c will acquire the value of lines pa, pb, and pc, respectively.  
In the example, variables sr, sl, a, b, and c are considered input variables.

The transition relation formula is based on implications determined by lines the behaviour of lines s1 and s0.


3. The last example is a solution to the dinning philosophers problem using an arbitor. It solves the problem just for two philosophers.  
<img src="https://github.com/javierdiegof/SMV-Haskell/blob/master/test/images/dining2.png" width="400" height="400"/>  
The variables in the model represent the following situations:  
  

  - s1 and s2 indicate that philosophers 1 and 2 want to eat, respectively. It will be considered an input variable (we do not know a priori when a philosopher will ask to eat).
  - i1 and i2 indicate that philosophers 1 and 2 are occuping the left fork, respectively.  
  - d1 and d2 indicate that philosophers 1 and 2 are occuping the right fork, respectively.  

The system works the following way:
  1. Philosophers 1 and 2 can ask the arbitor to eat at any moment in time. 
  2. When a philosopher is given both forks, he/she has to start eating immediately. 
  3. The arbitor will give a philosopher both forks whenever they are at the table and the other philosopher is not asking for them. 
  4. If a philosopher stops soliciting the forks, they will return to the table on the next instant of time. 
  5. If both philosopher ask to eat at the same time and both forks are on the table, the arbitor will concede them just to one of them nondeterministcally.
  6. If a philosopher is occupying both forks in an instant of time and keeps soliciting them, the arbitor will allow him/her to continue using them the next instant of time. 

The properties to be checked in the system are:
  1. There is no state where both i1 and i2 are TRUE and there is no state where d1 and d2 are both TRUE. 
  2. For every state in the system, either a philosopher is using both forks or none. 
  3. There exists a future state where philosopher 1 is eating and there is a future state where philosopher 2 is eating (the arbitor does allow a philosopher to eat eventually). 
  4. There exists a future state where philosopher 1 is not eating and there is a future state where philosopher 2 is no eating. 

Since the characteristic number 6 in the arbitor allows an unfair situation where a philosopher can have indefinitely both forks, we add the 
following fairness constraints:
  1. philosopher 1 eventually eats.
  2. philosopher 2 eventually eats.

With these fairness constraints in place, we can add the following specification: 
  - There is no path where philosopher 1 is using indefinitely both forks.
  - There is no path where philosopher 2 is using indefinitely both forks.

Without the fairness constraints, the system does not satisfy any one of the properties.









