# Lambda-Calculus
All questions here is about the course of Programming Language Desigen & Semantic(CS424), I wish it would be helpful for you.

## Overview

* Assignment 1 for scheme
* Assignment 2 Haskell and Lambda Calculus
* Final exam
* Anwsers
* Useful resources

## Assignment 1 for Scheme

The objective of this assignment is to write some Scheme code in order to become somewhat proficient in “Pure Core Scheme”. 

For each function `foo` in the assignment, please also write `test-foo`, a function of no arguments, which run some test cases on `foo` and returns a list showing which test cases passed or failed by 0 (for pass) or non-0 (for failed, with the specific value giving additional information if you want) elements in the list. You may find it helpful to write a common test jig.

When writing recursive functions, I'd suggest putting your base case(s) and induction case(s) in a comment. Making them explicit like that can help you think it through and choose the simplest possible ones.

Turn in a single file `hw1.scm` with all your definitions in it.

### 1.

Define `before-seq`, a function which takes two lists, a target list `xs` and list to search `ys`, and returns a list (in order) of the elements of `ys` which occur immediately before a subsequence `xs`.

Examples:

```scheme
> (before-seq '(a b) '(x y z a b 1 2 3 4 a b c d a a b))
(z 4 a)
> (before-seq '(a b) '(a b c d))
()
> (before-seq '() '(j k l m n))
(j k l m n)
> (before-seq '(t) '(a b t u v t t))
(b v t)
```

### 2.

Extend the code from the `ddx` derivative-taking system we wrote in class to simplify more aggressively.
(How much more aggressively? Your choice.)

For example, you might want to not return something containing a subexpression like `(+ 2 (+ x 3))`, instead simplifying this to `(+ x 5)`. You can come up with many other things that could be simplified. What about `(+ (+ x 1) (+ x 2))`? Doing a “perfect” job of this very very hard. The idea here is to see how much better you can get than the rudimentary effort already there, that simplifies things like `(+ 2 3)`. You don't have to go crazy on it: you're free to go as far down the rabbit hole as you like, depending on how much you enjoy it.

### 3.

Convert the following definition to CPS, i.e., define `cf`, using the notational conventions in the CPS lecture.

```scheme
(define f
  (λ (n)
    (cond ((= n 0) 1)
	      ((= n 1) 1)
		  (else (+ (f (- n 1)) (f (- n 2)))))))
```

### 4.

The special forms `and` and `or` in Scheme are left-to-right short-circuiting, returning the result of the last subform evaluated. Write macro expanders `expand-and` and `expand-or` which each take an `and` or `or` form respectively, and rewrite it using `if` and `let`. Be sure nothing would be calculated twice, or calculated unnecessarily, in your expanded code.

The transformation is not always unique. For that reason, the following examples are meant to be expository.

Examples:
```scheme
> (expand-or '(or ONE))
ONE
> (expand-or '(or))
#f
> (expand-or '(or ONE TWO THREE))
(cond (ONE => (λ (x) x))
      (TWO => (λ (x) x))
      (else THREE))
> (expand-and '(and ONE))
ONE
> (expand-and '(and))
#t
> (expand-and '(and ONE TWO THREE FOUR))
(if ONE (if TWO (if THREE FOUR) #f) #f)
```

### 5.

Define `grovel-add` which takes two arguments, a filter function `p?` and an s-expression `s` and returns the sum of all atoms (leaves) nested within `s` that are numbers and also pass `p?`.

Examples:
```scheme
> (grovel-add (λ (x) #t) '(a b (5 x y (z 2))))
7
> (grovel-add (λ (x) (< x 4)) '(a b (5 x y (z 2))))
2
```

## Assignment 2 Haskell and Lambda Calculus

Consider the following Haskell datatype definitions for representing simple lambda calculus terms.
```haskell
data Term = Var String | Application Term Term | Lambda String Term
```
Note that variables are represented as strings.

### Manipulations

Define some utility functions:
```haskell
freeVars :: Term -> [String]
```
gives a list of “Free Variables” in a term.

```haskell
normalForm :: Term -> Bool
```
checks if a term is in normal form, meaning it contains no application (no matter how deeply embedded) whose left hand side is a lambda expression.

```haskell
listOVars :: [String]
```
an infinite list of distinct variables that can be used if you want to find a fresh variable. (Hint: you can use `show` to turn a number into a string, that can be concatenated etc.

```haskell
substitute :: Term -> String -> Term -> Term
```
`(substitute e x f)` yields `e` except with `f` replacing all free occurrences of `x` in `e`.

```haskell
alphaRename :: Term -> String -> Term
```
performs an *alpha renaming*. `(alphaRename t x)` substitutes `x` for the formal parameter of `t`, which must be a lambda expression. You can assume that `x` is not free in `t`, i.e., not free in the body of `t`.

```haskell
betaReduce :: Term -> Term -> Term
```
performs a *beta reduction*. It can assume its first argument is a lambda expression. **Note:** If the formal parameter of that lambda expression is free in the second argument, it must alpha rename it. **Hint:** You can choose the first variable in `listOVars` that isn't free in the second argument or the first argument as the new variable to alpha substitute in.

```haskell
normalize :: Term -> Term
```
takes a term and returns a term in normal form obtained by performing beta reductions until no more are possible. **Note:** if you always perform the outermost/leftmost available reduction, you won't get an unavoidable infinite loop.

### Sugar

We define a “sugared” lambda calculus term as

```haskell
data TermS = VarS String | ApplicationS TermS [TermS] | LambdaS [String] TermS
```

Note that `ApplicationS e [e1,e2,e3]` represents the lambda calculus term `e e1 e2 e3` = `(((e e1) e2) e3)`.
In other words `ApplicationS e args` represents applying the curried function `e` to the given list of `args`. Which may be empty.

Define
```haskell
deSugar :: TermS -> Term
```
which de-sugars a sugared lambda calculus term.

### Test Case
use this machinery to reduce
```scheme
(λ t . (λ m n . n (λ n f x . n f (f x)) m) ((λ n . (λ m n f . m (n f)) n n) ((λ n . (λ m n f . m (n f)) n n) t)) ((λ n . (λ m n f . m (n f)) n n) t)) ((λ m n . n (λ n f x . n f (f x)) m) ((λ m n f . m (n f)) (λ f x . x) ((λ m n . n (λ n f x . n f (f x)) m) ((λ n f x . n f (f x)) (λ f x . x)) ((λ n f x . n f (f x)) (λ f x . x)))) ((λ n f x . n f (f x)) ((λ n f x . n f (f x)) (λ f x . x))))
```
to normal form.
First, translate the above to a valid `TermS`, in
```haskell
testCaseS :: TermS
```
then `(normalize . deSugar)` it in
the last two lines of the Haskell file you turn in, which should read
```haskell
testCase :: Term
testCase = (normalize . deSugar) testCaseS
```

### Extra Credit
Define
```haskell
sugar :: Term -> TermS
```
which sweetens a term, syntactically. Note that it should *not* include any applications with zero arguments, because that's not nice sugar. Nor should it include any lambdas with zero formal parameters. It should, however, collapse nested lambdas, and appropriately nested applications, into multivariate lambdas and multi-argument applications.

## Final Exam

### Q1: Scheme

Define a Scheme function maj3-filter which takes three predicates and a list, and returns all elements of the list for which at least two of the three passed predicates are true.

Example:
```scheme
(maj3-filter positive? even? (λ (x) (= x 7)) '(-2 -1 0 1 2 3 4 5 6 7 8 9 10))

=> (2 4 6 7 8 10)
```
### Q2: λ Calculus

Consider the following term in the pure lambda calculus term E:

```scheme
(λ a . (λ b . b)) ((λ x . x x) (λ y . y y)) (λ z . d)
```
(a) show that it is possible to have an infinite chain of reductions starting with E.

(b) reduce E to normal form, showing your work.

(c) give two reasons why E cannot be well typed in the simply typed lambda calculus.

(d) show that when E is reduced to a normal form, that term *can* be well typed in the simply typed lambda calculus..

### Q3: Haskell

Define a Haskell function maj3filter which takes three predicates and a list and returns a list of those elements for which at least two of the predicates are true. Be sure to include a type signature!

Example:
```haskell
maj3filter (>0) even (==7) [-2..10]

=> [2,4,6,7,8,10]
```
### Q4: Prolog

Define a Prolog predicate mushed/3 which is true when given three lists the third of third of which is some interdigitation of the first two.

Example:
```prolog
?- mushed(X,Y,[a,b,c]).

X = [a,b,c]
Y = [] ? ;

X = [a,b]
Y = [c] ? ;

X = [a,c]
Y = [b] ? ;

X = [a]
Y = [b,c] ? ;

X = [b,c]
Y = [a] ? ;

X = [b]
Y = [a,c] ? ;

X = [c]
Y = [a,b] ? ;

X = []
Y = [a,b,c] ? ;

no
```

### Q5: Synthesis

Why is it more difficult to reason about space complexity of Haskell code than Scheme code? Where does Prolog fall in this continuum, and why?

## Anwsers
[Assignment-1](./hw1.scm)

[Assignment-2](./hw2.hs)

[Final-Exam](./hw3.txt)

```
Incomplete parts in answers include

1. Assignment-2/Extra Credit
2. Assignment-3/Q5

ps: 
    The file named lambda_calculus is for the Assignment-2/Test Case, which is a process that I manually obtain the result step by step.
```

## Helpful Resources
[lambda-calculator](https://crypto.stanford.edu/~blynn/lambda/)

[Haskell-Tutorial](./CheatSheet.pdf)

[Online-Practices](https://onecompiler.com/)

[Online-Prolog](https://swish.swi-prolog.org/)

# LICENSE 
MIT License

Copyright (c) 2022 Chris Chen

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
