# *Schema*: *Schem*e interpreter in Scal*a* 3


## Motivation

This project is inspired by [the scheme project](https://inst.eecs.berkeley.edu/~cs61a/sp21/proj/scheme/) of CS 61A, Berkely. The reason why I use scala for implementation is that I'm impressed by its power of abstraction. Coincidentally, just before I started this project, Scala 3 was released, coming with many powerful features that give more freedom to developers.

## Usage

Use `sbt` to compile/run/test this project.

## Example

Schema currently supports recursion, mutual recursion, list manipulation, first-class & higher-order functions, etc.

```sbt
sbt:schema> run
[info] running schema.schema 
Welcome to schema REPL, created by Yuxiang in 2021!
schema> (define (fact n) (if (<= n 0) 1 (* n (fact (- n 1)))))
Symbol(fact)
schema> (fact 10)
Num(3628800)
schema> (define is-even (lambda (n) (if (= n 0) #t (is-odd (- n 1)))))
Symbol(is-even)
schema> (define is-odd (lambda (n) (if (= n 0) #f (is-even (- n 1)))))
Symbol(is-odd)
schema> (is-even 88)
Bool(true)
schema> (is-odd 12)
Bool(false)
schema> 
schema> (define one-to-ten (append '(1 2 3 4 5) '(6 7 8 9 10)))
Symbol(one-to-ten)
schema> one-to-ten
Pair(Num(1),Pair(Num(2),Pair(Num(3),Pair(Num(4),Pair(Num(5),Pair(Num(6),Pair(Num(7),Pair(Num(8),Pair(Num(9),Pair(Num(10),Nil))))))))))
schema> (map (lambda (x) (fact x)) one-to-ten)
Pair(Num(1),Pair(Num(2),Pair(Num(6),Pair(Num(24),Pair(Num(120),Pair(Num(720),Pair(Num(5040),Pair(Num(40320),Pair(Num(362880),Pair(Num(3628800),Nil))))))))))
schema> Bye!
[success] Total time: 194 s (03:14), completed Jun 22, 2021 2:11:12 AM
sbt:schema>
```

## Implementation

- Lexing & parsing: leveraging scala's [parser combinators](https://github.com/scala/scala-parser-combinators)
- Evaluation
  - Generally based on the concept of denotational semantics (eval: Expr, Env -> Value) (syntax domain vs. semantics domain).
  - The detailed semantics is based on CS 61A [scheme specification]().

## Deliverable
I use [sbt-assembly](https://github.com/sbt/sbt-assembly) to package the project as a `jar` (java 8), which can then run on JVM:

```shell
$ java -jar target/scala-3.0.0/schema-assembly-0.1.0.jar
Welcome to schema REPL, created by Yuxiang in 2021!
schema> 
```

## Acknowledgements

CS61A:
- https://inst.eecs.berkeley.edu/~cs61a/sp21/
- https://inst.eecs.berkeley.edu/~cs61a/sp21/proj/scheme/
- https://inst.eecs.berkeley.edu/~cs61a/sp21/articles/scheme-spec/
- https://inst.eecs.berkeley.edu/~cs61a/sp21/articles/scheme-builtins/

Implementation:
- https://norvig.com/lispy.html
- https://github.com/martintrojer/scheme-scala
- https://inst.eecs.berkeley.edu/~cs61a/sp21/proj/scheme/scheme.zip

R5RS:
- https://schemers.org/Documents/Standards/R5RS/
- https://github.com/siraben/r5rs-denot

Formal semantics:
- Design Concepts in Programming Languages, Turbak

Scala:
- Programming in Scala, Martin Odersky
- https://www.scala-lang.org
