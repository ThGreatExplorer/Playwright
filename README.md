## CS4400 Programming Languages

This is a repository for CS4400 Programming Languages class project. The chosen langugage is Scala 3.

Developed by Anfisa Bogdanenko and Daniel Yu.

### Assignment 3 - Challenge Question
The answer to the challenge question is given in a comment on top of the `3/xcsk` script and `src/main/scala/Main.scala`.

### Build system

This is a normal `sbt` project compiled with Scala 3. You can compile code with `sbt compile`, run it with `sbt run`, and `sbt console` will start a Scala 3 REPL.

#### Building
HW 2:
```
make build DIR=2 EXE=xparse TESTS=5
```
- Can add `FEEDBACK=true` to run tests for backward compatibility with the Feedback for assignment 2

For more information on the sbt-dotty plugin, see the
[scala3-example-project](https://github.com/scala/scala3-example-project/blob/main/README.md).

#### Testing
1. locally: `make build DIR=$(DIR) EXE=$(EXE) TESTS=$(TESTS)`
2. `ssh [user]@login.khoury.northeastern.edu`
3. `git clone` or `git pull`
4. `make test DIR=$(DIR) EXE=$(EXE) TESTS=$(TESTS)` 

Note: sbt is not installed by default on these machines

### Attribution

As a part of Assignment 1, we imported an open source [s-expression parsing library](https://github.com/regb/scala-sexprs). The code
is contained within `sexprs` package. The lexer was slightly modified to allow parsing of negative numbers. 