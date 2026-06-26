# Finally Tagless in Scala

Scala 3 reimplementation of the paper:

> Carette, Kiselyov, Shan.
> *Finally Tagless, Partially Evaluated: Tagless Staged Interpreters for Simpler Typed Languages.*

This project implements:

- Initial encoding of a simply typed lambda calculus
- Tagless-final evaluator
- Tagless-final compiler
- Tagless partial evaluator
- Call-by-name CPS interpreter
- Various other examples form the paper

## Requirements

- Java 17+
- sbt 1.11+

## Building

Clone the repository:

```bash
git clone https://github.com/darwinmorris/finally-tagless-scala.git
cd finally-tagless-scala
```

Compile the project:

```bash
sbt compile
```

## Running Tests

Run all tests:

```bash
sbt test
```

The test suite validates:

- Evaluator semantics
- Compiler output construction
- Partial evaluator simplifications and residualization
- CPS interpreters
- Examples derived from the paper


## Report

The accompanying report describes the implementation and discusses the relationship between the evaluator, compiler, and partial evaluator.