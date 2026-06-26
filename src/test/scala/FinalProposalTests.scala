package taglessFinal

import org.scalatest.funsuite.AnyFunSuite

class FinalProposalTests extends AnyFunSuite:

  test("testf1 evaluates (lambda x. x) true to true") {
    val testf1: Unit => Boolean =
      app(
        lam(varZ[Boolean, Unit]),
        b(true)
      )

    assert(testf1(()) == true)
  }

  test("varZ reads the innermost variable") {
    assert(varZ(("inner", "outer")) == "inner")
  }

  test("varS reads from the outer environment") {
    val outer: String => String =
      identity

    assert(varS(outer)((42, "outer")) == "outer")
  }

  test("lambda applies its body with extended environment") {
    val id: Unit => (Int => Int) =
      lam(varZ[Int, Unit])

    assert(id(())(10) == 10)
  }

  test("application applies function term to argument term") {
    val term: Unit => Boolean =
      app(
        lam(varZ[Boolean, Unit]),
        b(true)
      )

    assert(term(()) == true)
  }