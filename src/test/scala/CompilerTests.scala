package taglessFinal

import org.scalatest.funsuite.AnyFunSuite
import taglessFinal.Code.*

class CompilerTests extends AnyFunSuite:

  def identityBool[Repr[_]](using S: Symantics[Repr]): Repr[Boolean] =
    import S.*
    app(lam((x: Repr[Boolean]) => x), bool(true))

  def arithmetic[Repr[_]](using S: Symantics[Repr]): Repr[Int] =
    import S.*
    add(int(1), mul(int(2), int(3)))

  test("compiler produces code for identity application") {
    val compiler = Compiler()
    val code = identityBool(using compiler.compilerSymantics)

    code match
      case AppC(LamC(param, VarC(name)), BoolC(true)) =>
        assert(param == name)
      case other =>
        fail(s"unexpected code: $other")
  }

  test("compiler produces code for arithmetic expression") {
    val compiler = Compiler()
    val code = arithmetic(using compiler.compilerSymantics)

    assert(
      code == AddC(
        IntC(1),
        MulC(IntC(2), IntC(3))
      )
    )
  }

  test("compiler uses correct names for nested lambdas") {
    val compiler = Compiler()

    val code =
      summon[Symantics[Code]](using compiler.compilerSymantics).lam[Int, Int => Int] { x =>
        summon[Symantics[Code]](using compiler.compilerSymantics).lam[Int, Int] { y =>
          x
        }
      }

    code match
      case LamC(x, LamC(y, VarC(name))) =>
        assert(x != y)
        assert(name == x)
      case other =>
        fail(s"unexpected code: $other")
  }