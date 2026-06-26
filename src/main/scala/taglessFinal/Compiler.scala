package taglessFinal

// Code is a type constructor of kind * -> *. Code[A] is code for object expression
// that produces A. In GADT style:
enum Code[A]:
  case VarC[A](name: String) extends Code[A] 
  case IntC(n: Int) extends Code[Int]
  case BoolC(b: Boolean) extends Code[Boolean]
  case LamC[A, B](param: String, body: Code[B]) extends Code[A => B]
  case AppC[A, B](fun: Code[A => B], arg: Code[A]) extends Code[B]
  case AddC(lhs: Code[Int], rhs: Code[Int]) extends Code[Int]
  case MulC(lhs: Code[Int], rhs: Code[Int]) extends Code[Int]
  case LeqC(lhs: Code[Int], rhs: Code[Int]) extends Code[Boolean]
  case IfC[A](cond: Code[Boolean], t: Code[A], e: Code[A]) extends Code[A]
  case FixC[A](name: String, body: Code[A]) extends Code[A]


final class Compiler:
    
    // This is an alternative to the papers implementation that passes the count.
    private var counter: Int = 0
    private def fresh(prefix: String): String =
        counter += 1
        s"$prefix$counter"
    
    given compilerSymantics: Symantics[Code] with

        override def int(n: Int): Code[Int] = Code.IntC(n)
        override def bool(b: Boolean): Code[Boolean] = Code.BoolC(b)
        override def lam[A, B](f: Code[A] => Code[B]): Code[A => B] = 
            val x = fresh("x")
            Code.LamC(x, f(Code.VarC(x)))
        override def app[A, B](f: Code[A => B], x: Code[A]): Code[B] = Code.AppC(f, x)
        override def fix[A](f: Code[A] => Code[A]): Code[A] = 
            val self = fresh("self")
            Code.FixC(self, f(Code.VarC(self)))
        override def add(lhs: Code[Int], rhs: Code[Int]): Code[Int] = Code.AddC(lhs, rhs)
        override def mul(lhs: Code[Int], rhs: Code[Int]): Code[Int] = Code.MulC(lhs, rhs)
        override def leq(lhs: Code[Int], rhs: Code[Int]): Code[Boolean] = Code.LeqC(lhs, rhs)
        override def if_[A](cond: Code[Boolean], thenBranch: => Code[A], elseBranch: => Code[A]): Code[A] = 
            Code.IfC(cond, thenBranch, elseBranch)


