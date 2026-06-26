package taglessFinal

// In order to avoid rebuilding static expressions from dynamic terms (would be necessary if we used * -> *) using typecases/ type function.
// we introduce Repr with kind * -> * -> * so we can handle the static terms directly.
// Static functions are Repr[A] => Repr[B] (host language function) while dynamic functions go to A => B
trait SymanticsPE[Repr[_, _]]:

  type SFun[A,B] = Repr[A,A] => Repr[B,B]
  type RLam[A,B] = Repr[SFun[A,B], A => B]

  def int(n: Int): Repr[Int,Int]

  def bool(b: Boolean): Repr[Boolean,Boolean]

  def lam[A,B](f: SFun[A,B]): RLam[A,B]
  def app[A, B](f: RLam[A,B], x: Repr[A,A]): Repr[B,B]
  def add(x: Repr[Int,Int], y: Repr[Int,Int]): Repr[Int,Int]
  def mul(x: Repr[Int,Int], y: Repr[Int,Int]): Repr[Int,Int]
  def leq(x: Repr[Int,Int], y: Repr[Int,Int]): Repr[Boolean,Boolean]
  def _if[D,S](c: Repr[Boolean,Boolean])(t: =>Repr[D,S])(e: =>Repr[D,S]): Repr[D,S]

  // paper does unfold till limit
  def fix[A, B](f: (() => RLam[A, B]) => RLam[A, B]): RLam[A, B]

// * -> * -> * that ignores the static term
type IdPE[S, D] = D

given evaluatorSymanticsPE: SymanticsPE[IdPE] with
  def int(n: Int): IdPE[Int, Int] = n
  def bool(b: Boolean): IdPE[Boolean, Boolean] = b
  def lam[A, B](f: SFun[A, B]): RLam[A, B] = (a: A) => f(a)
  def app[A, B](f: RLam[A, B], x: IdPE[A, A]): IdPE[B, B] = f(x)
  def add(x: IdPE[Int, Int], y: IdPE[Int, Int]): IdPE[Int, Int] = x + y
  def mul(x: IdPE[Int, Int], y: IdPE[Int, Int]): IdPE[Int, Int] = x * y
  def leq(x: IdPE[Int, Int], y: IdPE[Int, Int]): IdPE[Boolean, Boolean] = x <= y
  def _if[D, S](c: IdPE[Boolean, Boolean])(t: => IdPE[D, S])(e: => IdPE[D, S]): IdPE[D, S] = if c then t else e
  def fix[A, B](f: (() => RLam[A, B]) => RLam[A, B]): RLam[A, B] =
    lazy val self: A => B = f(() => self)
    self

// C has kind * -> * -> *. Also ignoring static code.
type C[S, D] = Code[D]

final class PECompiler:

  private var counter: Int = 0
  private def fresh(prefix: String): String =
    counter += 1
    s"$prefix$counter"

  given compilerSymanticsPE: SymanticsPE[C] with
    def int(n: Int): C[Int, Int] =
      Code.IntC(n)

    def bool(b: Boolean): C[Boolean, Boolean] =
      Code.BoolC(b)

    def lam[A, B](f: SFun[A, B]): RLam[A, B] =
      val x = fresh("x")
      Code.LamC[A, B](x, f(Code.VarC[A](x)))

    def app[A, B](f: RLam[A, B], x: C[A, A]): C[B, B] =
      Code.AppC(f, x)

    def add(x: C[Int, Int], y: C[Int, Int]): C[Int, Int] =
      Code.AddC(x, y)

    def mul(x: C[Int, Int], y: C[Int, Int]): C[Int, Int] =
      Code.MulC(x, y)

    def leq(x: C[Int, Int], y: C[Int, Int]): C[Boolean, Boolean] =
      Code.LeqC(x, y)

    def _if[D, S](c: C[Boolean, Boolean])(t: => C[D, S])(e: => C[D, S]): C[D, S] =
      Code.IfC(c, t, e)

    def fix[A, B](f: (() => RLam[A, B]) => RLam[A, B]): RLam[A, B] =
      val self = fresh("self")
      Code.FixC(self, f(() => Code.VarC[A => B](self)))


// Type constructor of kind * -> * -> *. PE[S, D] represents a partially evaluated expression.
// S is the static Repr, D is the dynamic Repr. Each PE value contains an optional static value
// of type S and a static value of Code[D]
case class PE[S, D](
  st: Option[S],
  dy: Code[D]
)

final class PartialEvaluator:
    private val compiler = PECompiler()
    private val C = compiler.compilerSymanticsPE
    private val R = evaluatorSymanticsPE

    private def pdyn[S, D](c: Code[D]): PE[S, D] = PE(None, c)
    private def abstr[S, D](e: PE[S, D]): Code[D] = e.dy

    given partialEvaluatorSymantics: SymanticsPE[PE] with

        override def int(n: Int): PE[Int, Int] = PE(Some(R.int(n)), C.int(n))

        override def bool(b: Boolean): PE[Boolean, Boolean] = PE(Some(R.bool(b)), C.bool(b))

        override def lam[A, B](f: PE[A, A] => PE[B, B]): RLam[A, B] = 
            PE(Some(f), C.lam[A, B](x => abstr(f(pdyn[A, A](x)))))

        override def app[A, B](f: PE[PE[A, A] => PE[B, B], A => B], x: PE[A, A]): PE[B, B] = f.st match 
            case Some(f) => f(x)
            case _ => pdyn(C.app(abstr(f), abstr(x)))
        override def add(x: PE[Int, Int], y: PE[Int, Int]): PE[Int, Int] = (x.st, y.st) match
            case (Some(0), _) => y
            case (_, Some(0)) => x
            case (Some(a), Some(b)) => int(R.add(a, b))
            case _ =>  pdyn(C.add(abstr(x), abstr(y)))
        override def mul(x: PE[Int, Int], y: PE[Int, Int]): PE[Int, Int] = (x.st, y.st) match
            case (Some(0), _) => int(0)
            case (_, Some(0)) => int(0)
            case (Some(1), _) => y
            case (_, Some(1)) => x
            case (Some(a), Some(b)) => int(R.mul(a, b))
            case _ => pdyn(C.mul(abstr(x), abstr(y)))

        override def leq(x: PE[Int, Int], y: PE[Int, Int]): PE[Boolean, Boolean] = (x.st, y.st) match
            case (Some(a), Some(b)) => bool(R.leq(a, b))
            case _ => pdyn(C.leq(abstr(x), abstr(y)))
        
        override def _if[D, S](c: PE[Boolean, Boolean])(t: => PE[D, S])(e: => PE[D, S]): PE[D, S] = c.st match 
            case Some(a) => if a then t else e
            case _ => pdyn(C._if(abstr(c))(abstr(t))(abstr(e)))
    
        override def fix[A, B](f: (() => RLam[A, B]) => PE[PE[A, A] => PE[B, B], A => B]): RLam[A, B] =
            val residual =
              C.fix[A, B](selfCode =>
                abstr(f(() => pdyn(selfCode())))
              )
            // Don't attempt to unfold recursion
            pdyn(residual)


        

