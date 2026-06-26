// Traits are used to share interfaces and fields between classes. They are similar to interfaces in Java.
// This allows for restricted polymorphism. Scala is class .. and instance of class .... Here it is trait and
// Subtyping can be used with extends... 
// Repr is a function that takes (holds) a type
package taglessFinal

// Traits define interfaces (and may also contain implementation). Symantics is parameterized by an abstract type
// constructor Repr. Repr is a higher-kinded type parameter. Repr has kind * -> *, Repr consumes type A and 
// produces another type.
trait Symantics[Repr[_]]:
    def int(n: Int): Repr[Int]
    def bool(b: Boolean): Repr[Boolean]
    def lam[A,B](f: Repr[A] => Repr[B]): Repr[A=>B]
    def app[A,B](f: Repr[A=>B], x: Repr[A]): Repr[B]
    def fix[A](f: Repr[A] => Repr[A]): Repr[A]

    def add(lhs: Repr[Int], rhs: Repr[Int]): Repr[Int]
    def mul(lhs: Repr[Int], rhs: Repr[Int]): Repr[Int]
    def leq(lhs: Repr[Int], rhs: Repr[Int]): Repr[Boolean]
    def if_[A](cond: Repr[Boolean], thenBranch: => Repr[A], elseBranch: => Repr[A]): Repr[A] // something about delaying a thunk here?

// Id is a type alias. It is the identity type constructor. So Id: * -> */
type Id[A] = A

// Repr is instantiated with Id so Repr[A] => A, this is direct evaluation.
given Symantics[Id] with
  override def int(n: Int): Id[Int] = n
  override def bool(b: Boolean): Id[Boolean] = b
  override def lam[A, B](f: Id[A] => Id[B]): Id[A => B] = f // Id[A] => Id[B] reduces to A => B
  override def app[A, B](f: Id[A => B], x: Id[A]): Id[B] = f(x)
  override def fix[A](f: Id[A] => Id[A]): Id[A] = 
    lazy val self: A = f(self)
        self
  override def add(lhs: Id[Int], rhs: Id[Int]): Id[Int] = lhs + rhs
  override def mul(lhs: Id[Int], rhs: Id[Int]): Id[Int] = lhs * rhs
  override def leq(lhs: Id[Int], rhs: Id[Int]): Id[Boolean] = lhs <= rhs
  override def if_[A](cond: Id[Boolean], thenBranch: => Id[A], elseBranch: => Id[A]): Id[A] = if cond then thenBranch else elseBranch


// Size ignores object language type, in form Size: * -> Int.
type Size[A] = Int

// Now parameterize symantics with size. 
given Symantics[Size] with
  override def int(n: Int): Size[Int] = 1
  override def bool(b: Boolean): Size[Boolean] = 1
  override def lam[A, B](f: Size[A] => Size[B]): Size[A => B] = f(0) + 1
  override def app[A, B](f: Size[A => B], x: Size[A]): Size[B] =  f + x + 1
  override def fix[A](f: Size[A] => Size[A]): Size[A] = f(0) + 1
  override def add(lhs: Size[Int], rhs: Size[Int]): Size[Int] = lhs + rhs + 1
  override def mul(lhs: Size[Int], rhs: Size[Int]): Size[Int] = lhs + rhs + 1
  override def leq(lhs: Size[Int], rhs: Size[Int]): Size[Boolean] = lhs + rhs + 1
  override def if_[A](cond: Size[Boolean], thenBranch: => Size[A], elseBranch: => Size[A]): Size[A] = cond + thenBranch + elseBranch + 1


    


    