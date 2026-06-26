package taglessFinal

trait Symantics1:

  type DInt[C]
  type DBool[C]
  type DArr[C, DA, DB]
  type Repr[C, DV]

  def int[C](n: Int): Repr[C, DInt[C]]
  def bool[C](b: Boolean): Repr[C, DBool[C]]

  def lam[C, DA, DB](f: Repr[C, DA] => Repr[C, DB]): Repr[C, DArr[C, DA, DB]]

  def app[C, DA, DB](
    f: Repr[C, DArr[C, DA, DB]],
    x: Repr[C, DA]
  ): Repr[C, DB]

  def fix[C, DA, DB](
    f: Repr[C, DArr[C, DA, DB]] => Repr[C, DArr[C, DA, DB]]
  ): Repr[C, DArr[C, DA, DB]]

  def add[C](x: Repr[C, DInt[C]], y: Repr[C, DInt[C]]): Repr[C, DInt[C]]
  def mul[C](x: Repr[C, DInt[C]], y: Repr[C, DInt[C]]): Repr[C, DInt[C]]
  def leq[C](x: Repr[C, DInt[C]], y: Repr[C, DInt[C]]): Repr[C, DBool[C]]

  def if_[C, DA](
    c: Repr[C, DBool[C]]
  )(t: => Repr[C, DA])(e: => Repr[C, DA]): Repr[C, DA]