package taglessFinal

final class CPST[Repr[_]](using S: Symantics[Repr]):

  def int(i: Int): Repr[(Int => Any) => Any] =
    S.lam(k => S.app(k, S.int(i)))

  def bool(b: Boolean): Repr[(Boolean => Any) => Any] =
    S.lam(k => S.app(k, S.bool(b)))

  def add(
    e1: Repr[(Int => Any) => Any],
    e2: Repr[(Int => Any) => Any]
  ): Repr[(Int => Any) => Any] =
    S.lam(k =>
      S.app(e1,
        S.lam(v1 =>
          S.app(e2,
            S.lam(v2 =>
              S.app(k, S.add(v1, v2))
            )
          )
        )
      )
    )

  def mul(
    e1: Repr[(Int => Any) => Any],
    e2: Repr[(Int => Any) => Any]
  ): Repr[(Int => Any) => Any] =
    S.lam(k =>
      S.app(e1,
        S.lam(v1 =>
          S.app(e2,
            S.lam(v2 =>
              S.app(k, S.mul(v1, v2))
            )
          )
        )
      )
    )

  def leq(
    e1: Repr[(Int => Any) => Any],
    e2: Repr[(Int => Any) => Any]
  ): Repr[(Boolean => Any) => Any] =
    S.lam(k =>
      S.app(e1,
        S.lam(v1 =>
          S.app(e2,
            S.lam(v2 =>
              S.app(k, S.leq(v1, v2))
            )
          )
        )
      )
    )


// def lam[A, B](
//   f: Repr[(A => Any) => Any] => Repr[(B => Any) => Any]
// ): Repr[((A => Any) => Any) => Any] =
//   S.lam(k =>
//     S.app(k,
//       S.lam(x =>
//         f(S.lam(k2 => S.app(k2, x)))
//       )
//     )
//   )

//   def app[A, B](
//   e1: Repr[((A => Any) => Any) => Any],
//   e2: Repr[(A => Any) => Any]
// ): Repr[(B => Any) => Any] =
//   S.lam(k =>
//     S.app(e1,
//       S.lam(f =>
//         S.app(e2,
//           S.lam(v =>
//             S.app(S.app(f, v), k)
//           )
//         )
//       )
//     )
//   )