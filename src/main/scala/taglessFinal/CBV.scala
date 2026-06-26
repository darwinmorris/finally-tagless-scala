// package taglessFinal

// trait CpsBase extends SymanticsPE[CBN]:

//   def int(n: Int): CBN[Int, Int] =
//     CBN(k => k(n))

//   def bool(b: Boolean): CBN[Boolean, Boolean] =
//     CBN(k => k(b))

//   def app[A, B](f: RLam[A, B], x: CBN[A, A]): CBN[B, B] =
//     CBN(k => f.run(vf => vf(x).run(vx => k(vx))))

//   def add(x: CBN[Int, Int], y: CBN[Int, Int]): CBN[Int, Int] =
//     CBN(k => x.run(vx => y.run(vy => k(vx + vy))))

//   def mul(x: CBN[Int, Int], y: CBN[Int, Int]): CBN[Int, Int] =
//     CBN(k => x.run(vx => y.run(vy => k(vx * vy))))

//   def leq(x: CBN[Int, Int], y: CBN[Int, Int]): CBN[Boolean, Boolean] =
//     CBN(k => x.run(vx => y.run(vy => k(vx <= vy))))

//   def _if[D, S](c: CBN[Boolean, Boolean])(t: => CBN[D, S])(e: => CBN[D, S]): CBN[D, S] =
//     CBN(k => c.run(vc => if vc then t.run(k) else e.run(k)))

//   def fix[A, B](f: (() => RLam[A, B]) => RLam[A, B]): RLam[A, B] =
//     lazy val self: RLam[A, B] =
//       lam[A, B](arg => app(f(() => self), arg))
//     self

// given cbnSymantics: CpsBase with
//   def lam[A, B](f: SFun[A, B]): RLam[A, B] =
//     CBN(k => k(f))

// given cbvSymantics: CpsBase with
//   def lam[A, B](f: SFun[A, B]): RLam[A, B] =
//     CBN(k =>
//       k { e =>
//         CBN[B, B](k2 =>
//           e.run(v =>
//             f(CBN[A, A](ka => ka(v.asInstanceOf[A]))).run(k2) // cast?
//           )
//         )
//       }
//     )