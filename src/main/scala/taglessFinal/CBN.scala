package taglessFinal

/* 

let e1 = (fun k -> k 1)
let e2 = (fun k -> k 2)
let add e1 e2 = fun k -> e1 (fun v1 -> eq (fun v2 -> k (v1 + v2)))

eq (fun v1 -> ) so k = fun v1 ->. This means v1 is 1 and v2 is 2

let int x = fun k -> k x
(int -> w) -> w 
k expects an int, k returns some answer type w
k: int -> w

add also has type (int -> w) -> w. Extract ints v1 and v2 and apply k to them.

let lam f = fun k -> k f

f is already the semantic function ... 

let lam f = fun k -> k f
let app e1 e2 = fun k -> e1 (fun f -> f e2 k)

app e1 e2 does not evaluate the argument e2 by applying it to k, it passes e2 unevaluated
to the abstraction. Interping yields type. We can reuse the type function static from the 
previous section. 

 */


// Any approximates
// could also use a type here?
case class CBN[S, D](run: (S => Any) => Any)

// CBN CPS Interpreter. 
given cbnSymantics: SymanticsPE[CBN] with 

    override def int(n: Int): CBN[Int, Int] = CBN(k => k(n)) // run (Int => Any) => Any
    override def bool(b: Boolean): CBN[Boolean, Boolean] = CBN(k => k(b)) // override 

    override def lam[A, B](f: CBN[A, A] => CBN[B, B]): RLam[A, B] = CBN(k => k(f))
    override def app[A, B](f: CBN[CBN[A, A] => CBN[B, B], A => B], x: CBN[A, A]): CBN[B, B] =  CBN(k =>
        f.run (vf => vf(x).run ( vx =>
            k(vx)
        )))
    
    // ((k: Int => Any) => k(2))
        //(vx => ...)
        // sub (vx => ...) for k and suddenly you have sout
    override def add(x: CBN[Int, Int], y: CBN[Int, Int]): CBN[Int, Int] = CBN(k =>
        x.run (
            vx =>
                y.run (vy =>
                    k (vx + vy))
        ))

    override def mul(x: CBN[Int, Int], y: CBN[Int, Int]): CBN[Int, Int] = CBN(k =>
        x.run (
            vx =>
                y.run (vy =>
                    k (vx * vy))
        ))

    override def leq(x: CBN[Int, Int], y: CBN[Int, Int]): CBN[Boolean, Boolean] = CBN(k =>
        x.run (
            vx =>
                y.run (vy =>
                    k (vx <= vy))
        ))

    override def _if[D, S](c: CBN[Boolean, Boolean])(t: => CBN[D, S])(e: => CBN[D, S]): CBN[D, S] =
        CBN(k =>
            c.run(vc =>
            if vc then t.run(k)
            else e.run(k)
            )
        )

    override def fix[A, B](f: (() => RLam[A, B]) => CBN[CBN[A, A] => CBN[B, B], A => B]): RLam[A, B] = 
        lazy val self: RLam[A, B] = lam[A, B](arg => app(f(() => self), arg))
        self

    
def runCBN[S, D](x: CBN[S, D]): S =
    x.run(v => v).asInstanceOf[S]