
package taglessFinal

enum Var:
    case VZ
    case VS(v: Var)

enum Exp:
    case V(v: Var)
    case B(b: Boolean)
    case L(l: Exp)
    case A(fun: Exp, arg: Exp)


import Var.*
import Exp.*

val test1: Exp =
  A(
    L(V(VZ)),
    B(true)
  )

enum U:
  case UB(b: Boolean)
  case UA(f: U => U)

import U.*

def lookup[A](env: List[A], v: Var): A =
    (env, v) match // Match may not be exhaustive 
        case (x :: _, VZ) => x
        case (_ :: rest, VS(v2)) => lookup(rest, v2)

def eval0(env: List[U], e: Exp): U = 
    e match 
        case V(v) => lookup(env, v)
        case B(b) => UB(b)
        case L(body) => UA(x => eval0(x :: env, body))
        case A(e1, e2) => 
            eval0(env, e1) match 
                case UA(f) =>
                    f(eval0(env, e2))
                case other => throw new RuntimeException("can't apply to non func")

        
// Unfortunately we have the potential of runtime exceptions, both if the env is empty
// or if we pass poorly typed args to apply
// Eval never fails on well-typed terms, this soundness is not obvious to the metalang
// Must appease non-exhastive pattern matching in 