package taglessFinal

def varZ[A, Env](env: (A, Env)): A = 
    env._1;
def varS[A, B, Env](vp: Env => A)(env: (B, Env)): A = 
    vp(env._2)
def b[Env](bv: Boolean): Env => Boolean =
    _ => bv

def lam[A, B, Env](e: ((A, Env)) => B): Env => (A => B) =
    env => x => e((x, env))

def app[A, B, Env](e1: Env => (A => B), e2: Env => A): Env => B = 
    env => e1(env)(e2(env))

