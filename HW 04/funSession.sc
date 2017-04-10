object funSession {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  def inc(x: Double) = x + 1                      //> inc: (x: Double)Double

  def double(x: Double) = 2 * x                   //> double: (x: Double)Double

  /*
  	#1: --------------------------------------------------------------------------------------------------
  	Taken from lecture notes and professor example.
  */

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    def r(x: A): C = f(g(x));
    r _
  }                                               //> compose: [A, B, C](f: B => C, g: A => B)A => C

  var a = compose(inc, double)                    //> a  : Double => Double = <function1>
  a(2)                                            //> res0: Double = 5.0
  a = compose(double, inc)
  a(2)                                            //> res1: Double = 6.0

  /*
  	#2: --------------------------------------------------------------------------------------------------
  	I didn't know which version you want, so I made recursive and iterative versions.
  	Since selfIter(f, 0)= id where id(x) = x, I assumed that selfIter(f, 1)= f, where f(x) = f(x), and selfIter(f, 1)= f(f(x)).
  	However, it can also be interpreted that selfIter(f, 1) is f composed with itself once, or f(f(x)).
  */

  // Iterative
  def selfIter_Iter[T](f: T => T, n: Int): T => T = {
    if (n <= 0) {
      def id(x: T) = x
      id _
    } else {
      var result = f
      for (i <- 1 until n) {
        result = compose(f, result)
      }
      result
    }
  }                                               //> selfIter_Iter: [T](f: T => T, n: Int)T => T

  var b = selfIter_Iter(double, 0)                //> b  : Double => Double = <function1>
  b(7)                                            //> res2: Double = 7.0
  b = selfIter_Iter(double, 1) // double(7)
  b(7)                                            //> res3: Double = 14.0
  b = selfIter_Iter(double, 3) // double(double(double(7))) or 2 * (2 * (2 * (7)))
  b(7)                                            //> res4: Double = 56.0

  // Recursive:
  def selfIter_Rec[T](f: T => T, n: Int): T => T = {
    if (n <= 0) {
      def id(x: T) = x
      id _
    } else
      compose(f, selfIter_Rec(f, n - 1))
  }                                               //> selfIter_Rec: [T](f: T => T, n: Int)T => T

  var c = selfIter_Rec(double, 0)                 //> c  : Double => Double = <function1>
  c(7)                                            //> res5: Double = 7.0
  c = selfIter_Rec(double, 1) // double(7)
  c(7)                                            //> res6: Double = 14.0
  c = selfIter_Rec(double, 3) // double(double(double(7))) or 2 * (2 * (2 * (7)))
  c(7)                                            //> res7: Double = 56.0

  /*
  	#5: --------------------------------------------------------------------------------------------------
  	Taken from lecture notes and examples the professor worked on in class.
  	This is also found in the DDS example worksheet the professor gave us. It's already tail recursive,
  	and adding a helper does not help at all as the inputs will be the same.
  */

  def controlLoop[S](state: S, cycle: Int, halt: (S, Int) => Boolean, update: (S, Int) => S): S = {
    if (halt(state, cycle))
      state
    else
      controlLoop(update(state, cycle), cycle + 1, halt, update)
  }                                               //> controlLoop: [S](state: S, cycle: Int, halt: (S, Int) => Boolean, update: (
                                                  //| S, Int) => S)S

  /*
  	#6: --------------------------------------------------------------------------------------------------
  	I don't know which one you want, the population BEFORE it exceeds 10^5 or AFTER, so I gave both.
  	Instructions:
  	"...the initial population of one doubles every week UNTIL IT EXCEEDS 10^5...compute the size of the final population."
  */

  // Halts if the next cycle will exceed 10^5
  def halt_1(currentState: Int, cycle: Int): Boolean = {
    if (currentState == 0)
      true
    else
      update(currentState, cycle + 1) > 100000
  }                                               //> halt_1: (currentState: Int, cycle: Int)Boolean

  // Halts if the current state exceeds 10^5
  def halt_2(currentState: Int, cycle: Int): Boolean = {
    if (currentState == 0)
      true
    else
      currentState > 100000
  }                                               //> halt_2: (currentState: Int, cycle: Int)Boolean

  def update(current: Int, cycle: Int): Int = {
    current * 2
  }                                               //> update: (current: Int, cycle: Int)Int

  // If initial state is 0 (no amoeba to breed)
  controlLoop(0, 0, halt_1, update)               //> res8: Int = 0

  // Population before exceeding 10^5
  controlLoop(1, 0, halt_1, update)               //> res9: Int = 65536

  // Population after exceeding 10^5
  controlLoop(1, 0, halt_2, update)               //> res10: Int = 131072

  /*
  	#7: --------------------------------------------------------------------------------------------------
  	Code is taken from lecture notes and in class examples. Professor worked on these problems in class.
  	However, it cannot solve functions that have constant outputs, such as def example(x: Double) = 1.
  	This causes infinite loops as the f(guess) will never be <= delta.
  */

  val delta = 1e-7                                //> delta  : Double = 1.0E-7

  def deriv(f: Double => Double) = {
    def df(x: Double): Double =
      (f(x + delta) - f(x)) / delta
    df _
  }                                               //> deriv: (f: Double => Double)Double => Double

  def solve(f: Double => Double) = {
    val initGuess = 1.0
    val df = deriv(f)

    def halt(guess: Double, cycle: Int): Boolean = {
      math.abs(f(guess)) <= delta
    }

    def update(guess: Double, cycle: Int): Double = {
      guess - f(guess) / df(guess)
    }

    controlLoop(initGuess, 0, halt, update)
  }                                               //> solve: (f: Double => Double)Double

  def funct(x: Double): Double = 3 * x + 2        //> funct: (x: Double)Double
  solve(funct)                                    //> res11: Double = -0.6666666669271359

  /*
  	#8: --------------------------------------------------------------------------------------------------
  	Code is taken from lecture notes and in class examples. Professor worked on these problems in class.
  */

  def squareRoot(x: Double) = {
    // Finds n that fits n^2 - x = 0, since solve finds roots.
    def findSqrt(n: Double) =
      n * n - x
    solve(findSqrt)
  }                                               //> squareRoot: (x: Double)Double

  squareRoot(49)                                  //> res12: Double = 7.000000000000002

  /*
  	#9: --------------------------------------------------------------------------------------------------
  	Code is taken from lecture notes and in class examples. Professor worked on these problems in class.
  */

  def cubeRoot(x: Double) = {
    // Finds n that fits n^3 - x = 0, since solve finds roots.
    def findCbrt(n: Double) =
      n * n * n - x
    solve(findCbrt)
  }                                               //> cubeRoot: (x: Double)Double

  cubeRoot(27)                                    //> res13: Double = 3.0000000000001155

  /*
  	#10: --------------------------------------------------------------------------------------------------
  	Code is taken from lecture notes and in class examples. Professor worked on these problems in class.
  */

  def nthRoot(x: Double, n: Int) = {
    def findRoot(z: Double) =
      math.pow(z, n) - x
    solve(findRoot)
  }                                               //> nthRoot: (x: Double, n: Int)Double

  nthRoot(16, 4)                                  //> res14: Double = 2.0000000000023226
}