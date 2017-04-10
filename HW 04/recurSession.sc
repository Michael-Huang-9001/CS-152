object recursionSession {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  /*
  	#5: --------------------------------------------------------------------------------------------------
  	Using tail recursion, we can avoid pushing additional calls into the stack frame, so it helps with the stack overflow.
  	However, since the number of calls are still relatively n or m calls (depending on the function),
  	tail recursion should still be O(n).
  */

  def inc(n: Int) = n + 1                         //> inc: (n: Int)Int
  def dec(n: Int) = n - 1                         //> dec: (n: Int)Int

  def add(n: Int, m: Int) = {
    def helper(result: Int, count: Int): Int =
      if (count <= 0)
        result
      else
        helper(inc(result), dec(count))
    helper(n, m)
  }                                               //> add: (n: Int, m: Int)Int

  add(6, 17)                                      //> res0: Int = 23

  def mul(n: Int, m: Int) = {
    def helper(result: Int, count: Int): Int =
      if (count <= 0)
        result
      else
        helper(add(result, n), dec(count))
    helper(0, m)
  }                                               //> mul: (n: Int, m: Int)Int

  mul(4, 3)                                       //> res1: Int = 12

  def exp2(m: Int) = {
    def helper(result: Int, count: Int): Int =
      if (count <= 0)
        result
      else
        helper(mul(result, 2), dec(count))
    helper(1, m)
  }                                               //> exp2: (m: Int)Int

  exp2(0)                                         //> res2: Int = 1
  exp2(1)                                         //> res3: Int = 2
  exp2(2)                                         //> res4: Int = 4
  exp2(3)                                         //> res5: Int = 8
  exp2(4)                                         //> res6: Int = 16
  exp2(5)                                         //> res7: Int = 32

  def hyperExp(m: Int) = {
    def helper(result: Int, count: Int): Int =
      if (count <= 0)
        result
      else
        helper(exp2(result), dec(count))
    helper(exp2(0), m)
  }                                               //> hyperExp: (m: Int)Int

  hyperExp(0)                                     //> res8: Int = 1
  hyperExp(1)                                     //> res9: Int = 2
  hyperExp(2)                                     //> res10: Int = 4
  hyperExp(3)                                     //> res11: Int = 16

  /*
  	#7: --------------------------------------------------------------------------------------------------
  */

  object Calculator {
    def repl {
      def helper(cmmd: Array[String]) {
        if (!cmmd(0).equals("quit")) {
          try {
            if (cmmd.length != 3) {
              throw new Exception("syntax = NUMBER OPERATOR NUMBER")
            }
            if (cmmd(1) == "+") {
              println("result = " + (cmmd(0).toDouble + cmmd(2).toDouble))
            } else if (cmmd(1) == "*") {
              println("result = " + (cmmd(0).toDouble * cmmd(2).toDouble))
            } else if (cmmd(1) == "-") {
              println("result = " + (cmmd(0).toDouble - cmmd(2).toDouble))
            } else if (cmmd(1) == "/") {
              println("result = " + (cmmd(0).toDouble / cmmd(2).toDouble))
            } else {
              throw new Exception("unrecognized operator: " + cmmd(1))
            }
          } catch {
            case e: Exception => println(e)
          }
          helper(readLine("-> ").split("\\s+"))
        }
      }
      helper(readLine("-> ").split("\\s+"))
      println("bye")
    }
    def main(args: Array[String]): Unit = { repl }
  }

  /*
  	#9: --------------------------------------------------------------------------------------------------
  */
  
  def fib_rec(n: Int): Int = {
    if (n == 0)
      0
    else if (n == 1)
      1
    else
      fib_rec(n - 1) + fib_rec(n - 2)
  }                                               //> fib_rec: (n: Int)Int

  fib_rec(10)                                     //> res12: Int = 55

  def fib_tail(n: Int) = {
    def helper(x: Int, y: Int, z: Int): Int =
      if (x <= 0)
        y
      else
        helper(x - 1, z, y + z)
    helper(n, 0, 1)
  }                                               //> fib_tail: (n: Int)Int

  fib_tail(10)                                    //> res13: Int = 55

  /*
  	#10: --------------------------------------------------------------------------------------------------
  */

  // Generic Pascal's Triangle/bionomial coefficient formula from Java
  def choose(n: Int, m: Int): Int =
    if (n < m) // Used to prevent stack overflow
      0
    else if (n == m || m == 0)
      1
    else
      choose(n - 1, m - 1) + choose(n - 1, m)     //> choose: (n: Int, m: Int)Int

  choose(5, 3)                                    //> res14: Int = 10
}