object funSession {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  def even(x: Int): Boolean = {
    x % 2 == 0
  }                                               //> even: (x: Int)Boolean

  def odd(x: Int): Boolean = {
    x % 2 == 1
  }                                               //> odd: (x: Int)Boolean

	/*
  	#6: --------------------------------------------------------------------------------------------------
  	Returns number of elements that matches the predicate.
  */

  // Iterative:
  def six_Iter[T](list: List[T], pred: T => Boolean): Int = {
    var count = 0
    for (i <- list) {
      if (pred(i))
        count += 1
    }
    count
  }                                               //> six_Iter: [T](list: List[T], pred: T => Boolean)Int

  six_Iter(List(1, 2, 3, 4, 5), even)             //> res0: Int = 2
  six_Iter(List(1, 2, 3, 4, 5), odd)              //> res1: Int = 3

  // Recursive:
  def six_Rec[T](list: List[T], pred: T => Boolean): Int = {
    if (list == Nil) {
      0
    } else if (pred(list.head)) {
      1 + six_Rec(list.tail, pred)
    } else {
      six_Rec(list.tail, pred)
    }
  }                                               //> six_Rec: [T](list: List[T], pred: T => Boolean)Int

  six_Rec(List(1, 2, 3, 4, 5), even)              //> res2: Int = 2
  six_Rec(List(1, 2, 3, 4, 5), odd)               //> res3: Int = 3

  // Tail recursive:
  def six_Tail[T](list: List[T], pred: T => Boolean) = {
    def helper(vals: List[T], cond: T => Boolean, count: Int): Int =
      if (vals == Nil)
        count
      else if (pred(vals.head))
        helper(vals.tail, cond, count + 1)
      else
        helper(vals.tail, cond, count)
    helper(list, pred, 0)
  }                                               //> six_Tail: [T](list: List[T], pred: T => Boolean)Int

  six_Tail(List(1, 2, 3, 4, 5), even)             //> res4: Int = 2
  six_Tail(List(1, 2, 3, 4, 5), odd)              //> res5: Int = 3

  // Map-filter-reduce
  def six_MFR[T](list: List[T], pred: T => Boolean) = {
    list.filter(pred).length
  }                                               //> six_MFR: [T](list: List[T], pred: T => Boolean)Int

  six_MFR(List(1, 2, 3, 4, 5), even)              //> res6: Int = 2
  six_MFR(List(1, 2, 3, 4, 5), odd)               //> res7: Int = 3

  /*
  	#7: --------------------------------------------------------------------------------------------------
  	Returns true if all elements of a list matches a predicate
  */

  // Iterative:
  def seven_Iter[T](list: List[T], pred: T => Boolean): Boolean = {
    var truth = true
    for (i <- list; if (truth)) {
      if (!pred(i))
        truth = false
    }
    truth
  }                                               //> seven_Iter: [T](list: List[T], pred: T => Boolean)Boolean

  seven_Iter(List(1, 2, 3, 4, 5), even)           //> res8: Boolean = false
  seven_Iter(List(2, 4), even)                    //> res9: Boolean = true
  seven_Iter(List(1, 2, 3, 4, 5), odd)            //> res10: Boolean = false
  seven_Iter(List(1, 3, 5), odd)                  //> res11: Boolean = true

  // Recursive:
  def seven_Rec[T](list: List[T], pred: T => Boolean): Boolean = {
    if (list == Nil || !pred(list.head))
      false
    else if (pred(list.head) && list.tail == Nil)
      true
    else
      seven_Rec(list.tail, pred)
  }                                               //> seven_Rec: [T](list: List[T], pred: T => Boolean)Boolean

  seven_Rec(List(1, 2, 3, 4, 5), even)            //> res12: Boolean = false
  seven_Rec(List(2, 4), even)                     //> res13: Boolean = true
  seven_Rec(List(1, 2, 3, 4, 5), odd)             //> res14: Boolean = false
  seven_Rec(List(1, 3, 5, 7, 9, 11), odd)         //> res15: Boolean = true

  // Tail recursive: it's the same as the recursive version because there are no pending processes nor variables added to stack.

  // Map-filter-reduce
  def seven_MFR[T](list: List[T], pred: T => Boolean) = {
    list.filter(pred).length == list.length
  }                                               //> seven_MFR: [T](list: List[T], pred: T => Boolean)Boolean

  seven_MFR(List(1, 2, 3, 4, 5), even)            //> res16: Boolean = false
  seven_MFR(List(2, 4), even)                     //> res17: Boolean = true
  seven_MFR(List(1, 2, 3, 4, 5), odd)             //> res18: Boolean = false
  seven_MFR(List(1, 3, 5), odd)                   //> res19: Boolean = true

  /*
  	#8: --------------------------------------------------------------------------------------------------
  	Returns true if at least one element of a list matches a predicate
  */

  // Iterative
  def eight_Iter[T](list: List[T], pred: T => Boolean): Boolean = {
    var truth = false
    for (i <- list; if (!truth)) {
      if (pred(i))
        truth = true
    }
    truth
  }                                               //> eight_Iter: [T](list: List[T], pred: T => Boolean)Boolean

  eight_Iter(List(1, 2, 3, 4, 5), even)           //> res20: Boolean = true
  eight_Iter(List(1, 3, 5), even)                 //> res21: Boolean = false
  eight_Iter(List(1, 2, 3, 4, 5), odd)            //> res22: Boolean = true
  eight_Iter(List(2, 4), odd)                     //> res23: Boolean = false

  // Recursive
  def eight_Rec[T](list: List[T], pred: T => Boolean): Boolean = {
    if (list == Nil)
      false
    else if (pred(list.head))
      true
    else
      eight_Rec(list.tail, pred)
  }                                               //> eight_Rec: [T](list: List[T], pred: T => Boolean)Boolean

  eight_Rec(List(1, 2, 3, 4, 5), even)            //> res24: Boolean = true
  eight_Rec(List(1, 3, 5), even)                  //> res25: Boolean = false
  eight_Rec(List(1, 2, 3, 4, 5), odd)             //> res26: Boolean = true
  eight_Rec(List(2), odd)                         //> res27: Boolean = false

  // Tail recursive: it's the same as the recursive version because there are no pending processes nor variables added to stack.

  // Map-filter-reduce
  def eight_MFR[T](list: List[T], pred: T => Boolean) = {
    list.filter(pred).length > 0
  }                                               //> eight_MFR: [T](list: List[T], pred: T => Boolean)Boolean

  eight_MFR(List(1, 2, 3, 4, 5), even)            //> res28: Boolean = true
  eight_MFR(List(1, 3, 5), even)                  //> res29: Boolean = false
  eight_MFR(List(1, 2, 4, 6), odd)                //> res30: Boolean = true
  eight_MFR(List(2, 4), odd)                      //> res31: Boolean = false
}