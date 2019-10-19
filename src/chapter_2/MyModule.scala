package chapter_2

// A comment!
/* Another comment */
/** A documentation comment */
object MyModule {          //singleton object, which simultaneously declares a class and its only instance
  def abs(n: Int): Int =  //abs takes an integer and returns an integer
    if (n < 0) -n         //returns the negation of 'n' if it's less than zero
    else n

  def factorial(n: Int): Int = {
    @annotation.tailrec   // compiler give us a compile error if it’s unable to eliminate the tail calls of the function
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)

    go(n, 1)
  }

  def fibonacci(n: Int): Int = {
    @annotation.tailrec
    def go(n:Int, acc: Int): Int =
      if (n == 0) 0
      else if (n == 1) acc
      else go(n-1, acc+n)

    go(n, 1)
  }

  private def formatFibonacci(x: Int) = {     //private method can only be called by other members of MyModule
    val msg = "The fibonacci of %d is %d"             //string with two placeholders for numbers marked as %d
    msg.format(x, fibonacci(x))                       //replace the two %d placeholders in the string with 'x' and 'fibonacci(x)' respectively
  }

  private def formatFactorial(x: Int) = {     //private method can only be called by other members of MyModule
    val msg = "The factorial of %d is %d"             //string with two placeholders for numbers marked as %d
    msg.format(x, factorial(x))                       //replace the two %d placeholders in the string with 'x' and 'factorial(x)' respectively
  }

  private def formatAbs(x: Int) = {     //private method can only be called by other members of MyModule
    val msg = "The absolute value of %d is %d"  //string with two placeholders for numbers marked as %d
    msg.format(x, abs(x))                       //replace the two %d placeholders in the string with 'x' and 'abs(x)' respectively
  }

  // higher-order function (HOF) - takes w function as an argument
  def formatResult(name: String, n: Int, f: Int => Int) = { //high order function: f required to be a function from Int ot Int
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  // Polymorphic functions - works for any type it’s given
  // Monomorphic function to find a String in an array
  def findFirst(ss: Array[String], key: String): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= ss.length) -1    // if 'n' is past the end of the array, return -1 --> key doesn't exist in the array
      else if (ss(n) == key) n  // 'ss(n)' extracts the nth element of the array 'ss'. If the element at 'n' is equal to the key, return 'n' --> element appears in the array at that index
      else loop(n + 1)          // increment 'n' and keep looking

    loop(0) // start the loop at the first element of the array
  }

  // Polymorphic (generic) function to find an element in an array
  // instead of hardcoding 'String', take a type 'A' as a parameter
  // instead of hardcoding an equity check for a given key, take a function 'p' with which to test each element of the array
  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n //if the function 'p' matches the current element, we've found a match and we returns its index in the array
      else loop(n + 1)

    loop(0)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n >= as.length -1) true
      else if (ordered(as(n), as(n+1))) loop(n + 1)
      else false

    loop(0)
  }

  // Polymorphic functions are often so constrained by their type that they only have one implementation!
  /*
  The partial1 function has three type parameters: A, B, and C. It then takes two arguments.
  The argument f is itself a function that takes two arguments of types A and B,
  respectively, and returns a value of type C. The value returned by partial1 will also be
  a function, of type B => C.
  The return type of partial1 is B => C, so we know that we have to return a function of that type: (b: B)
  On the right-hand-side of the => arrow comes the body of that anonymous function.
  Now that we’ve asked for a value of type B, what do we want to return from our anonymous function?
  The type signature says that it has to be a value of type C. According to the signature, C is
  the return type of the function f.
  */
  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    //(b: B) => f(a, b)
    b => f(a, b)
  /* The result is a higher-order function that takes a function of two arguments and partially applies it. That is, if we have an A
  and a function that needs both A and B to produce C, we can get a function that just needs B to produce C (since we already have the A).
  */


  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => ((b: B) => f(a, b))

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  def main(args: Array[String]): Unit = {//'unit' serves the same purpose as void in Java or C
    println(formatAbs(-42))
    println(formatFactorial(7))
    println(formatFibonacci(7))

    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
    println(formatResult("fibonacci", 7, fibonacci))
    // some examples of anonymous functions:
    println(formatResult("increment", 7, (x: Int) => x + 1))
    println(formatResult("increment2", 7, (x) => x + 1))
    println(formatResult("increment3", 7, x => x + 1))
    println(formatResult("increment4", 7, _ + 1))
    println(formatResult("increment5", 7, x => { val r = x + 1; r }))

    println(findFirst(Array("AA", "BB", "CC", "AA"), "AA"))
    println(findFirst(Array(7, 8, 9, 13, 9), (x: Int) => x == 9))

    println(isSorted(Array(7, 8, 9, 13, 9), (x: Int, y: Int) => x < y))
    println(isSorted(Array(7), (x: Int, y: Int) => x < y))
    println(isSorted(Array(7, 8), (x: Int, y: Int) => x < y))
    println(isSorted(Array(7, 8, 9, 13, 15), (x: Int, y: Int) => x < y))

    // f andThen g is the same as g compose f
    val f = (x: Double) => math.Pi / 2 - x
    val cos = f andThen math.sin
    println("cos: " + cos(1))
  }


}
