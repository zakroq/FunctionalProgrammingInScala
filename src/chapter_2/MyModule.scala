package chapter_2

// A comment!
/* Another comment */
/** A documentation comment */
object MyModule {          //singleton object, which simultaneously declares a class and its only instance
  def abs(n: Int): Int =  //abs takes an integer and returns an integer
    if (n < 0) -n         //returns the negation of 'n' if it's less than zero
    else n

  private def formatAbs(x: Int) = {     //private method can only be called by other members of MyModule
    val msg = "The absolute value of %d is %d"  //string with two placeholders for numbers marked as %d
    msg.format(x, abs(x))                       //replace the two %d placeholders in the string with 'x' and 'abs(x)' respectively
  }

  def main(args: Array[String]): Unit = //'unit' serves the same purpose as void in Java or C
    println(formatAbs(-42))

}
