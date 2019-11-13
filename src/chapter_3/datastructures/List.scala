package chapter_3.datastructures

// A trait is an abstract interface that may optionally contain implementations of some methods.
// Adding sealed in front means that all implementations of the trait must be declared in this file
// Using 'A' parameter inside of the Cons data constructor, we declare the List data type to be polymorphic in the type
// of elements it contains, which means we can use this same definition for a list of Int elements (denoted List[Int]),
// Double elements (denoted List[Double]), String elements (List[String]), and so on.
// By adding the type parameter [+A] after 'sealed trait List', data types can be polymorphic.
// The + indicates that the type parameter A is covariant [variance annotation]- this means that, for instance, List[Dog] is considered a subtype of List[Animal],
// assuming Dog is a subtype of Animal. (More generally, for all types X and Y, if X is a subtype of Y, then List[X] is a subtype of List[Y])
sealed trait List[+A] // List data type, parametrized on a type, A - [+A -> typ A i wszystko z czego dziedziczy]
// Nothing is a subtype of all types, which means that in conjunction with the variance annotation, Nil can be considered a List[Int], a List[Double], and so on.
case object Nil extends List[Nothing] // A List data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists.
                                                            // Note that 'tail' is another List[A], which may be Nil or another Cons

object List { // List companion object, contains functions for creating and working with lists
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with 'x' is 'x' plus the sum of the rest of the list
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def main(args: Array[String]): Unit = {
    val ex1: List[Double] = Nil
    val ex2: List[Int] = Cons(1, Nil)
    val ex3: List[String] = Cons("a", Cons("b", Nil))
  }
}


