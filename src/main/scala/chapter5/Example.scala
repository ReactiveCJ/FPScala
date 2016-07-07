package chapter5

import Stream._

object Example extends App {

  def if2[A](cond: Boolean, OnTrue: => A, OnFalse: => A): A = {
    if (cond)
      OnTrue
    else
      OnFalse
  }

  println(if2(false, sys.error("fail"), 3))

  def pair(i: => Int) = (i, i)

  pair {
    println("hi"); 1 + 41
  }

  //we can cache the value explicitly and use lazy keyword
  def pair2(i: => Int) = {
    lazy val j = i; (j, j)
  }

  pair2 {
    println("hi2"); 1 + 41
  }


  //infinite streams and recursion
  val ones: Stream[Int] = cons(1, ones)
  ones.take(5).toList
  ones.map(_ + 1).exists(_ % 2 == 0)
  ones.takeWhile(_ == 1)
  ones.forAll(_ != 1)
}
