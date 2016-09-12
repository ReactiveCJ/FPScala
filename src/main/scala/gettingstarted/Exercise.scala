package gettingstarted


object Exercise extends App{


  def isSorted[A](data:Array[A],gt:(A,A) => A):Boolean = {
    val g = data.reduceLeft(gt)
    g == data.head
  }

  def partiall[A,B,C](a:A,f:(A,B) =>C):B=>C = {
    val y = (a:A) => (b:B) => f(a,b)
    y(a)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a:A) => (b:B) => f(a,b)
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a:A,b:B) => f(a)(b)
  }

  def compose[A,B,C](f:B=>C, g:A=>B):A=>C = {
    (a:A) => f(g(a))
  }

}
