package chapter5

import chapter5.Stream._

object Exercise extends App{

  val s = Stream(1,2,3,4,5)
  println(s.toList)
  println(s.take(2).toList)
  println(s.takeWhile(_ % 2 == 0).toList)
  println(s.takeWhileViaFoldRight(_ % 2 == 0).toList)


  val twos = constant(2)

  println(twos.take(3).toList)

  val fromThrees = from(3)
  println(fromThrees.take(3).toList)

  println(fibs.take(5).toList)

  println(s.mapViaUnfold(_+2).toList)

}
