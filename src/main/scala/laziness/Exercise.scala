package laziness

import laziness.Stream._

object Exercise extends App{

  val s = Stream(1,2,3,4,5)
  val p = Stream(6,7,8,9)
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

  println(s.takeViaUnfold(2).toList)

  println(s.takeWhile(_ < 3).toList)

  println(s.zip(p).take(4).toList)

  println(s.zipAll(p).take(6).toList)

  println(s.tails.take(5).map(_.toList).toList)

  println(Stream(1,2,3).scanRight(0)(_+_).toList)
}
