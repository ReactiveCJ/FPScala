package laziness

import Stream._

object Example extends App {

  //lazy evaluate
  val s = cons( {println("start evaluate"); 1}, empty)

  //head option
  println(s.headOption)

  //evaluate once
  println(s.headOption)

  //take
  val st = Stream(1,2,3)
  println(st.take(2).toList)

  //takeWhile
  println(st.takeWhile(_ > 1).toList)


  //exists
  println(st.exists(_==2))

  //forall
  println(st.forAll(_ < 4))


}
