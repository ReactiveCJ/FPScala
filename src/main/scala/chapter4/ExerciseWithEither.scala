package chapter4

object ExerciseWithEither  extends App{

  def mean(xs:IndexedSeq[Double]):Either[String,Double] = {
    if(xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)
  }

  def safeDiv(x:Double,y:Double):Either[Exception,Double] = {
    try {
      Right(x/y)
    }catch {
      case e:Exception => Left(e)
    }
  }

  case class Person(name: Name, age: Age)
  sealed case class Name( value: String)
  sealed case class Age( value: Int)

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person)

  println(mkPerson("",-1))

  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    es match {
      case Nil => Right(Nil)
      case h::t => f(h) flatMap(
        h1 =>
          traverse(t)(f) map ( h1::_))
    }
  }

  val l1 = List(1,2,0,3,4,5)
  def f(x:Int):Either[String,Int] = {
    if(x < 1)
      Left("Less than 1")
    else
      Right(x * 3)
  }

  println(traverse(l1)(f))

}
