package errorhandling


trait Either[+E,+A] {

  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(_) => b
      case Right(_) => this
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    this.flatMap( a1 => b.map( b1 => f(a1,b1)))
  }

  def right:Option[A] = {
    this match {
      case Left(_) => None
      case Right(a) => Some(a)
    }
  }

}

case class Left[+E](value:E) extends Either[E,Nothing]

case class Right[+A](value:A) extends Either[Nothing,A]

object Either {

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

  def Try[A](a: => A):Either[Exception,A] = try Right(a) catch { case e:Exception => Left(e)}

  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    es match {
      case Nil => Right(Nil)
      case h::t => f(h) flatMap(
        h1 =>
          traverse(t)(f) map ( h1 :: _))
    }
  }

  def traverse_1[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    es match {
      case Nil => Right(Nil)
      case h::t => f(h).map2(traverse_1(t)(f))(_::_)
    }
  }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(x => x)

  val l1 = List(1,2,0,3,4,5)

  def f(x:Int):Either[String,Int] = {
    if(x < 0)
      Left("Less than 1")
    else
      Right(x * 3)
  }




}

object test extends App{

  import Either._

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

  def insure(age:String,speed:String):Either[Exception,Double] = {
    Try(age.toDouble).map2(Try(speed.toDouble))(_*_)
  }


  val l1 = List(1,2,0,3,4,5)
  println(traverse(l1)(f))

  println(insure("2.o","3.0"))

}