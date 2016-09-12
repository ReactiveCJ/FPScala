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
