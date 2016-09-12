package errorhandling


sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = {
    this match {
      case None => None
      case Some(a) => Some(f(a))
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    this match {
      case None => None
      case Some(a) => f(a)
    }
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case None => default
      case Some(a) => a
    }
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this match {
      case None => ob
      case _ => this
    }
  }

  def filter(f: A => Boolean): Option[A] = {
    this match {
      case Some(a) if f(a) => this
      case _ => None
    }
  }



}

case class Some[+A](get:A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  def lift[A,B](f:A => B):Option[A] => Option[B] = _ map f

  def mean(xs:Seq[Double]):Option[Double] = {
    if(xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  def variance(xs:Seq[Double]):Option[Double] = {
    mean(xs) flatMap( m => mean(xs.map(x => math.pow(x - m,2))))
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap( a => b.map( b => f(a,b) ) )
  }


  def sequence[A](a:List[Option[A]]):Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case h::t => h flatMap(hh => sequence(t) map  (hh :: _) )
    }
  }


  def sequence_1[A](a:List[Option[A]]):Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _) )
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case h::t => f(h) flatMap {
        h1 =>
          traverse(t)(f) map {
            t1 =>
              h1 :: t1
          }
      }
    }
  }

  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case h::t => map2(f(h),traverse_1(t)(f))(_ :: _)
    }
  }

  def sequenceViaTraverse[A](a:List[Option[A]]):Option[List[A]] = {
    traverse(a)(x => x)
  }


}