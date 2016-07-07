package chapter5



trait Stream[+A] {

  import Stream._
  def uncons:Option[(A,Stream[A])]
  def isEmpty:Boolean = uncons.isEmpty

  def toListRecursive:List[A] = {
    uncons match {
      case Some((h,t)) =>
        println(123)
        h :: t.toListRecursive
      case None => Nil
    }
  }

  def toList:List[A] = {

    def go(s:Stream[A],acc:List[A]):List[A] = {
      s.uncons match {
        case Some((h,t)) => go(t,h :: acc)
        case None => acc
      }
    }
    go(this,List()).reverse
  }

  def take(n:Int):Stream[A] = {
    uncons match {
      case Some((h,t)) if n >1 =>
        cons(h,t.take(n-1))
      case Some((h, _)) if n == 1 => cons(h, empty)
      case _ => empty
    }
  }

  def takeWhile(f:A => Boolean):Stream[A] = {
    uncons match {
      case Some((h,t)) if f(h) => cons(h,t.takeWhile(f))
      case Some((h,t)) => t.takeWhile(f)
      case None => empty
    }
  }

  @inline
  def foldRight[B](z: =>B)(f: (A, =>B) =>B):B = {
    uncons match {
      case Some((h,t)) =>
        f(h,t.foldRight(z)(f))
      case None =>
        z
    }
  }

  def exists(p: A => Boolean):Boolean = {
    foldRight(false)((a,b) => p(a) || b)
  }

  def forAll(p: A => Boolean):Boolean = {
    foldRight(true)((a,b) => p(a) && b)
  }

  def takeWhileViaFoldRight(f:A => Boolean):Stream[A] = {
    foldRight(empty[A])(
      (a,b) =>
        if( f(a))
          cons(a,b)
        else
          b
    )
  }

  def headOption: Option[A] =
    foldRight(None: Option[A])((h,_) => Some(h))

  def map[B](f: A => B):Stream[B] = {
    foldRight(empty[B])((a, b) => cons(f(a), b))
  }

  def filter(f:A => Boolean):Stream[A] = {
    uncons match {
      case Some((h,t)) if f(h) => cons(h,t.takeWhile(f))
      case Some((h,t)) => t.takeWhile(f)
      case None => empty
    }
  }

  def append[B >: A](s: => Stream[B]):Stream[B] = {
    foldRight(s)((h,t) => cons(h,t))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h,t) => f(h) append t)


  def mapViaUnfold[B](f: A => B):Stream[B] = {
    unfold(this)( {
      case Cons(h,t) =>
        Some(f(h),t)
      case _ =>
        println(123)
        None
    }
    )
  }

  def takeViaUnfold(n:Int):Stream[A] = {
    unfold((this,n)) {
      case (Cons(h,t),1) => Some(h,(empty,0))
      case (Cons(h,t),m) => Some(h,(t,m-1))
      case _ => None
    }
  }

  def takeWhileViaUnfold(p:A => Boolean):Stream[A] = {
    unfold(this) {
      case Cons(h,t) if p(h) => Some(h,t)
      case _ => None
    }
  }

  def zipWith[B,C](s2:Stream[B])(f:(A,B) => C):Stream[C] = {
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1, h2), (t1, t2))
      case _ => None
    }
  }

  def zip[B](s2:Stream[B]):Stream[(A,B)] = {
    zipWith(s2)((_,_))
  }

  def zipAll[B](s2:Stream[B]):Stream[(Option[A],Option[B])] = {
    zipWithAll(s2)((_,_))
  }

  def zipWithAll[B,C](s2:Stream[B])(f:(Option[A],Option[B]) => C):Stream[C] = {
    unfold(this,s2) {
      case (Empty,Empty) => None
      case (Cons(h,t),Empty) => Some(f(Some(h), Option.empty[B]),(t, empty[B]))
      case (Empty,Cons(h,t)) => Some(f(Option.empty[A],Some(h)),(empty[A],t))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1), Some(h2)), (t1,t2))
    }
  }

}

case class Cons[+A](h:A ,tl:Stream[A]) extends Stream[A] {
  lazy val uncons = Some(h, tl)
}

case object Empty extends Stream[Nothing] {
  def uncons = None
}

object Stream {

  def empty[A]:Stream[A] = new Stream[A] {
    def uncons = None
  }

  def cons[A](hd: => A, tl: => Stream[A]):Stream[A]  = {
    new Stream[A] {
      lazy val uncons = Some(hd,tl)
    }
  }

  def apply[A](as:A*):Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head,apply(as.tail:_*))
  }

  def constant[A](a:A):Stream[A] = {
    cons(a,constant(a))
  }

  def from(n: Int): Stream[Int] = {
    cons(n,from(n+1))
  }

  def fibs:Stream[Int] = {
    def go(f0:Int,f1:Int):Stream[Int] =
      cons(f0,go(f1,f0+f1))
    go(0,1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((h,s)) => cons(h,unfold(s)(f))
      case None => empty
    }
  }

  def fibsViaUnfold = unfold((0,1)){case (f0,f1) => Some(f0,(f1,f0+f1))}

  def fromViaUnfold(n:Int) = unfold(n)(n => Some(n,n+1))

  def constantViaUnfold(n:Int) = unfold(n)(n => Some(n,n))

  def onesViaUnfold(n:Int) = constant(1)


  def startsWith[A](s: Stream[A], s2: Stream[A]): Boolean = {
    s.zipAll(s2).takeWhile(_._2.isDefined).forAll{ case (a,b) => a == b}
  }


}