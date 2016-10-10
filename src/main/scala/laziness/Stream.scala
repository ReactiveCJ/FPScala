package laziness


case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A,t: () => Stream[A]) extends Stream[A]

trait Stream[+A] {

  import Stream._


  def headOption:Option[A] = this match {
    case Empty => None
    case Cons(h,t) => Some(h())
  }

  def toListRecursive:List[A] = {
    this match {
      case Cons(h,t) =>
        h() :: t().toListRecursive
      case Empty => Nil
    }
  }

  def toList:List[A] = {

    def go(s:Stream[A],acc:List[A]):List[A] = {
      s match {
        case Cons(h,t) => go(t(),h() :: acc)
        case Empty => acc
      }
    }
    go(this,List()).reverse
  }

  def take(n:Int):Stream[A] = {
    this match {
      case Cons(h,t) if n >1 =>
        println(12)
        cons(h(),t().take(n-1))
      case Cons(h, _) if n == 1 =>
        println(32)
        cons(h(), empty)
      case _ => empty
    }
  }


  def drop(n:Int):Stream[A] = {
    this match {
      case Cons(h,t) if n > 1 =>
        t().drop(n-1)
      case Cons(h,t) if n ==0 =>
        t()
    }
  }



  def takeWhile(f:A => Boolean):Stream[A] = {
    this match {
      case Cons(h,t) if f(h()) => cons(h(),t().takeWhile(f))
      case Cons(h,t) => t().takeWhile(f)
      case Empty => empty
    }
  }


  @inline
  def foldRight[B](z: =>B)(f: (A, =>B) =>B):B = {
    this match {
      case Cons(h,t) =>
        f(h(),t().foldRight(z)(f))
      case _ => z
    }
  }


  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z,Stream(z)))(
      (a,p0) => {
        lazy val p1 = p0
        val b2 = f(a, p1._1)
        (b2, cons(b2, p1._2))
      }
    )._2

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

  def map[B](f: A => B):Stream[B] = {
    foldRight(empty[B])((a, b) => cons(f(a), b))
  }

  def filter(f:A => Boolean):Stream[A] = {
    foldRight(empty[A])((a,b) =>
      if(f(a))
        cons(a,b)
      else
        b
    )
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h,t) => f(h) append t)

  def append[B >: A](s: => Stream[B]):Stream[B] = {
    foldRight(s)((h,t) => cons(h,t))
  }


  def find(p: A => Boolean):Option[A] = filter(p).headOption


  def mapViaUnfold[B](f: A => B):Stream[B] =
    unfold(this){
      case Cons(h,t) =>
        Some(f(h()),t())
      case _ =>
        None
    }


  def takeViaUnfold(n:Int):Stream[A] =
    unfold((this,n)){
      case (Cons(h,t),1) => Some(h(),(empty[A],0))
      case (Cons(h,t),m) => Some(h(),(t(),m-1))
      case _ => None
    }

  def takeWhileViaUnfold(p:A => Boolean):Stream[A] =
    unfold(this) {
      case Cons(h,t) if p(h()) => Some(h(),t())
      case _ => None
    }

  def zipWith[B,C](s2:Stream[B])(f:(A,B) => C):Stream[C] =
    unfold((this, s2)){
        case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
        case _ => None
    }



  def zip[B](s2:Stream[B]):Stream[(A,B)] = zipWith(s2)((_,_))


  def zipAll[B](s2:Stream[B]):Stream[(Option[A],Option[B])] = zipWithAll(s2)((_,_))

  def zipWithAll[B,C](s2:Stream[B])(f:(Option[A],Option[B]) => C):Stream[C] =
    unfold(this,s2){
        case (Empty,Empty) => None
        case (Cons(h,t),Empty) => Some(f(Some(h()), Option.empty[B]),(t(), empty[B]))
        case (Empty,Cons(h,t)) => Some(f(Option.empty[A],Some(h())),(empty[A],t()))
        case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())), (t1(),t2()))
    }

  def tails: Stream[Stream[A]] =
    unfold(this){
        case Cons(h,t) => Some(this,t())
        case _ => None
    }


}



object Stream {


  def cons[A](hd: => A, tl: => Stream[A]):Stream[A]  = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head,() => tail)
  }

  def empty[A]:Stream[A] = Empty

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

  def hasSubsequence[A](s1: Stream[A], s2: Stream[A]): Boolean =
    s1.tails exists (startsWith(_,s2))

}