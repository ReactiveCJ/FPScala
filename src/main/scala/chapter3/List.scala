package chapter3

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head:A,tail:List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("List is Nil")
    case Cons(_, t) => t
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0)
      l
    else {
      l match {
        case Nil => Nil
        case Cons(_, t) => drop(t, n - 1)
      }
    }
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case Cons(h, t) => Cons(h, dropWhile(t)(f))
    }
  }

  def setHead[A](l: List[A], head: A): List[A] = {
    l match {
      case Nil => Cons(head, Nil)
      case Cons(_, t) => Cons(head, t)
    }
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }
  }

  def init[A](l: List[A]): List[A] = {
   l match {
     case Nil => Nil
     case Cons(h,Nil) => Nil
     case Cons(h,t) => Cons(h,init(t))
   }
  }

  def foldRight[A,B](l:List[A],z:B)(f:(A,B) => B):B = {
    l match {
      case Nil => z
      case Cons(x, xs) => f(x,foldRight(xs,z)(f))
    }
  }


  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (A, B) => B):B = {
    l match {
      case Nil => z
      case Cons(x,xs) => foldLeft(xs,f(x,z))(f)
    }
  }

  def sumViaFoldRight(l:List[Int]) = foldRight(l,0)(_ + _)
  def productViaFoldRight(l:List[Double]) = foldRight(l,1.0)(_ * _)
  def length[A](l :List[A]):Int = foldRight(l,0)((_,B) => 1 + B)

  def sumViaFoldLeft(l:List[Int]) = foldLeft(l,0)(_ + _)
  def productViaFoldLeft(l:List[Double]) = foldLeft(l,1.0)(_ * _)

  def reverse[A](l:List[A]):List[A] = foldLeft(l,Nil:List[A])((h,acc) => Cons(h,acc))

  def appendViaFoldRight[A](a1:List[A],a2:List[A]):List[A] = foldRight(a1,a2)(Cons(_,_))

  def concat[A](l:List[List[A]]):List[A] = {
    foldRight(l,Nil:List[A])(append)
  }

  def AddOne(l:List[Int]):List[Int] = foldRight(l,Nil:List[Int])((h,t) => Cons(h+1,t))
  def doubleToString(l:List[Int]):List[String] = foldRight(l,Nil:List[String])((h,t) => Cons(h.toString,t))

  def map[A,B](l:List[A])(f: A => B):List[B] = {
    foldRight(l,Nil:List[B])((h,t) => Cons(f(h),t))
  }

  def filter[A](l:List[A])(f: A => Boolean) = {
    foldRight(l,Nil:List[A])((h,t) => if(f(h)) Cons(h,t) else t)
  }

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
    concat(map(l)(f))
  }

  def filterViaFlatMap[A](l:List[A])(f: A => Boolean) = {
    flatMap(l)(a => if(f(a)) List(a) else Nil )
  }

  def addPairwise(a1:List[Int],a2:List[Int]):List[Int] = {
    (a1,a2) match  {
      case (_,Nil) => Nil
      case (Nil,_) => Nil
      case (Cons(h1,t1),Cons(h2,t2)) => Cons(h1+h2,addPairwise(t1,t2))
    }
  }

  def zipWith[A,B,C](a:List[A],b:List[B])(f:(A,B) => C):List[C] = {
    (a,b) match  {
      case (_,Nil) => Nil
      case (Nil,_) => Nil
      case (Cons(h1,t1),Cons(h2,t2)) => Cons(f(h1,h2),zipWith(t1,t2)(f))
    }
  }

  def eq[A](l: List[A], sub: List[A]): Boolean = {
    (l, sub) match {
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => eq(t1, t2)
      case _ => false
    }
  }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    (l,sub) match {
      case (_,Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => eq(t1, t2)
      case (Cons(h1,t1),_) => hasSubsequence(t1,sub)
    }
  }


}