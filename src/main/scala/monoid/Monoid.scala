package monoid


trait Monoid[A] {
  def op(a:A,b:A):A
  def zero:A
}

object Monoid{

  val stringMonoid = new Monoid[String] {
    def op(a1:String,a2:String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1:List[A], a2:List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition:Monoid[Int] = new Monoid[Int] {
    def op(a1:Int,a2:Int) = a1 + a2
    val zero = 0
  }

  val intMultiplication:Monoid[Int] = new Monoid[Int] {

    def op(a1: Int, a2: Int): Int = a1 * a2
    val zero: Int = 1
  }

  val booleanOr:Monoid[Boolean] = new Monoid[Boolean] {

    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    val zero: Boolean = false
  }

  val booleanAnd:Monoid[Boolean] = new Monoid[Boolean] {

    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    val zero: Boolean = true
  }

  def optionMonoid[A]:Monoid[Option[A]] = new Monoid[Option[A]] {

    def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
    val zero: Option[A] = None
  }

  def dual[A](m:Monoid[A]):Monoid[A] = new Monoid[A] {

    def op(a1: A, a2: A): A = m.op(a2,a1)
    def zero: A = m.zero
  }

  def endoMonoid[A]:Monoid[A => A] = new Monoid[A => A] {

    def op(f: A => A, g: A => A): A => A = f.compose(g)
    def zero: A => A = a => a
  }

  import testing._
  import Prop._

  def monoidLaws[A](m:Monoid[A], gen:Gen[A]):Prop = forAll(
    //Associativity
    for{
      x <- gen
      y <- gen
      z <- gen
    } yield (x,y,z))( p =>
      m.op(p._1,m.op(p._2,p._3)) == m.op(m.op(p._1,p._2),p._3)) &&
    //Identity
    forAll(gen)((a:A) =>
      m.op(a,m.zero) == a && m.op(m.zero,a) == a
    )

  def concatenate[A](as:List[A], m:Monoid[A]):A = as.foldLeft(m.zero)(m.op)

  def foldMap[A,B](as:List[A], m:Monoid[B])(f: A=>B):B = {
    as.map(f).foldLeft(m.zero)(m.op)
  }

  def foldMapV2[A,B](as:List[A], m:Monoid[B])(f: A=>B):B = {
    as.foldLeft(m.zero)(
      (b:B,a:A) => m.op(b,f(a))
    )
  }

  def foldMapV3[A,B](as:List[A], m:Monoid[B])(f: A=>B):B = {
    as.foldRight(m.zero)(
      (a:A,b:B) => m.op(f(a),b)
    )
  }

  def foldMapV[A,B](as:IndexedSeq[A],m:Monoid[B])(f: A => B):B = {

    if(as.isEmpty)
      m.zero
    else if(as.length == 1)
      f(as.head)
    else{
      val (l,r) = as.splitAt(as.length/2)
      m.op(foldMapV(l,m)(f),foldMapV(r,m)(f))
    }
  }

  sealed trait WC
  case class Stub(chars:String) extends WC
  case class Part(lStub:String,word:Int,rStub:String) extends WC


  def wcMonoid:Monoid[WC] = new Monoid[WC] {

    val zero = Stub("")
    def op(a: WC, b: WC): WC = (a,b) match {
      case (Stub(c),Stub(d)) =>  Stub(c + d)
      case (Stub(c),Part(l,w,r)) => Part(c + l, w, r)
      case (Part(l,w,r),Stub(d)) => Part(l, w, r + d)
      case (Part(l1,w1,r1),Part(l2,w2,r2)) =>
        Part(l1, w1 + (if( (r1+l1).isEmpty) 0 else 1),r2)
  }
  }

  def count(s:String):Int = {
    def wc(c:Char):WC =
      if(c.isWhitespace)
        Part("",0,"")
      else
        Stub(c.toString)

    def unstub(s:String) = s.length min 1

    foldMapV(s.toCharArray,wcMonoid)(wc)  match {
      case Stub(z) => unstub(z)
      case Part(l,w,r) => unstub(l) + w + unstub(r)
    }
  }

  def productMonoid[A,B](A:Monoid[A],B:Monoid[B]):Monoid[(A,B)] = new Monoid[(A, B)] {

    override def op(a: (A, B), b: (A, B)): (A, B) = (A.op(a._1,b._1),B.op(a._2,b._2))
    override def zero: (A, B) = (A.zero,B.zero)
  }

  def mapMergeMonoid[K,V](V:Monoid[V]):Monoid[Map[K,V]] =
    new Monoid[Map[K, V]] {
      def zero: Map[K, V] = Map[K,V]()
      def op(a: Map[K, V], b: Map[K, V]): Map[K, V] =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero)))
        }
    }

  def functionMonoid[A,B](B:Monoid[B]):Monoid[A=>B] = new Monoid[A => B] {

    def op(f: A => B, g: A => B): A => B = a => B.op(f(a),g(a))
    def zero: A => B = _ => B.zero
  }

  def bag[A](as:IndexedSeq[A]):Map[A,Int] = {
    val m = mapMergeMonoid[A,Int](intAddition)
    as.foldLeft(m.zero)((b,a) => m.op(b,Map(a->1)))
  }
}

trait Foldable[F[_]] {
  def foldRight[A,B](as:F[A])(z:B)(f: (A,B) =>B): B
  def foldLeft[A,B](as:F[A])(z:B)(f: (B,A) =>B): B
  def foldMap[A,B](as:F[A])(f: A=>B)(mb:Monoid[B]): B
  def concatenate[A](as: F[A])(m:Monoid[A]):A = foldLeft(as)(m.zero)(m.op)
  def toList[A](fa:F[A]):List[A] = foldRight(fa)(List[A]())(_::_)
}


object ListFoldable extends Foldable[List]{

  def foldRight[A,B](as:List[A])(z:B)(f: (A,B) =>B): B = as.foldRight(z)(f)
  def foldLeft[A,B](as:List[A])(z:B)(f: (B,A) =>B): B = as.foldLeft(z)(f)
  def foldMap[A, B](as: List[A])(f: (A) => B)(mb: Monoid[B]): B = as.foldLeft(mb.zero)((b,a) => mb.op(b,f(a)))

}

object IndexSeqFoldable extends Foldable[IndexedSeq]{

  def foldRight[A,B](as:IndexedSeq[A])(z:B)(f: (A,B) =>B): B = as.foldRight(z)(f)
  def foldLeft[A,B](as:IndexedSeq[A])(z:B)(f: (B,A) =>B): B = as.foldLeft(z)(f)
  def foldMap[A, B](as: IndexedSeq[A])(f: (A) => B)(mb: Monoid[B]): B = as.foldLeft(mb.zero)((b,a) => mb.op(b,f(a)))

}

object StreamFoldable extends Foldable[Stream]{

  def foldRight[A,B](as:Stream[A])(z:B)(f: (A,B) =>B): B = as.foldRight(z)(f)
  def foldLeft[A,B](as:Stream[A])(z:B)(f: (B,A) =>B): B = as.foldLeft(z)(f)
  def foldMap[A, B](as: Stream[A])(f: (A) => B)(mb: Monoid[B]): B = as.foldLeft(mb.zero)((b,a) => mb.op(b,f(a)))

}

sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left:Tree[A],right:Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {

  def foldRight[A,B](as:Tree[A])(z:B)(f: (A,B) =>B): B = as match {
    case Leaf(a) => f(a,z)
    case Branch(left,right) => foldRight(left)(foldRight(right)(z)(f))(f)
  }
  def foldLeft[A,B](as:Tree[A])(z:B)(f: (B,A) =>B): B = as match {
    case Leaf(a) => f(z,a)
    case Branch(left,right) => foldLeft(right)(foldLeft(left)(z)(f))(f)
  }

  def foldMap[A, B](as: Tree[A])(f: (A) => B)(mb: Monoid[B]): B = as match {
    case Leaf(a) => f(a)
    case Branch(left,right) => mb.op(foldMap(left)(f)(mb),foldMap(right)(f)(mb))
  }

}

object OptionFoldable extends Foldable[Option] {
  def foldRight[A,B](as:Option[A])(z:B)(f: (A,B) =>B): B = as match {
    case None => z
    case Some(a) => f(a,z)
  }
  def foldLeft[A,B](as:Option[A])(z:B)(f: (B,A) =>B): B = as match {
    case None => z
    case Some(a) => f(z,a)
  }

  def foldMap[A, B](as: Option[A])(f: (A) => B)(mb: Monoid[B]): B = as match {
    case None => mb.zero
    case Some(a) => mb.op(mb.zero,f(a))
  }
}



object MonoidTest extends App{
  import Monoid._
  println(count("i am a jdk"))

  val M = mapMergeMonoid[String,Map[String,Int]](mapMergeMonoid[String,Int](intAddition))

  val m1 = Map("k1" -> Map("v1"->1,"v2"->2))
  val m2 = Map("k1" -> Map("v2"->3))
  println(M.op(m1,m2))

  println(bag(Vector("a","rose","is","a","rose")))

  val m = productMonoid(intAddition,intAddition)
  val p = ListFoldable.foldMap(List(1,2,3,4))(a => (1,a))(m)
  println(p._2/p._1.toDouble)

}