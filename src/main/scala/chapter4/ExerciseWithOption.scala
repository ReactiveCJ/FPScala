package chapter4

import java.util.regex._


object ExerciseWithOption extends App{

  def mean(xs:Seq[Double]):Option[Double] = {
    if(xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  def variance(xs:Seq[Double]):Option[Double] = {
    mean(xs) flatMap( m => mean(xs.map(x => math.pow(x - m,2))))
  }

  def pattern(s:String):Option[Pattern] = {
    try {
      Some(Pattern.compile(s))
    }catch {
      case e:PatternSyntaxException => None
    }
  }

  def mkMatcher(pat:String):Option[String => Boolean] = {
    pattern(pat) map (p => (s:String) => p.matcher(s).matches())
  }

  def mkMatcher_1(pat:String):Option[String => Boolean] = {
    for{
      p <- pattern(pat)
    } yield (s:String) => p.matcher(s).matches()
  }

  def doesMatch(pat:String,s:String):Option[Boolean] = {
    for{
      p <- mkMatcher_1(pat)
    } yield p(s)
  }

  def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] =
    for {
      f <- mkMatcher(pat)
      g <- mkMatcher(pat2)
    } yield f(s) && g(s)


  def bothMatch_1(pat: String, pat2: String, s: String): Option[Boolean] = {
    mkMatcher(pat) flatMap (f =>
      mkMatcher(pat2) map (g =>
        f(s) && g(s)))
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for{
      p <- a
      q <- b
    } yield f(p,q)
  }

  def bothMatch_2(pat: String, pat2: String, s: String): Option[Boolean] = {
    map2(mkMatcher(pat),mkMatcher(pat2))((f,g) => f(s) && g(s))
  }

  def sequence[A](a:List[Option[A]]):Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case h::t => h flatMap(hh => sequence(t) map  (hh :: _) )
    }
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

  def sequenceViaTraverse[A](a:List[Option[A]]):Option[List[A]] = {
    traverse(a)(x => x)
  }

  //test sequence
  val a = Some(1)
  val b = Some(2)
  val c = None
  println(map2(a,b)(_+_))
  val l = List(a,b,c)
  println(sequenceViaTraverse(l))

  //test traverse
  val l1 = List(1,2,3,4,5)
  def f(x:Int) = {
    if(x < 1)
      None
    else
      Some(x * 3)
  }
  println(traverse(l1)(f))


}
