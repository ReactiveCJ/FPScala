package errorhandling

import java.util.regex._


object ExerciseWithOption extends App{

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



}
