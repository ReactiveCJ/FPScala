package testing
import java.util.concurrent.{ExecutorService, Executors, Future}
import state._
import laziness._
import Prop._
import Gen._
import SGen._
import parallelism.Par
import parallelism.Par.{Par, UnitFuture}

sealed trait Result{
  def isFalsified:Boolean
}

case object Passed extends Result {
  def isFalsified = false
}

case object Proved extends Result {
  def isFalsified = false
}

case class Falsified(failure:FailedCase,successes:SuccessCount) extends Result {
  def isFalsified = true
}

case class Gen[A](sample:State[RNG,A]){

  def map[B](f: A => B):Gen[B] = Gen(sample.map( a => f(a)))

  def map2[B,C](g:Gen[B])(f:(A,B) => C):Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def flatMap[B](f: A => Gen[B]):Gen[B] = Gen(sample.flatMap( a => f(a).sample))

  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)

  def listOfN(size:Gen[Int]):Gen[List[A]] = size.flatMap(listOfN)

  def unsized:SGen[A] = SGen(_ => this)

  def **[B](g:Gen[B]):Gen[(A,B)] = (this map2 g)((_,_))
}

object Gen{

  def choose(start:Int,stopExclusive:Int):Gen[Int] = Gen(
    State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start))
  )

  def unit[A](a: => A):Gen[A] = Gen(State.unit[RNG,A](a))

  def boolean:Gen[Boolean] = Gen(State(RNG.boolean))

  def listOfN[A](n:Int,g:Gen[A]):Gen[List[A]] = {
    Gen(State.sequence(List.fill(n)(g.sample)))
  }

  def union[A](g1:Gen[A],g2:Gen[A]):Gen[A] = {
    boolean.flatMap{
      b =>
        if(b) g1 else g2
    }
  }

  def weighted[A](g1:(Gen[A],Double),g2:(Gen[A],Double)):Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(State(RNG.double).flatMap{
      d =>
        if(d < g1Threshold)
          g1._1.sample
        else
          g2._1.sample

    })
  }


}


case class SGen[A](g:Int => Gen[A]){

  def apply(n:Int):Gen[A] = g(n)

  def map[B](f: A => B):SGen[B] =
    SGen{ g(_) map f }

  def flatMap[B](f:A => SGen[B]):SGen[B] = {
    SGen{
      n =>
        g(n).flatMap(f(_).g(n))
    }
  }
}

object SGen {
  def listOf[A](g:Gen[A]):SGen[List[A]] = SGen(g.listOfN)
  def listOfN1[A](g:Gen[A]):SGen[List[A]] = {
    SGen(n => g.listOfN(n max 1))
  }
}

case class Prop(run:(MaxSize,TestCases,RNG) => Result) {

  def &&(p:Prop):Prop = Prop{
    (m,n,rng) => run(m,n,rng) match {
      case Passed | Proved => p.run(m,n,rng)
      case x => x
    }
  }

  def ||(p:Prop) = Prop{
    (m,n,rng) => run(m,n,rng) match {
      case Falsified(msg, _) => p.tag(msg).run(m,n,rng)
      case x => x
    }
  }


  def tag(msg:String) = Prop{
    (m,n,rng) => run(m,n,rng) match {
      case Falsified(e,c) => Falsified(msg+"\t"+e,c)
      case x => x
    }
  }

}

object Prop{

  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int
  type MaxSize = Int


  def forAll[A](as:Gen[A])(f:A =>Boolean):Prop = Prop(
    (m,n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map{
      case (a,i) =>
        try{
          if(f(a)) Passed else Falsified(a.toString,i)
        } catch {case e:Exception => Falsified(buildMsg(a,e),i)}
    }.find(_.isFalsified).getOrElse(Passed)
  )

  def forAll[A](g:SGen[A])(f: A => Boolean):Prop = {
    forAll(g(_))(f)
  }

  def forAll[A](g:Int => Gen[A])(f: A => Boolean):Prop = Prop{
    (max,n,rng) =>
      val casePerSize = (n+max-1)/max
      val props:Stream[Prop] = Stream.from(0).take(n.min(max) +1).map( i => forAll(g(i))(f))
      val prop:Prop = props.map{
        p => Prop{
          (max, _, rng) =>
            p.run(max,casePerSize,rng)
        }}.toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

  val S = weighted(
    choose(3,4).map(Executors.newFixedThreadPool) -> 0.75,
    unit(Executors.newCachedThreadPool()) -> 0.25
  )

  object ** {
    def unapply[A,B](arg: (A,B)): Option[(A,B)] = Some(arg)
  }

  def forAllPar[A](g:Gen[A])(f: A => Par[Boolean]):Prop = forAll( S ** g){case s ** a => f(a)(s).get()}


  def randomStream[A](g:Gen[A])(rng:RNG):Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s:A,e:Exception):String =
    s"test cast: $s\n" +
      s"generated an exception:${e.getMessage}\n"+
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"


  def run(p:Prop,maxSize: Int = 100,testCases:Int = 100,rng:RNG = RNG.simple(System.currentTimeMillis)):Unit = {
    p.run(maxSize,testCases,rng) match {
      case Falsified(msg,n) =>
        println(s"!Falsified after $n passed tests: $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
      println(s"+ OK, passed proved property.")
  }
  }

  def check(p: => Boolean):Prop = Prop{
    (_,_,_) =>
      if(p) Proved else Falsified("()",0)
  }

  def checkPar(p:Par[Boolean]):Prop = forAllPar(Gen.unit(()))(_ => p)

}

object Test extends App {

  val prop = forAll(Gen.choose(0, 5))(_ < 3)
  Prop.run(prop)

  val smallInt = Gen.choose(-10, 10)
  val sortedProp = forAll(listOf(smallInt)) { l =>
    val ls = l.sorted
    l.isEmpty || ls.tail.isEmpty || !ls.zip(ls.tail).exists { case (a, b) => a > b }
  }
  Prop.run(sortedProp)

  val ES = Executors.newCachedThreadPool()
  val p1 = Prop.forAll(Gen.unit(Par.unit(1))) { i =>
    Par.map(i)(_ + 1)(ES).get() == Par.unit(2)(ES).get
  }

  val p2 = Prop.check{
    val p = Par.map(Par.unit(1))(_+1)
    val p2 = Par.unit(2)
    p(ES).get == p2(ES).get
  }
  Prop.run(p2)

  def equal[A](p:Par[A], p2:Par[A]):Par[Boolean] = {
    Par.map2(p,p2)(_ == _)
  }

  val p3 = check{
    equal(
      Par.map(Par.unit(1))(_+1),
      Par.unit(2)
    )(ES).get()
  }
  Prop.run(p3)

  val p4 = checkPar{ equal (
    Par.map(Par.unit(1))(_+1),
    Par.unit(2)
  )}

  Prop.run(p4)

  val pint:Gen[Par[Int]] = Gen.choose(0,10).map(Par.unit)
  val p5 = forAllPar(pint)(n => equal(Par.map(n)(y =>y),n))
  Prop.run(p5)

  lazy val pint2: Gen[Par[Int]] = choose(-100,100).listOfN(choose(0,3)).map(l =>
    l.foldLeft(Par.unit(0))((p,i) =>
      Par.fork { Par.map2(p, Par.unit(i))(_ + _) })
  )

  val forkProp = Prop.forAllPar(pint2)(i => equal(Par.fork(i), i)) tag "fork"
  Prop.run(forkProp,2,2)


}

