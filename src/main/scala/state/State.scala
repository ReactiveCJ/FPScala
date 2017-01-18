package state

import scala.annotation.tailrec

trait RNG {

  def nextInt:(Int,RNG)

}


object RNG {


  case class simple(seed:Long) extends RNG {
    def nextInt:(Int,RNG) = {
      val seed2 = (seed*0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
      ((seed2 >>>16).asInstanceOf[Int],simple(seed2))
    }
  }

  def nonNegativeInt(rng:RNG):(Int,RNG) = {
    val (i,r) = rng.nextInt
    (if(i < 0) -(i+1) else i, r)
  }

  def double(rng:RNG):(Double,RNG) = {

    val (i,r) = nonNegativeInt(rng)
    (i/(Double.MaxValue+1),r)
  }

  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match { case (i,rng2) => (i%2==0,rng2) }

  def intDouble(rng:RNG):((Int,Double),RNG) = {

    val (i,r1) = rng.nextInt
    val (j,r2) = double(r1)
    ((i,j),r2)
  }

  def doubleInt(rng:RNG):((Double,Int),RNG) = {

    val (d,r1) = double(rng)
    val (i,r2) = r1.nextInt
    ((d,i),r2)
  }

  def double3(rng:RNG):((Double,Double,Double),RNG) = {

    val (d1,r1) = double(rng)
    val (d2,r2) = double(r1)
    val (d3,r3) = double(r2)
    ((d1,d2,d3),r3)
  }

  def ints(count:Int)(rng:RNG):(List[Int],RNG) = {

    if(count==0)
      (List(),rng)
    else{
      val (x,r1) = rng.nextInt
      val (xs,r2) = ints(count-1)(r1)
      (x::xs,r2)
    }
  }

  def ints2(count:Int)(rng:RNG):(List[Int],RNG) = {

    @tailrec
    def go(count:Int,rng:RNG,xs:List[Int]):(List[Int],RNG) = {
      if(count == 0)
        (xs,rng)
      else{
        val (x,r1) = rng.nextInt
        go(count -1 ,r1,x::xs)
      }
    }
    go(count,rng,List())
  }

  type Rand[+A] = RNG => (A,RNG)

  val int:Rand[Int] = _.nextInt

  def unit[A](a:A):Rand[A] = rng => (a,rng)


  def map[A,B](s:Rand[A])(f:A => B):Rand[B] = rng => {
    val (a,rng2) = s(rng)
    (f(a),rng2)
  }

  def nonNegativeEven:Rand[Int] = map(int)(i => i - i%2)

  def _double:Rand[Double] = map(nonNegativeInt)(_ / (Double.MaxValue+1))

  def map2[A,B,C](ra:Rand[A],rb:Rand[B])(f:(A,B) => C):Rand[C] = {
      rng =>
        val (a,r1) = ra(rng)
        val (b,r2) = rb(r1)
        (f(a,b),r2)
  }

  def both[A,B](ra:Rand[A],rb:Rand[B]):Rand[(A,B)] = map2(ra,rb)((_,_))

  def randIntDouble:Rand[(Int,Double)] = both(int,double)

  def sequence[A](fs:List[Rand[A]]):Rand[List[A]] = {
    rng => {
      fs.foldLeft((List[A](),rng)) {
        (l: (List[A], RNG), ra: Rand[A]) =>
          val (a, r1) = ra(l._2)
          (l._1.+:(a),r1)
      }
    }
  }

  def sequence2[A](fs:List[Rand[A]]):Rand[List[A]] = {
    rng =>
      fs.foldRight((List[A](),rng)){
        (ra:Rand[A],l:(List[A],RNG)) =>
          val (a,r1) = ra(l._2)
          (l._1.+:(a),r1)
      }
  }

  def sequence3[A](fs:List[Rand[A]]):Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))(
      (ra,f) => map2(ra,f)(_::_)
    )
  }

  def _ints(count:Int):Rand[List[Int]]= {
    sequence(List.fill(count)( _.nextInt))
  }

  def flatMap[A,B](s:Rand[A])(g: A => Rand[B]):Rand[B] = {

    rng => {
      val (a,r1) = s(rng)
      g(a)(r1)
    }
  }

  def nonNegativeLessThan(n:Int):Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if(i + n - 1 - mod >=0 )
        unit(mod)
      else
        nonNegativeLessThan(n)
    }
  }

  def _map[A,B](s:Rand[A])(f: A => B ):Rand[B] = flatMap(s)( a => rng => (f(a),rng))

  def _map2[A,B,C](ra:Rand[A],rb:Rand[B])(f:(A,B) => C):Rand[C] =  flatMap(ra){ a => map(rb)( b => f(a,b) ) }


}

case class State[S,+A](run: S => (A,S)) {

  def map[B](f: A => B):State[S,B] = State {
    s =>
      val (a, s1) = run(s)
      (f(a),s1)
  }


  def flatMap[B](f: A => State[S,B]):State[S,B] = {
    State {
      s =>
        val (a,s1) = run(s)
        f(a).run(s1)
    }
  }

  def map2[B,C](sb:State[S,B])(f: (A,B) => C):State[S,C] = {
    State {
      s =>
        val (a,s1) = run(s)
        val (b,s2) = sb.run(s1)
        (f(a,b),s2)
    }
  }

  def _map2[B,C](sb:State[S,B])(f: (A,B) => C):State[S,C] = flatMap( a => sb.map( b => f(a,b)))


}


object State {

  def unit[S,A](a: => A):State[S,A] = State( s => (a,s) )

  def sequence[S,A](fs:List[State[S,A]]):State[S,List[A]] = {

    @tailrec
    def go(s:S,fs:List[State[S,A]],acc:List[A]):(List[A],S) = {
      fs match {
        case Nil => (acc.reverse,s)
        case h::t =>
          val (a,s1) = h.run(s)
          go(s1,t,a :: acc)
      }
    }
    State{ s => go(s,fs,List()) }
  }

  def _sequence[S,A](fs:List[State[S,A]]):State[S,List[A]] = {
    fs.foldRight(unit[S,List[A]](List())){
      (f,acc) => f.map2(acc)(_ :: _)
    }

  }

  def get[S]:State[S,S] = State( s => (s,s))

  def set[S](s:S):State[S,Unit] = State(_ => ((),s))

  def modify[S](f: S => S):State[S,Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def _modify[S](f: S => S):State[S,Unit] = get.flatMap{ s => set(f(s)) }

}

import State._

sealed trait Input

case object Coin extends Input
case object Turn extends Input

case class Machine(locked:Boolean,candies:Int,coins:Int)

object Candy extends App{

  def update = (i:Input) => (s:Machine) =>
    (i,s) match {

      case (_,Machine(_,0,_)) => s
      case (Coin, Machine(false,_,_)) => s
      case (Turn, Machine(true,_,_)) => s
      case (Coin,Machine(true,candy,coin)) => Machine(false,candy,coin + 1)
      case (Turn,Machine(false,candy,coin)) => Machine(false,candy - 1,coin)
    }


  def simulateMachine(inputs: List[Input]):State[Machine,(Int,Int)] = for {
    x <- State.sequence(
      inputs.map{
          val b:(Machine => Machine) => State[Machine,Unit] = modify[Machine]
          b.compose(update)
      }
    )
    s <- get
  } yield (s.candies,s.coins)

  def _simulateMachine(inputs: List[Input]) = State.sequence(inputs map (modify[Machine] _ compose update) ).flatMap{
    _ =>
      get.map(
        s =>
          (s.candies,s.coins)
      )
  }

  val a = modify[Machine] _

  val b:(Machine => Machine) => State[Machine,Unit] = modify[Machine]

  val t = List(Coin,Turn,Turn,Coin)

  println(simulateMachine(t).run(Machine(true,10,10))._1)

}