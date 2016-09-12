package state


trait RNG {

  def nextInt:(Int,RNG)

}


object RNG {

  def simple(seed:Long):RNG = new RNG{
    def nextInt = {
      val seed2 = (seed*0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
      ((seed2 >>>16).asInstanceOf[Int],simple(seed2))
    }
  }

  type Rand[+A] = RNG => (A,RNG)


  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a,rng2) = ra(rng)
      val (b,rng3) = rb(rng2)
      (f(a,b),rng3)
    }
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {

    rng => {
      val (a,rng2) = f(rng)
      g(a)(rng2)
    }
  }

  def _map[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)( a => unit(f(a)))
  }

  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)( a=> map(rb)(b=>f(a,b)))
  }


  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      fs.foldLeft((List[A](),rng))(
        (l:(List[A],RNG),ra:Rand[A]) => {
          val (b,rng2) = ra(l._2)
          (l._1.+:(b), rng2)
        }
      )
    }
  }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      fs.foldRight((List[A](),rng))(
        (ra:Rand[A],l:(List[A],RNG)) => {
          val (b,rng2) = ra(l._2)
          (l._1.+:(b), rng2)
        }
      )
    }
  }

  def sequence3[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))



}
import State._
case class State[S,+A](action: S => (A,S)) {

  def map[B](f: A => B):State[S,B] = State(
    s => {
      val (a, s1) = action(s)
      (f(a),s1)
    }
  )

  def flatMap[B](f:A => State[S,B]): State[S, B] = {
    State(
      s => {
        val (a,s1) = action(s)
        f(a).action(s1)
      }
    )
  }

  def _map[B](f: A => B):State[S,B] = flatMap( a => unit(f(a)))

  def _map2[B,C](sb: State[S,B])(f: (A, B) => C): State[S,C] = {
    flatMap( a => sb.map( b => f(a,b)) )
  }


}

object State {

  type Rand[+A] = State[RNG,A]

  def unit[S,A](a: A): State[S,A] =
    State(s => (a,s))

  def sequence[S,A](sas: List[State[S,A]]): State[S,List[A]] = {
    sas.foldRight(unit[S,List[A]](List()))((f,acc) => f._map2(acc)(_ :: _) )
  }


  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]:State[S,S] = State(s => (s,s))

  def set[S](s:S):State[S,Unit] = State(_ => ((),s))

  def _modify[S](f:S => S):State[S,Unit] = get.flatMap( s => set( f(s)))

}

sealed trait Input

case object Coin extends Input
case object Turn extends Input

case class Machine(locked:Boolean,candies:Int,coins:Int)

object Candy {

  def update = (i:Input) => (s:Machine) =>
    (i,s) match {
      case (_,Machine(_,0,_)) => s
      case (Coin, Machine(false,_,_)) => s
      case (Turn, Machine(true,_,_)) => s
      case (Coin, Machine(true,candy,coin)) => Machine(false,candy,coin + 1)
      case (Turn,Machine(false,candy,coin)) => Machine(true,candy - 1,coin)
    }



  def simulateMachine(inputs: List[Input]) = for {
    x <- State.sequence(inputs map (_modify[Machine] _ compose update)  )
    s <- get
  } yield (s.candies,s.coins)

  def _simulateMachine(inputs: List[Input]) = State.sequence(inputs map (_modify[Machine] _ compose update) ).flatMap{
    _ =>
      get.map(
        s =>
          (s.candies,s.coins)
      )
  }

}

