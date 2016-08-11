package chapter6


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


  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
     rng => {
       fs.foldLeft((List[A](),rng))(
         (l:(List[A],RNG),ra:Rand[A]) => {
           val (b,rng2) = ra(l._2)
           println(l._1)
           println(l._1.+:(b))
           (l._1.+:(b), rng2)
         }
       )
     }
  }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def sequence3[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      fs.foldRight((List[A](),rng))(
        (ra:Rand[A],l:(List[A],RNG)) => {
          val (b,rng2) = ra(l._2)
          (l._1.+:(b), rng2)
        }
      )
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


}


