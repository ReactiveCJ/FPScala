package parallelism


import java.util.concurrent._
import language.implicitConversions

object Par extends App{

  type Par[A] = ExecutorService => Future[A]
  def run[A](s:ExecutorService)(a:Par[A]):Future[A] = a(s)

  def unit[A](a:A) = (es:ExecutorService) => UnitFuture(a)

  case class UnitFuture[A](get:A) extends Future[A] {

    override def isDone: Boolean = true

    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def get(timeout: Long, unit: TimeUnit): A = get

    override def isCancelled: Boolean = false

  }

  def map2[A,B,C](a:Par[A],b:Par[B])(f:(A,B) => C):Par[C] = {

    (es:ExecutorService) =>
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get(),bf.get()))
  }

  def fork[A](a: => Par[A]):Par[A] =
    es => es.submit( new Callable[A] {
      override def call(): A = a(es).get() //need one more thread
    })

  def lazyUnit[A](a: => A):Par[A] = fork(unit(a))

  def asyncF[A,B](f: A => B):A => Par[B] = {
    a =>
      lazyUnit(f(a))
  }

  def map[A,B](pa:Par[A])(f: A => B):Par[B] =
    map2(pa,unit())((a,_) => f(a))


  def sequenceSimple[A](ps:List[Par[A]]):Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(List()))((h,t) => map2(h,t)(_ :: _))

  def sequenceRight[A](ps:List[Par[A]]):Par[List[A]] = {

    ps match {
      case Nil => unit(List[A]())
      case h::t => map2(h,fork(sequenceRight(t)))(_::_)
    }
  }

  def sequenceBalance[A](ps:IndexedSeq[Par[A]]):Par[IndexedSeq[A]] = {
    if (ps.isEmpty)
      unit(Vector())
    else if(ps.length <= 1)
      map(ps.head)( a => Vector(a))
    else {
      val (l, r) = ps.splitAt(ps.length / 2)
      map2(sequenceBalance(l),sequenceBalance(r))(_ ++ _)
    }
  }

  def sequence[A](ps:List[Par[A]]):Par[List[A]] =
    map(sequenceBalance(ps.toIndexedSeq))(_.toList)


  def parMap[A,B](ps:List[A])(f :A => B):Par[List[B]] = {

    val fbs:List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }


  def parFilter[A](as:List[A])(f: A => Boolean):Par[List[A]] = {
    val fas:List[Par[List[A]]] = as.map( asyncF( a => if(f(a)) List(a) else List()))
    map(sequence(fas))( _.flatten)
  }

  def equals[A](e:ExecutorService)(p:Par[A],p2:Par[A]):Boolean = p(e).get() == p2(e).get()

  def delay[A](fa: => Par[A]):Par[A] = es => fa(es)

  val a = lazyUnit(42 + 1)
  val b = unit(40+3)
  val es = Executors.newFixedThreadPool(1)
  //dead lock
  //println(equals(es)(fork(a),b))

  def choiceN[A](n:Par[Int])(choices:List[Par[A]]):Par[A] = {
    es =>
      val ind = n(es).get()
      choices(ind)(es)
  }

  def choiceMap[K,V](key:Par[K])(choices:Map[K,Par[V]]):Par[V] = {
    es =>
      val k = run(es)(key).get()
      choices(k)(es)
  }


  def chooser[A,B](pa:Par[A])(choices:A => Par[B]):Par[B] = {
    es =>
      choices(run(es)(pa).get())(es)
  }

  def flatMap[A,B](a:Par[A])(f:A => Par[B]):Par[B] = {
    es =>
      f(a(es).get())(es)
  }

  def join[A](a:Par[Par[A]]):Par[A] = {
    es =>
      run(es)(a).get()(es)
  }

  def joinViaFlatMap[A](a:Par[Par[A]]):Par[A] = flatMap(a)(x => x)

  def flatMapViaJoin[A,B](a:Par[A])(f:A => Par[B]):Par[B] = join(map(a)(f))


  def _map2[A,B,C](a:Par[A],b:Par[B])(f:(A,B) => C):Par[C] = flatMap(a){ x => flatMap(b) { y => unit[C](f(x, y)) } }




}