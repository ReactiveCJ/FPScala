package state

import RNG._

import scala.annotation.tailrec

object Exercise extends App{

  val rng = RNG.simple(10000l)

  def positiveInt(rng:RNG):(Int,RNG) = {
    val (i,r) = rng.nextInt
    (if(i<0) -(i+1) else i, r)
  }

  def double(rng:RNG):(Double,RNG) = {
    val (i,r) = positiveInt(rng)
    (i /(Int.MaxValue.toDouble + 1),r)
  }

  def intDouble(rng:RNG):((Int,Double),RNG) = {
    val (i,r) = rng.nextInt
    val (d,r2) = double(r)
    ((i,d),r2)
  }

  def doubleInt(rng:RNG):((Double,Int),RNG) = {
    val (d,r) = double(rng)
    val (i,r2) = r.nextInt
    ((d,i),r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }


  val int: Rand[Int] = rng => rng.nextInt

  def ints(count: Int)(rng: RNG): (List[Int],RNG) = {

    @tailrec
    def go(count:Int,r:RNG,xs:List[Int]):(List[Int],RNG) = {
      if(count < 1)
        (xs,r)
      else{
        val (x,r2) = r.nextInt
        go(count -1,r2,x::xs)
      }
    }
    go(count,rng,List())
  }

  def positiveMax(n: Int): Rand[Int] = map(double)( a => (a * n).toInt)
  def _double:Rand[Double] = map(positiveInt)( i => i/ (Int.MaxValue.toDouble + 1))

  def intDoubleViaMap2:Rand[(Int,Double)] = map2(_.nextInt,double)((a,b) => (a,b))

  def _ints(count: Int): Rand[List[Int]] =
    sequence3(List.fill(count)(int))

  def nonNegativeLessThan(n:Int):Rand[Int] = {
    flatMap(positiveInt){ r =>
      val mod = r % n
      if(r + n -1 - mod >=0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  val mac = Machine(true,10,10)
  val x = Candy.simulateMachine(List(Coin,Coin,Turn)).action(mac)
  val y = Candy._simulateMachine(List(Coin,Coin,Turn)).action(mac)

  println(x)
  println(y)
}
