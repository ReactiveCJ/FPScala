package datastructures

import datastructures.List._
import datastructures.Tree._

object Exercise {

  def main(args: Array[String]) {

    /*
    val z = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    println(z)

    val p = List.dropWhile[Int](List(1,2,3,4,5))( x => x%2 ==0)
    println(p)

    println(init(List(1,2,3,4,5)))

    println(foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))

    val l:List[Double] = List(1,2,3,4,5)
    println(reverse(l))

    println(appendViaFoldRight(List(6,7,8),l))
    println(concat(List(List(1,2,3,4),List(5,6,7))))

    println(addPairwise(List(1,2,3,4),List(5,6,7,8)))

    println(zipWith(List(1,2,3,4),List(5,6,7,8))(_+_))

    println(hasSubsequence(List(1,2,3,4),List(2,3)))
    println(hasSubsequence(List(1,2,3,4),List(1,4)))
    println(hasSubsequence(List(1,2,3,4),List(2,4)))
    println(hasSubsequence(List(1,2,3,4),List(3)))
*/

    val leaf1 = Leaf(1)
    val leaf2 = Leaf(2)
    val leaf3 = Leaf(3)
    val leaf4 = Leaf(4)

    val m1 = Branch(leaf1,leaf2)
    val m2 = Branch(leaf3,leaf4)
    val r = Branch(m1,m2)
    println(size(r))
    println(maximum(r))
    println(depth(r))
    println(Tree.map(r)(_*3))

    println(sizeViaFold(r))
    println(maximumViaFold(r))
    println(depthViaFold(r))
    println(Tree.mapViaFold(r)(_*3))
  }
}
