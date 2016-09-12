package datastructures


sealed trait Tree[+A]

case class Leaf[A](value:A) extends Tree[A]

case class Branch[A](left:Tree[A],right:Tree[A]) extends Tree[A]

object Tree {

  def size[A](tr:Tree[A]):Int = {
    tr match {
      case Leaf(_) => 1
      case Branch(l,r) => 1 + size(l) + size(r)
    }
  }

  def maximum(tr:Tree[Int]):Int = {
    tr match {
      case Leaf(n) => n
      case Branch(l, r) => maximum(l) max maximum(r)
    }
  }

  def depth[A](tr:Tree[A]):Int = {
    tr match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }
  }

  def map[A,B](tr:Tree[A])(f:A => B):Tree[B] = {
      tr match {
        case Leaf(a) => Leaf(f(a))
        case Branch(l,r) => Branch(map(l)(f),map(r)(f))
      }
  }

  def fold[A,B](tr:Tree[A])(f:A => B)(g:(B,B) => B):B = {
    tr match {
      case Leaf(a) => f(a)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
  }

  def sizeViaFold[A](tr:Tree[A]):Int = fold[A,Int](tr)( a =>1)( 1 + _ + _)
  def maximumViaFold(tr:Tree[Int]):Int = fold(tr)( a => a)(_ max _)
  def depthViaFold[A](tr:Tree[A]):Int = fold(tr)(_ => 0)((l,r) => 1 + (l max r))
  def mapViaFold[A,B](tr:Tree[A])(f:A=>B):Tree[B] = fold(tr)(a => Leaf(f(a)):Tree[B])(Branch(_,_))

}