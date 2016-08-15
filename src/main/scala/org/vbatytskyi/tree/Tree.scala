package org.vbatytskyi.tree

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
case object Nil extends Tree[Nothing]

object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
    case Nil => 0
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def max(tree: Tree[Int]): Int = tree match {
    case Nil => throw new IllegalArgumentException
    case Leaf(value) => value
    case Branch(l, r) => Math.max(max(l), max(r))
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Nil => 0
    case Leaf(_) => 1
    case Branch(l, r) => 1 + Math.max(depth(l), depth(r))
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Nil => Nil
    case Leaf(value) => Leaf(f(value))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](tree: Tree[A], z: B)(f: A => B)(f2: (B, B) => B): B = tree match {
    case Nil => z
    case Leaf(value) => f(value)
    case Branch(l, r) => f2(fold(l, z)(f)(f2), fold(r, z)(f)(f2))
  }
}
