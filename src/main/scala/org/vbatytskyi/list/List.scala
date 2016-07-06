package org.vbatytskyi.list

import scala.annotation.tailrec

sealed trait List[+T]

case object Nil extends List[Nothing]
case class Cons[+T](head: T, tail: List[T]) extends List[T]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // may be rewritten as drop(list, 1)
  def tail[A](list: List[A]): List[A] = list match {
    case Nil => throw new IllegalArgumentException("List is empty")
    case Cons(_, t) => t
  }

  def setHead[A](list: List[A], el: A): List[A] = list match {
    case Nil => throw new IllegalArgumentException("List is empty")
    case Cons(_, t) => Cons(el, t)
  }

  def size[A](list: List[A]): Int = {
    def size(as: List[A], acc: Int): Int = as match {
      case Nil => acc
      case Cons(_, tail) => size(tail, acc + 1)
    }
    size(list, 0)
  }

  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (_, n1) if n1 < 0 => throw new IllegalArgumentException("Illegal index")
    case (_, 0) => l
    case (Nil, _) => throw new IllegalArgumentException("List is too short")
    case (Cons(_, t), _) => drop(t, n - 1)
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => l
  }

  def append[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Cons(h, t) => Cons(h, append(t, l2))
    case Nil => l2
  }

  def reverse[A](l: List[A]): List[A] = {
    @tailrec
    def reverse(l: List[A], acc: List[A]): List[A] = l match {
      case Nil => acc
      case Cons(h, t) => reverse(t, Cons(h, acc))
    }
    reverse(l, Nil)
  }

  def init[A](l: List[A]): List[A] = {
    @tailrec
    def init(l: List[A], acc: List[A]): List[A] = l match {
      case Nil => acc
      case Cons(h, Nil) => reverse(acc)
      case Cons(h, t) => init(t, Cons(h, acc))
    }
    init(l, Nil)
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def foldRightTail[A, B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(l), z)((a, b) => f(b, a))

  def length[A](l: List[A]): Int = foldRight(l, 0)((a, b) => 1 + b)

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =  l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sum(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def product(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

  def foldLeftLength[A](l: List[A]): Int = foldLeft(l, 0)((b, a) => b + 1)

  def foldLeftReverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((b, a) => Cons(a, b))

  def foldLeftUsingFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(reverse(l), z)((a, b) => f(b, a))

  def foldRightUsingFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(l), z)((b, a) => f(a, b))

  def foldLeftAppend[A](l1: List[A], l2: List[A]): List[A] = foldLeft(l2, l1)((b: List[A], a) => l1 match {
    case Nil => l2
    case Cons(h, t) => Cons(h, foldLeftAppend(t, l2))
  })

  def appendMultiple[A](lists: List[List[A]]): List[A] = lists match {
    case Nil => Nil
    case Cons(h, t) => append(h, appendMultiple(t))
  }

  def addOne(list: List[Int]): List[Int] = foldRightTail(list, Nil: List[Int])((a, b) => Cons(a + 1, b))

  def toString(list: List[Double]): List[String] = foldRightTail(list, Nil: List[String])((a, b) => Cons(a + "", b))

  def map[A, B](list: List[A])(f: A => B): List[B] = foldRightTail(list, Nil: List[B])((a, b) => Cons(f(a), b))

  def filter[A](list: List[A])(f: A => Boolean): List[A] = list match {
    case Nil => Nil
    case Cons(h, t) if f(h) => Cons(h, filter(t)(f))
    case Cons(h, t) => filter(t)(f)
  }

  def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = appendMultiple(map[A, List[B]](list)(a => f(a)))

  def filterByFlatMap[A](list: List[A])(f: A => Boolean): List[A] = flatMap(list)(a => if (f(a)) List(a) else Nil)

  def zipAdd(list1: List[Int], list2: List[Int]): List[Int] = (list1, list2) match {
    case (Nil, Nil) => Nil
    case (Nil, _) => list2
    case (_, Nil) => list1
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zipAdd(t1, t2))
  }

  def zipWith[A](list1: List[A], list2: List[A])(f: (A, A) => A): List[A] = (list1, list2) match {
    case (Nil, Nil) => Nil
    case (Nil, _) => list2
    case (_, Nil) => list1
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = {
    true
  }
}
