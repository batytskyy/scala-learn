package org.vbatytskyi.option

sealed trait Option[+A] {

  def isDefined: Boolean
  def get: A

  def map[B](f: A => B): Option[B] = this match {
    case Some(value) => Some(f(value))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(value) => value
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = if (this.isDefined) this else ob

  def filter(f: A => Boolean): Option[A] = if (this.isDefined && f(get)) this else None
}

case object None extends Option[Nothing] {
  override def isDefined = false
  override def get: Nothing = throw new RuntimeException
}

case class Some[+A](value: A) extends Option[A] {
  override def get: A = value
  override def isDefined = true
}
