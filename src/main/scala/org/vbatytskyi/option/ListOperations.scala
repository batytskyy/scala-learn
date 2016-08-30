package org.vbatytskyi.option

object ListOperations {

  def variance(xs: Seq[Double]): scala.Option[Double] = {
    xs.reduceOption(_ + _)
      .map(_ / xs.length)
      .map(m => xs.map(x => (x - m) * (x - m)))
      .map(_.sum / xs.length)
  }

}
