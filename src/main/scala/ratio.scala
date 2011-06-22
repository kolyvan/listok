// this code taken from github.com/derekjw/scala-ratio-datatype
// author: derekjw (Derek Williams)

package net.fyrie {

package object ratio {
  implicit def bigInt2Ratio[T <% BigInt](in: T): Ratio = Ratio(in)
}

package ratio {

import math._

object Ratio {

  private val cache: Map[(BigInt, BigInt), Ratio] = Map() ++
    (1 to 10).flatMap(n => (1 to 10).map(d => ((BigInt(n),BigInt(d)), create(n,d)))) ++
    List(15, 30, 60, 125, 250, 500, 1000).map(d => ((BigInt(1), BigInt(d)), create(1, d)))

  private def create(n: BigInt, d: BigInt) = d.signum match {
      case -1 => apply(-n, -d)
      case 0 => throw new IllegalArgumentException("Zero denominator")
      case 1 if (n == 1 || d == 1) => new Ratio(n, d)
      case _ => 
        val g = n.gcd(d)
        new Ratio(n / g, d / g)
  }

  def apply(in: String): Ratio =
    in.split("/").toList.map(_.trim) match {
      case n :: d :: Nil => apply(BigInt(n),BigInt(d))
      case n :: Nil => apply(BigInt(n))
      case _ => apply(0)
    }

  def apply(n: BigInt): Ratio = cache.getOrElse((n,1), new Ratio(n, 1))

  def apply(n: BigInt, d: BigInt): Ratio = cache.getOrElse((n,d), create(n,d))

  def unapply(in: Any): Option[(BigInt,BigInt)] = in match {
    case Ratio(n, d) => Some((n, d))
    case _ => None
  }

  val zero = apply(0)

  val one = apply(1)

}

final class Ratio private (val n: BigInt, val d: BigInt) extends ScalaNumber with ScalaNumericConversions with Ordered[Ratio] {
  
  def *(that: BigInt): Ratio = Ratio(n * that, d)
  def /(that: BigInt): Ratio = Ratio(n, d * that)
  def +(that: BigInt): Ratio = Ratio(n + (that * d), d)
  def -(that: BigInt): Ratio = Ratio(n - (that * d), d)

  def *(that: Ratio): Ratio = Ratio(n * that.n, d * that.d)
  def /(that: Ratio): Ratio = Ratio(n * that.d, d * that.n)
  def +(that: Ratio): Ratio = Ratio((n * that.d) + (that.n * d), d * that.d)
  def -(that: Ratio): Ratio = Ratio((n * that.d) - (that.n * d), d * that.d)

  def compare(that: Ratio): Int = (n * that.d) compare (that.n * d)

  override def toString = if (d > 1) (n.toString + "/" + d.toString) else (n.toString)

  override def hashCode: Int = 37 * (37 + (n % BigInt(Int.MaxValue)).toInt) + (d % BigInt(Int.MaxValue)).toInt

  override def equals(in: Any): Boolean = in match {
    case x: Ratio => n == x.n && d == x.d
    case x: Int => n == x && d == 1
    case x: Long => n == x && d == 1
    case x: BigInt => n == x && d == 1
    case _ => false
  }

  def underlying(): AnyRef = (n,d)

  def isWhole = false

  def intValue = (n / d).toInt

  def longValue = (n / d).toLong

  def floatValue = (BigDecimal(n) / BigDecimal(d)).toFloat

  def doubleValue = (BigDecimal(n) / BigDecimal(d)).toDouble
}

}
}
