package com.impact.utils.interpolation

import language.higherKinds
import collection.generic.CanBuildFrom
import scalaz._
import Scalaz._

trait ScaledMonoid[A] extends Monoid[A] {
  def scale(a:A, factor:Double): A
}

trait LowPriorityScaledMonoid {

  trait NumericSM[A] extends ScaledMonoid[A] {
    val num: Numeric[A]
    val toN: (Double => A)
    def append(f1:A, f2: => A) = num.plus(f1,f2)
    def zero: A = num.zero
    def scale(a:A, d:Double) = toN(num.toDouble(a) * d)
  }

  // Optimize for doubles
  implicit object DoubleSSG extends ScaledMonoid[Double] {
    def append(a:Double, b: => Double) = a+b
    def zero = 0.0
    def scale(a:Double, d:Double) = a * d
  }

  // Handle containers
  implicit def traverseToSM[F[_]:Traverse:Plus, A:ScaledMonoid](implicit MFA:Monoid[F[A]],cbf:CanBuildFrom[Nothing,A,F[A]]) = new ScaledMonoid[F[A]] {
    val ssm = ScaledMonoid[A]
    val tf = Traverse[F]
    def scale(fa:F[A], scale:Double):F[A] = tf.map(fa)(ssm.scale(_, scale))
    def zero:F[A] = MFA.zero
    def append(fa:F[A], fb: => F[A]):F[A] = {
      val (rem, comb) = tf.zipWith(fa, fb) {(i, j) =>
        ssm.append(i, j.getOrElse(ssm.zero))
      }
      Plus[F].plus(comb, rem.to[F])
    }
  }

}

object ScaledMonoid extends LowPriorityScaledMonoid {

  def apply[A:ScaledMonoid] = implicitly[ScaledMonoid[A]]

  implicit object IntSSG extends NumericSM[Int] {
    val num = implicitly[Numeric[Int]]
    val toN:Double => Int = _.round.toInt
  }

}

/*
Provides generic interpolation based on metric space.
 */
object Interpolation {

  /**
    Given a collection of 'points' and values, interpolates to the given point.
    */
  def apply[F[_]:Traverse,A:RelativeDistance,B:ScaledMonoid](from: F[(A,B)])(to: A):B = {
    val dist = RelativeDistance[A]
    def distFroms:Stream[(Double, B)] = from.toStream.map(p => (dist.apply(p._1, to), p._2))
    weighted(distFroms)
  }

  def weighted[A](xs:Stream[(Double, A)])(implicit scaled:ScaledMonoid[A]):A = {
    @annotation.tailrec
    def loop(totalWeights:Double, refWeight:Double, runningVal:A, remaining:Stream[(Double,A)]):(Double, A) = remaining match {
      // Distance of zero means an exact match and no interpolation needed
      case (0.0, v) #:: _tail => (1.0, v)
      case (w1, v) #:: tail => {
        val weight = refWeight / w1
        val newVal = scaled.append(runningVal, scaled.scale(v, weight))
        loop(totalWeights + weight, refWeight, newVal, tail)
      }
      // done
      case _ => (totalWeights, runningVal)
    }

    xs match {
      case (weight, startV) #:: tail => {
        val (totalWeight, v) = loop(1.0, weight, startV, xs.tail)
        scaled.scale(v, 1 / totalWeight)
      }
      case _ => scaled.zero
    }
  }

  /*
  Interpolation when all the weights are the same.
   */
  def average[F[_]:Foldable, A:ScaledMonoid](xs: F[A]): A = {
    weighted(Foldable[F].toStream(xs).map(a=>(1.0, a)))
  }
}
