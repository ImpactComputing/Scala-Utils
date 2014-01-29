package com.impact.utils.interpolation

import java.awt.geom.Point2D

trait RelativeDistance[A] {
  def apply(a:A, b:A): Double
}

trait GenericEuclideanDistance[A] {
  // Builds a component vector
  def componentDists(a:A, b:A): List[Double]
  // Standard Euclidean Distance from component vectors
  def calcDist(xs:Seq[Double]):Double = {
    Math.sqrt(xs.foldLeft(0.0)({case (acc, x) => acc + x*x}))
  }
  def apply(a:A, b:A): Double = calcDist(componentDists(a,b))
}

trait LowPriorityDistance {

  implicit def numericDist[A:Numeric] = new RelativeDistance[A] {
    val num = implicitly[Numeric[A]]
    def apply(a:A, b:A) = num.toDouble(num.abs(num.minus(a,b)))
  }

  // Creates a new RelativeDistance using Euclidean Distance and tuples
  // It's annoying writing tuple matching, but not much you can do
  // Macro in the future?  Shapeless?
  implicit def euclidean2[A:RelativeDistance,B:RelativeDistance] = new RelativeDistance[(A,B)] with GenericEuclideanDistance[(A,B)] {
    val aRD = implicitly[RelativeDistance[A]]
    val bRD = implicitly[RelativeDistance[B]]
    def componentDists(a:(A,B), b:(A,B)) = {
      List(aRD.apply(a._1, b._1), bRD.apply(a._2, b._2))
    }
  }

  implicit def euclidean3[A:RelativeDistance,B:RelativeDistance,C:RelativeDistance] = new RelativeDistance[(A,B,C)] with GenericEuclideanDistance[(A,B,C)] {
    val cRD = implicitly[RelativeDistance[C]]
    val abGED = implicitly[GenericEuclideanDistance[(A,B)]]
    def componentDists(a:(A,B,C), b:(A,B,C)): List[Double] = {
      cRD.apply(a._3, b._3) :: abGED.componentDists((a._1, a._2), (b._1, b._2))
    }
  }

  implicit def euclidean4[A,B,C,D](implicit abRD: GenericEuclideanDistance[(A,B)], cdRD:GenericEuclideanDistance[(C,D)]) = new RelativeDistance[(A,B,C,D)] with GenericEuclideanDistance[(A,B,C,D)] {
    def componentDists(a:(A,B,C,D), b:(A,B,C,D)): List[Double] = {
      abRD.componentDists((a._1, a._2), (b._1, b._2)) ++ 
      cdRD.componentDists((a._3, a._4), (b._3, b._4))
    }
  }
} 

object RelativeDistance extends LowPriorityDistance {
  def apply[A:RelativeDistance] = implicitly[RelativeDistance[A]]
    
  // Specialied for speed
  implicit object DoubleRelDistance extends RelativeDistance[Double] {
    def apply(a:Double, b:Double) = (a - b).abs
  }

  /**
    * Returns a new RelativeDistance based on a list of components.
    */
  def euclidean[A](f: A => List[Double]) = new RelativeDistance[A] with GenericEuclideanDistance[A] {
    def componentDists(a:A, b:A) = (f(a), f(b)).zipped.map((i,j) => (i-j).abs)
  }
}
