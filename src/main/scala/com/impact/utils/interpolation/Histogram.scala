package com.impact.utils.interpolation

import com.impact.utils.Permutation

/**
  * Not really a histogram, more of a bucket-based interpolation
  */
class Histogram[K:Numeric:Ordering, V](bvs: Seq[(K,V)]) {
  import math.Ordering.Implicits._
  import math.Numeric.Implicits._
  val kvs = bvs.toStream.sortBy(_._1)
  val kvMap = kvs.toMap
  val brackets:Stream[Stream[K]] = (for((k,v) <- kvs) yield k).sliding(2).toStream
  
  def bracketAt(a:K): Option[(K, K)] = bracketsAt(Stream(a)).head

  def bracketKVAt(a:K): Option[((K, V),(K, V))] = {
    for((left, right) <- bracketAt(a)) yield (left -> kvMap(left)) -> (right -> kvMap(right))
  }

  def bracketsKVAt(as:Seq[K]): Stream[Option[((K, V),(K, V))]] = {
    Permutation(as).distinct.sorted.fmap(sas => {
      for(bracket <- bracketsAt(sas.toStream)) yield {
        for((left, right) <- bracket) yield {
          (left -> kvMap(left)) -> (right -> kvMap(right))
        }
      }
    }).flatten.restore.toStream
  }

  def interpolate(a: K)(implicit s: ScaledMonoid[V]): Option[V] = interpolates(Vector(a)).head

  def interpolates(as: Seq[K])(implicit sm: ScaledMonoid[V]): Stream[Option[V]] = {
    Permutation(as).distinct.sorted.fmap(sas => {
      for((bracket, k) <- bracketsAt(sas.toStream).zip(sas)) yield {
        for((left,right) <- bracket) yield {
          import scalaz._, Scalaz._
          val bs = List(left, right).map(a=> a-> kvMap(a))
          Interpolation.apply(bs)(k)
        }
      }
    }).flatten.restore.toStream
  }

  def nearest(a:K): Option[(K, V)] = bracketKVAt(a) match {
    case Some((l,r)) => Some(if(a - l._1 > r._1 - a) l else r)
    case None if brackets.isEmpty => None
    case None if a < kvs.head._1 => Some(kvs.head)
    case None => Some(kvs.last)
  }

  // Requires sorted stream
  private def bracketsAt(as: Stream[K]): Stream[Option[(K,K)]] = {
    def loop(rem:Stream[K], bs: Stream[Stream[K]]):Stream[((K,K),Int)] = bs match {
      case (left #:: right #:: _r) #:: rest => {
        val (inBucket, rightOf) = rem.span(a=> a >= left && a <= right)
        ((left,right), inBucket.size) #:: loop(rightOf, rest)
      }
      case _ => Stream()
    }
    // break apart the stream to the left of the brackets
    val (leftOf, rightOf) = as.span(_ < kvs.head._1)
    val middle = loop(rightOf, brackets).flatMap(a=> Stream.continually(Some(a._1)).take(a._2))
    def Nones = Stream.continually(None)
    (Nones.take(leftOf.size) ++ middle ++ Nones).take(as.size)
  }
}

object Histogram {
  def apply[K:Numeric:Ordering,V](bucketValues: Seq[(K,V)]):Histogram[K,V] = new Histogram(bucketValues)
}
