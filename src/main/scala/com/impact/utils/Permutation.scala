/*
 ** Copyright (c) 2014 Impact Computing Corporation
 **
 ** Licensed under the Apache License, Version 2.0 (the "License")
 ** you may not use this file except in compliance with the License.
 ** You may obtain a copy of the License at:
 **   http://www.apache.org/licenses/LICENSE-2.0
 **
 ** Unless required by applicable law or agreed to in writing, software
 ** distributed under the License is distributed on an "AS IS" BASIS,
 ** WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 ** See the License for the specific language governing permissions and
 ** limitations under the License.
 */

package com.impact.utils

import scalaz.Liskov, Liskov. _

/**
  Useful collection that allows transformations on data and then 'snaps' it back
  to its original shape.  It works by providing a series of retractions for each map,
  allowing it reverse the shape transforms when 'restore' is called.

  For example:
  val perm  = Permutation(List(4,3,2,1,1,2,3,4))
  val perm2 = perm.distinct         // Perm(Seq(4,3,2,1))
  val perm3 = perm2.sorted          // Perm(Seq(1,2,3,4))
  val perm4 = perm3.map(_*10)       // Perm(Seq(10, 20, 30, 40))
  val xs    = perm4.restore.flatten // Seq(40, 30, 20, 10, 10, 20, 30, 40)

*/
class Permutation[+A](xs: Seq[A], mapping: Int => Option[Int], length: Int) {
  
  private def nextPerm[B](xs: Seq[B], mapping: Int => Option[Int]): Permutation[B] = {
    new Permutation(xs, andThen(mapping), length)
  }

  private def makeMapping(ks: Seq[Option[Int]]): Int => Option[Int] = (a:Int) => {
    for(maybeIndex <- ks.lift(a); index <- maybeIndex) yield index
  }

  private def andThen(newMapping: Int=>Option[Int]):Int=>Option[Int] = {
    mapping(_).flatMap(newMapping)
  }
  
  /**
    * Removes all items with indexes not in the set.
    */
  def remove(f: Seq[A] => Set[Int]) = {
    val keep = f(xs)
    val newXs = xs.zipWithIndex.filter(a=>keep.contains(a._2)).map(_._1)
    val xsMap = newXs.zipWithIndex.toMap
    val newIdxs = xs.map(xsMap.lift)
    nextPerm(newXs, makeMapping(newIdxs))
  }

  def filter(f: A => Boolean):Permutation[A] = {
    remove(_.zipWithIndex.filter(a => f(a._1)).map(_._2).toSet)
  }

  def collect[B](f: PartialFunction[A,B]):Permutation[B] = {
    val (newXs, keepIdxs) = xs.toStream.map(f.lift).zipWithIndex.collect({
      case (Some(x), i) => x -> i
    }).unzip

    val mapping = keepIdxs.zipWithIndex.toMap.lift
    nextPerm(newXs, mapping)
  }
  
  def sorted[B >: A](implicit ORD: Ordering[B]): Permutation[B] = {
    val (newXs,newIdxs) = xs.zipWithIndex.sortBy[B](_._1).unzip
    val newMapping = makeMapping(newIdxs.view.map(Some(_)))
    nextPerm(newXs, newMapping)
  }

  def sortBy[B:Ordering](f: A => B): Permutation[A] = this.sorted(Ordering.by(f))
  
  def distinct: Permutation[A] = {
    val distXs = xs.toStream.distinct
    val xsToNewIDX = distXs.zipWithIndex.toMap
    val newIdxs = xs.map(xsToNewIDX)
    nextPerm(distXs, makeMapping(newIdxs.view.map(Some(_))))
  }

  // We only allow flatten on options right now
  def flatten[B](implicit ev: A <~< Option[B]):Permutation[B] = {
    Liskov.co[Permutation, A, Option[B]](ev)(this) collect {
      case Some(i) => i
    }
  }

  def foreach(f: A => Unit): Unit = xs.foreach(f)
  
  def map[B](f: A => B): Permutation[B] = {
    new Permutation(xs.map(f), mapping, length)
  }
  
  def fmap[B](f: Seq[A] => Seq[B]): Permutation[B] = {
    val newItems = f(xs)
    assert(newItems.size == xs.size)
    new Permutation(newItems, mapping, length)
  }

  def restore: Seq[Option[A]] = (0 until length).map(mapping(_).flatMap(xs.lift))

  def toStream: Stream[A] = xs.toStream

  def size: Int = xs.size

  override def toString: String = s"Permutation(${xs})"
  
}

object Permutation {
  def identityMapping(length: Int): Int => Option[Int] = (a:Int) => if(a < length) Some(a) else None
  
  def apply[A](xs: Seq[A]) = {
    val size = xs.size
    new Permutation(xs, identityMapping(size), size)
  }
}
