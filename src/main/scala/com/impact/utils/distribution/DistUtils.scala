/*
** Copyright (c) 2013 Impact Computing Corporation
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

package com.impact.utils.distribution

import org.apache.commons.math3.random.{RandomGenerator,Well19937c}

import org.apache.commons.math3.distribution._
import org.apache.commons.math3.stat.descriptive.moment.{Mean,StandardDeviation}


trait DistBounds[Dist] {
  val meanMin: Double = Double.MinValue
  val meanMax: Double = Double.MaxValue
  val stdMin:  Double = 0
  val stdMax:  Double = Double.MaxValue

  def isValid(mean: Double, std: Double) = {
    mean < meanMax && mean > meanMin && std > stdMin && std < stdMax
  }
}

trait LowPriorityDistBounds {
  implicit def FullBounds[A <:AbstractRealDistribution] = new DistBounds[A] {}
}

object DistBounds extends LowPriorityDistBounds {
  implicit object UniformBounds extends DistBounds[UniformRealDistribution]

  implicit object GammaBounds extends DistBounds[GammaDistribution] {
    override val meanMin = 0.0
  }

  implicit object BetaBounds extends DistBounds[BetaDistribution] {
    override val meanMin = 0.0
    override val meanMax = 1.0
  }

}

trait DistBuilder[From, Dist] {
  def build(from: From): Dist
}

object DistBuilder {
  // Basically a case class but can't extend from case classes
  case class StatMoment(mean:Double, std:Double)

  // From a statistical moment, either return the distribution or None if can't
  trait MomentBuilder[Dist] extends DistBuilder[StatMoment, Option[Dist]]

  // Too much boilerplate, lets use partial functions to simplify building
  def constructSMDist[Dist:DistBounds](f:PartialFunction[StatMoment,Dist]) = new MomentBuilder[Dist] {
    def build(meanStd:StatMoment):Option[Dist] = {
      if(implicitly[DistBounds[Dist]].isValid(meanStd.mean, meanStd.std)) {
        PartialFunction.condOpt(meanStd)(f)
      } else None
    }
  }

  // Uniform builder
  implicit val momentToUniform = constructSMDist({
    case StatMoment(mean, _s) => new UniformRealDistribution(mean, mean + 1e-15)
  })

  implicit val momentsToNormal = constructSMDist({
    case StatMoment(mean,s) => new NormalDistribution(mean, s)
  })

  // Gamma builder - Will throw an exception if mean <= 0 since it's
  // left bounded at zero.
  implicit val momentsToGamma = constructSMDist({
      case StatMoment(mean,std) =>
          val alpha = (mean * mean) / (std*std)  
          val beta = (std * std)/mean
          new GammaDistribution(RandomGenGetter.rng, alpha, beta, GammaDistribution.DEFAULT_INVERSE_ABSOLUTE_ACCURACY)
  })

  // Beta builder
  implicit val momentsToBeta = constructSMDist({
      case StatMoment(mean,std) => 
        val (meansq, variance) = (mean*mean, std*std)
        val alpha = meansq*(1-mean)/variance;
        val beta = alpha*(1-mean)/mean;
        new BetaDistribution(RandomGenGetter.rng, alpha, beta, GammaDistribution.DEFAULT_INVERSE_ABSOLUTE_ACCURACY)
  })

  // Helper method to make converts
  def constructVia[A,B,C](f: A => B)(implicit c: DistBuilder[B, C]) = new DistBuilder[A,C] {
    def build(from: A) = c.build(f(from))
  }

  def safeStd(std: Double) = if(std > 1e-15) std else 1e-15

  // I would use StandardDeviation, but that's limited only to primitive arrays
  def calcStd(xs: Seq[Double], mean: Double) = {
    var acc = 0.0
    var len = 0
    val it = xs.toIterator
    while(it.hasNext) {
      val diff = it.next - mean
      acc += diff*diff
      len += 1
    }
    Math.sqrt((acc - acc / len) / (len - 1.0))
  }
  
  def sampleToSM(xs: Seq[Double]) = {
      val mean = xs.sum / xs.length
      // std is not allowed to be zero
      val std = safeStd( calcStd(xs, mean) )
      StatMoment(mean, std)
  }

  // Default Sample to Moment converter
  implicit def sampleViaMomentDist[Dist:MomentBuilder] = {
    constructVia[Seq[Double], StatMoment, Option[Dist]](sampleToSM)
  }

  // Want to default to another distribution on fail?
  implicit def sampleToEither[Dist1:MomentBuilder,Dist2:MomentBuilder] = new DistBuilder[StatMoment,Either[Dist1,Option[Dist2]]] {
    def build(eus:StatMoment) = {
      implicitly[MomentBuilder[Dist1]].build(eus) match {
        case Some(dist) => Left(dist)
        case None => Right(implicitly[MomentBuilder[Dist2]].build(eus))
      }
    }
  }

  // Wire it up directly
  implicit def sampleToDefault[Dist1:MomentBuilder,Dist2:MomentBuilder] = {
    constructVia[Seq[Double], StatMoment, Either[Dist1,Option[Dist2]]](sampleToSM)
  }
}

// Instantiating new random generators is the most expensive part of this
object RandomGenGetter {
  private var count = 0
  private var well: RandomGenerator = newGenerator()

  private def newGenerator():RandomGenerator = new Well19937c()

  def rng: RandomGenerator = {
    count += 1
    if(count > 10000) {
      well = newGenerator()
      count = 0
    }
    well
  }
}

object DistUtils {
  import DistBuilder._
  // Useful aliases
  type SampleToDist[Dist] = DistBuilder[Seq[Double],Option[Dist]]
  type MomentToDist[Dist] = MomentBuilder[Dist]

  // Some magic here
  def buildFromSample[Dist:SampleToDist](sample: Seq[Double]) = implicitly[SampleToDist[Dist]].build(sample)

  def buildFromMoment[Dist:MomentToDist](mean: Double, std:Double) = implicitly[MomentToDist[Dist]].build(StatMoment(mean,safeStd(std)))

  // For Java land # blech
  def buildGammaFromMoment(mean: java.lang.Double, std:java.lang.Double) = buildFromMoment[GammaDistribution](mean,std)

  def buildBetaFromMoment(mean: java.lang.Double, std:java.lang.Double) = buildFromMoment[BetaDistribution](mean,std)

}

