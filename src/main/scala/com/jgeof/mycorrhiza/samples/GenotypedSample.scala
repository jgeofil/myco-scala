package com.jgeof.mycorrhiza.samples

import scala.collection.mutable


abstract class Sample(identifier: String, origin: String){
    import Sample.distCache

    def getName: String = identifier
    override def toString: String = s"$identifier"

    def >(that: Sample): Boolean = this.getName > that.getName
    def -(that: Sample): Float = {
        distCache.getOrElseUpdate(if(this>that)(this,that)else(that,this), if(this==that) 0 else Float.PositiveInfinity)
    }
    def updateDist(s: Sample, f: Float): Unit = {
        distCache.update(if(this>s)(this,s)else(s,this), f)
    }
}

class GenotypedSample(identifier:String, origin:String) extends Sample(identifier, origin) with Genotype {

    def initDist(that: GenotypedSample): Unit = {
        val shared = this===that
        val same = this=?that
        updateDist(that, shared-same)
    }

}

class PrecaculatedSample(identifier: String, origin: String) extends Sample(identifier, origin){
    def initDist(that: PrecaculatedSample, distance: Float): Unit = {
        updateDist(that, distance)
    }
}

object Sample {
    var distCache = new mutable.HashMap[(Sample, Sample), Float]
    var permCache = new mutable.HashMap[(Sample, Sample), Float]
    def unapply(arg: Sample): Option[String] = Some(arg.getName)

    def persistDistances(): Unit = {
        permCache = distCache.clone()
    }

    def restoreDistances(): Unit = {
        distCache = permCache.clone()
    }
}

object GenotypedSample {

    def apply(identifier: String, origin: String, genotype: String): GenotypedSample = {
        val sample = new GenotypedSample(identifier, origin)
        sample.initFromString(genotype)
        sample
    }
}
