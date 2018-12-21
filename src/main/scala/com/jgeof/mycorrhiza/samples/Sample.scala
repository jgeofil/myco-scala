package com.jgeof.mycorrhiza.samples

import scala.collection.mutable

class Sample(identifier:String, origin:String) extends Genotype() {

    import Sample.distCache

    def getName: String = identifier

    override def toString(): String = s"$identifier"

    def -(that: Sample): Float = {
        distCache.getOrElseUpdate((this,that),{
            val shared = this===that
            val same = this=?that
            //(same-shared)/same.toFloat
            shared-same
        })
    }

    def updateDist(s: Sample, f: Float): Unit = {
        distCache.update((this,s), f)
        distCache.update((s,this), f)
    }

    def >(that: Sample): Boolean = this.getName > that.getName

}

object Sample {

    val distCache = new mutable.HashMap[(Sample,Sample), Float]

    def apply(identifier: String, origin: String, genotype: String): Sample = {
        val sample = new Sample(identifier, origin)
        sample.initFromString(genotype)
        sample
    }

    def unapply(arg: Sample): Option[String] = Some(arg.getName)

}
