package com.jgeof.mycorrhiza.samples

import scala.collection.mutable

class Sample(identifier:String, origin:String) extends Genotype() {

    val distanceTo = new mutable.HashMap[Sample, Float]

    def getName: String = identifier

    override def toString(): String = s"$identifier"

    def -(that: Sample): Float = {
        if(distanceTo.contains(that)){
           distanceTo(that)
        }else{
            val shared = this===that
            val same = this=?that
            (same-shared)/same.toFloat
        }
    }

    def updateDist(s: Sample, f: Float): Unit = distanceTo.update(s, f)

    def >(that: Sample): Boolean = this.getName > that.getName

}

object Sample {

    def apply(identifier: String, origin: String, genotype: String): Sample = {
        val sample = new Sample(identifier, origin)
        sample.initFromString(genotype)
        sample
    }

    def unapply(arg: Sample): Option[String] = Some(arg.getName)

}
