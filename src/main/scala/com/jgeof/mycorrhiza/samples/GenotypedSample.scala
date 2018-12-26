package com.jgeof.mycorrhiza.samples
import com.jgeof.mycorrhiza.samples.Genotype

class GenotypedSample(identifier:String, origin:String) extends Sample(identifier, origin) with Genotype {

    def initDist(that: GenotypedSample): Unit = {
        val shared = this===that
        val same = this=?that
        updateDist(that, (shared-same)/shared.toFloat)
    }

}

object GenotypedSample {

    def apply(identifier: String, origin: String, genotype: String): GenotypedSample = {
        val sample = new GenotypedSample(identifier, origin)
        sample.initFromString(genotype)
        sample
    }
}
