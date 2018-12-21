package com.jgeof.mycorrhiza.distances

import com.jgeof.mycorrhiza.samples.GenotypedSample

object Distances {

    type Metric = (GenotypedSample, GenotypedSample) => Float

    def getDistanceMatrix(samples: Seq[GenotypedSample], metric: Metric): DistanceMatrix = {
        println("Calculating distances...")
        var matrix = new DistanceMatrix(samples)
        for(i <- samples.indices.par){
            for(j <- i+1 until samples.length par){
                val si = samples(i)
                val sj = samples(j)
                matrix.setTo(si, sj, metric(si, sj))
            }
        }
        println("DONE")
        matrix
    }

    def jukesCantor(rate: Float=0.25f)(sa: GenotypedSample, sb:GenotypedSample):Float = {
        val same = sa === sb
        val valid = sa =? sb
        (valid-same)/valid.toFloat
    }

}
