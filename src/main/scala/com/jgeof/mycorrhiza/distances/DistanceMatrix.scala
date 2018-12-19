package com.jgeof.mycorrhiza.distances
import com.jgeof.mycorrhiza.{Sample, Util}

class DistanceMatrix(samples: Seq[Sample]){

    private val keys = samples.toArray
    private val matrix = Array.ofDim[Float](keys.length, keys.length)

    private val toInt = (s: Sample) => keys.indexOf(s)

    def setTo(sample1: Sample, sample2: Sample, value: Float): Unit = {
        val i = toInt(sample1)
        val j = toInt(sample2)
        if(i > j) matrix(i)(j) = value
        else matrix(j)(i) = value
    }

    def apply(sample1: Sample, sample2: Sample): Float = {
        val i = toInt(sample1)
        val j = toInt(sample2)
        if(i > j) matrix(i)(j) else matrix(j)(i)
    }

    override def toString: String = {
        var str: String = ""
        str += "\n"
        val short = matrix.take(5)++matrix.takeRight(5)
        short.foreach(arr => {
            val sha = arr.take(5)++arr.takeRight(5)
            sha.foreach(value => {
                str += Util.fv(value)+"\t"

            })
            str += "\n"
        })
        str += "\n"
        str
    }
}
