package com.jgeof.mycorrhiza

import com.jgeof.mycorrhiza.distances.{DistanceMatrix, Distances}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class Sample(identifier:String, origin:String, rawGenotype: String) extends Genotype(rawGenotype) {

    def getName: String = identifier

    override def toString(): String = s"[$identifier: $origin]"
}

object Sample {

    private val instances = new ArrayBuffer[Sample]
    var distMatrix: Option[DistanceMatrix] = None

    def app(identifier: String, origin: String, rawGenotype: String): Sample = {
        val sample = new Sample(identifier, origin, rawGenotype)
        instances.append(sample)
        sample
    }

    def unapply(arg: Sample): Option[String] = Some(arg.getName)

    val getSamples: ArrayBuffer[Sample] = instances

    def calcDistances(metric: Distances.Metric): Unit = {
        distMatrix = Some(Distances.getDistanceMatrix(instances, metric))
        println("DONE")
    }

    def readFromFile(fileName: String): Unit = {
        println("Reading samples form file...")
        val lines = Source.fromFile(fileName).getLines()
        var t0 = System.nanoTime()
        var sampleCount = 0
        var thresh = 20
        var arr = Array.empty[String]
        for(str <- lines) {
            arr = str.split("\t")
            instances.append(Sample.app(arr.head, arr(1), arr.last))
            sampleCount += 1
            if(sampleCount % thresh == 0){
                val perSecond = thresh/((System.nanoTime()-t0)*1e-9)
                thresh = perSecond.toInt*10
                println(s"Reading $perSecond samples per second, with $sampleCount done...")
                t0 = System.nanoTime()
            }
        }
    }
}
