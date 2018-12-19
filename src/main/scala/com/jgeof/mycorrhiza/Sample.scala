package com.jgeof.mycorrhiza

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class Sample(identifier:String, origin:String, rawGenotype: String) extends Genotype(rawGenotype) {

    def getName: String = identifier
}

object Sample {

    def readFromFile(fileName: String): Array[Sample] = {
        println("Reading samples form file...")
        val lines = Source.fromFile(fileName).getLines()
        val sampleBuffer = ArrayBuffer.empty[Sample]
        var t0 = System.nanoTime()
        var sampleCount = 0
        var thresh = 20
        for(str <- lines) {
            val arr = str.split("\t")
            sampleBuffer.append(new Sample(arr.head, arr(1), arr.last))
            sampleCount += 1
            if(sampleCount % thresh == 0){
                val perSecond = thresh/((System.nanoTime()-t0)*1e-9)
                thresh = perSecond.toInt*10
                println(s"Reading $perSecond samples per second, with $sampleCount done...")
                t0 = System.nanoTime()
            }
        }
        sampleBuffer.toArray
    }
}
