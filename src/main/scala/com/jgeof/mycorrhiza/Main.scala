package com.jgeof.mycorrhiza

import com.jgeof.mycorrhiza.distances.Distances

object Main extends App{

    var t0 = System.nanoTime()
    Sample.readFromFile("/home/jeremy/repos/scala-myco/resources/sequences-N1000-L1000000-P10.tsv")
    var t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0)*1e-9)

    t0 = System.nanoTime()
    Sample.calcDistances(Distances.jukesCantor())
    println("DONE")
    print(Sample.distMatrix.toString)
    t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0)*1e-9)
    Console.out.flush()

}


