package com.jgeof.mycorrhiza

object Main extends App{

    var t0 = System.nanoTime()
    val slist = Sample.readFromFile("resources/sequences-N500-L100000-P10.tsv")
    var t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0)*1e-9)

    t0 = System.nanoTime()
    val dist = Distances.toDistanceMatrix(slist, Distances.jukesCantor())
    t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0)*1e-9)

}


