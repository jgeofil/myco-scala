package mycorrhiza

object Main extends App{

    var t0 = System.nanoTime()
    val slist = Sample.readFromFile("resources/sequences-N20-L100-P3.tsv").toList
    var t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0)*1e-9)


    t0 = System.nanoTime()
    val dist = Distances.toDistanceMatrix(slist, Distances.jukesCantor())
    t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0)*1e-9)

}


