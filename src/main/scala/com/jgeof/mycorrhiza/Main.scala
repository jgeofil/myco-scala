package com.jgeof.mycorrhiza

import com.jgeof.mycorrhiza.util.Timer.time
import com.jgeof.mycorrhiza.Graph._

object Main extends App{

    val graph = new Graph

    time("Read samples"){graph.readSamplesFromFile("/home/jeremy/repos/scala-myco/resources/sequences-N15-L100-P3.tsv")}

    graph.run()

    Console.out.flush()

}


