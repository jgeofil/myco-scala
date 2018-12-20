package com.jgeof.mycorrhiza

import com.jgeof.mycorrhiza.util.Timer.time
import com.jgeof.mycorrhiza.Graph._

object Main extends App{

    val graph = new Graph

    time("Read samples"){graph.readSamplesFromFile("resources/sequences-N20-L100-P3.tsv")}

    graph.populateClusters()

    val (c1: graph.Cluster, c2: graph.Cluster) = graph.findMinClusters(Graph.MainClusterList())
    graph.populateTempClusters(c1, c2)
    val (c1: graph.Cluster, c2: graph.Cluster) = graph.findMinClusters(Graph.MainClusterList())

    Console.out.flush()

}


