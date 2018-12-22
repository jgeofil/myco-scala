package com.jgeof.mycorrhiza

import com.jgeof.mycorrhiza.graph._
import com.jgeof.mycorrhiza.util.Timer.time
import com.typesafe.scalalogging.LazyLogging
import com.jgeof.mycorrhiza.splits.Splits
import com.jgeof.mycorrhiza.examples._


object Main extends App with LazyLogging with scalax.chart.module.Charting{


    //LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[Logger].setLevel(Level.DEBUG)

    val graph = new Graph

    //time("Read samples"){graph.readSamplesFromFile("/home/jeremy/repos/scala-myco/resources/sequences-N20-L100-P3.tsv")}

    graph.setSamplesAndDistances(Example2.s,Example2.o,Example2.d)

    val ordering = graph.run()

    val splits = new Splits()

    splits.setOrdering(ordering)

    splits.run()

    Console.out.flush()

}


