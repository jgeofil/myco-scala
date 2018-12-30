package com.jgeof.mycorrhiza

import breeze.linalg.{DenseVector, SparseVector, VectorBuilder}
import com.jgeof.mycorrhiza.graph._
import com.jgeof.mycorrhiza.util.Timer.time
import com.typesafe.scalalogging.LazyLogging
import com.jgeof.mycorrhiza.splits.Splits
import com.jgeof.mycorrhiza.examples._


object Main extends App with LazyLogging with scalax.chart.module.Charting{


    val va = new VectorBuilder[Int](10)

    va.add(2,4)
    va.add(3,0)
    va.add(4,1)

    //LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[Logger].setLevel(Level.DEBUG)

    val graph = new Graph

    time("Read samples"){graph.readSamplesFromFile("/home/jeremy/repos/scala-myco/resources/gipsy.tsv")}

    //com.jgeof.mycorrhiza.graph.setSamplesAndDistances(Example2.s,Example2.o,Example2.d)

    val ordering = graph.run()


    val splits = new Splits()

    splits.setOrdering(ordering)

    splits.run()

    Console.out.flush()

}


