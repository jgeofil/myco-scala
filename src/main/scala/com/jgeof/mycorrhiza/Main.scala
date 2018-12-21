package com.jgeof.mycorrhiza

import com.jgeof.mycorrhiza.util.Timer.time
import com.jgeof.mycorrhiza.Graph._
import com.typesafe.scalalogging.LazyLogging
import ch.qos.logback.classic.{Level,Logger}
import org.slf4j.LoggerFactory


object Main extends App with LazyLogging{

    LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[Logger].setLevel(Level.INFO)

    val graph = new Graph

    time("Read samples"){graph.readSamplesFromFile("resources/sequences-N200-L10000-P3.tsv")}

    graph.run()

    Console.out.flush()

}


