package com.jgeof.mycorrhiza

import com.jgeof.mycorrhiza.util.Timer.time
import com.jgeof.mycorrhiza.Graph._
import com.typesafe.scalalogging.LazyLogging
import ch.qos.logback.classic.{Level, Logger}
import com.jgeof.mycorrhiza.splits.Splits
import org.slf4j.LoggerFactory


object Main extends App with LazyLogging{

    LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[Logger].setLevel(Level.DEBUG)

    val graph = new Graph

    //time("Read samples"){graph.readSamplesFromFile("/home/jeremy/repos/scala-myco/resources/testgroup-10.tsv")}

    var s = List("B","C","D","E","F")
    var o = Map("B"->"0","C"->"0","D"->"0","E"->"0","F"->"0")
    var d = Map(
        ("B","C")->8f,
        ("B","D")->13f,
        ("B","E")->19f,
        ("B","F")->11f,
        ("C","D")->7f,
        ("C","E")->13f,
        ("C","F")->15f,
        ("D","E")->10f,
        ("D","F")->12f,
        ("E","F")->14f
    )
    s = List("A","B","C","D")
    o = Map("A"->"0","B"->"0","C"->"0","D"->"0")
    d = Map(
        ("A","B")->7f,
        ("A","C")->7f,
        ("A","D")->10f,
        ("B","C")->12f,
        ("B","D")->8f,
        ("C","D")->11f
    )
    graph.setSamplesAndDistances(s,o,d)
    val ordering = graph.run()

    val splits = new Splits

    splits.setOrdering(ordering)

    splits.run()

    Console.out.flush()

}


