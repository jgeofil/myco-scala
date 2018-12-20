package com.jgeof.mycorrhiza.util

class Timer(){

    var t0 = 0l

    def start(): Unit = t0 = System.nanoTime()

    def now(): Double = (System.nanoTime() - t0) * 1e-9
}

object Timer {

    def time[R](name: String)(block: => R): R = {
        val t0 = System.nanoTime()
        val result = block
        val t1 = System.nanoTime()
        println(s"Elapsed time for $name: " + (t1 - t0)*1e-9 + "s")
        result
    }
}
