package com.jgeof.mycorrhiza.util

object Timer {

    def time[R](name: String)(block: => R): R = {
        val t0 = System.nanoTime()
        val result = block
        val t1 = System.nanoTime()
        println(s"Elapsed time for $name: " + (t1 - t0)*1e-9 + "s")
        result
    }
}
