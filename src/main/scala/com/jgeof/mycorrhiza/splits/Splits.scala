package com.jgeof.mycorrhiza.splits

import breeze.linalg.{DenseMatrix, DenseVector}
import com.jgeof.mycorrhiza.samples.Sample
import scala.collection.mutable.ArrayBuffer
import breeze.optimize.linear._
import breeze.numerics._

class Splits {

    var order = new Array[Sample](0)

    def setOrdering(newOrdering: Seq[Sample]): Unit = {
        order = newOrdering.toArray
    }

    def run(): Unit = {

        //order = order.reverse
        print(order.toList)

        val pairwiseIndices = for(x <- order.indices; y <- x+1 until order.size) yield (x,y)

        // Vector of pairwise distances
        val pairwiseDistances = for((x,y) <- pairwiseIndices) yield (order(x)-order(y)).toDouble

        // Vector of all possible splits
        var splitsStartEndVector = for(s <- 1 until order.size; p <- s until order.size) yield (s,p)
        splitsStartEndVector = splitsStartEndVector.sortBy(sp => sp._1 - sp._2)

        def seperated(i: Int, j: Int, s0: Int, s1: Int): Double =
            if(((i < s0 | i > s1) & j >= s0 & j <= s1) | (i >= s0 & i <= s1 & (j < s0 | j > s1))) 1d else 0d

        // Indicator matrix
        var indicatorMatrix = for((x,y) <- pairwiseIndices) yield for((s0, s1) <- splitsStartEndVector) yield seperated(x,y,s0, s1)
        indicatorMatrix = indicatorMatrix.transpose
        var indicatorArray = indicatorMatrix.flatten.toArray

        val ata = new DenseMatrix(pairwiseIndices.size, pairwiseIndices.size, indicatorArray)
        val atb = DenseVector[Double](pairwiseDistances.toArray)

        val nnls = new NNLS()
        val X = nnls.minimize(ata, atb)

        val atbr = DenseVector[Double](atb.toArray.reverse)

        println(ata*X)
        println(((ata*X)-atb).map(x => math.pow(x,2)).toArray.sum)
        println(((ata*X)-atbr).map(x => math.pow(x,2)).toArray.sum)
        println(atb)
        println(atbr)
    }
}
