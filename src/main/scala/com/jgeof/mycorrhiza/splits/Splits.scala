package com.jgeof.mycorrhiza.splits

import breeze.linalg.{CSCMatrix, DenseMatrix, DenseVector}
import com.jgeof.mycorrhiza.samples.Sample
import com.jgeof.mycorrhiza.splits.solver._

class Splits extends scalax.chart.module.Charting {

    var order = new Array[Sample](0)
    var N = 0

    def setOrdering(newOrdering: Seq[Sample]): Unit = {
        order = newOrdering.toArray
        N = order.length
    }

    def run(): Unit = {

        val pairwiseIndices = for(x <- order.indices; y <- x+1 until N) yield (x,y)

        // Vector of pairwise distances
        val pairwiseDistances = for((x,y) <- pairwiseIndices) yield (order(x)-order(y)).toDouble
        val pairwiseArray = pairwiseDistances.toArray

        // Vector of all possible splits
        var splitsStartEndVector = for(start <- 1 until N; end <- start until N) yield (start, end)
        splitsStartEndVector = splitsStartEndVector.sortBy(sp => sp._1 - sp._2)

        def seperated(i: Int, j: Int, start: Int, end: Int): Boolean =
            ((i < start | i > end) & j >= start & j <= end) | (i >= start & i <= end & (j < start | j > end))

        // Indicator matrix
        var indicatorMatrix = for((x,y) <- pairwiseIndices) yield {
            for((start, end) <- splitsStartEndVector) yield
                if(seperated(x, y, start, end)) 1d else 0d
        }
        var indicatorArray = indicatorMatrix.flatten.toArray

        val ata = new DenseMatrix[Double](pairwiseIndices.length,pairwiseIndices.length, indicatorArray)
        val atb = DenseVector[Double](pairwiseDistances.toArray)

        val solver = new TSNNLS()

        solver.init(ata, atb)

        val xxx = solver.solve()

        //val atx = DenseVector[Double]((for(x <- xxx) yield if(x> 1e-6) x else 0).toArray)
        println("W")
        println(xxx.toArray.toList)
        println("REAL")
        println(pairwiseArray.toList)
        println("EST")
        println(ata*xxx)
        println("OBJ")
        println(Solver.objectiveFunc(ata, xxx, atb))
        println("MSE")
        println(Solver.meanSquare(ata, xxx, atb))


        val diff = atb-(ata*xxx)

        val estimated = (ata*xxx).toArray
        println("DIFF")
        println(diff)

        val data = List(
            (for((x,y) <- estimated.zipWithIndex.toList) yield (y,x)).toXYSeries("Est"),
            (for((x,y) <-pairwiseDistances.zipWithIndex) yield (y,x)).toXYSeries("Real"))

        val chart2 = XYLineChart(data)
        chart2.show()

    }
}
