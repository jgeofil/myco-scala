package com.jgeof.mycorrhiza.splits

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import com.jgeof.mycorrhiza.samples.Sample
import com.jgeof.mycorrhiza.splits.solver.OpenSha

class Splits extends scalax.chart.module.Charting {

    var order = new Array[Sample](0)

    def setOrdering(newOrdering: Seq[Sample]): Unit = {
        order = newOrdering.toArray
    }

    def run(): Unit = {

        val pairwiseIndices = for(x <- order.indices; y <- x+1 until order.size) yield (x,y)

        // Vector of pairwise distances
        val pairwiseDistances = for((x,y) <- pairwiseIndices) yield (order(x)-order(y)).toDouble
        val pairwiseArray = pairwiseDistances.toArray

        // Vector of all possible splits
        var splitsStartEndVector = for(s <- 1 until order.size; p <- s until order.size) yield (s,p)
        splitsStartEndVector = splitsStartEndVector.sortBy(sp => sp._1 - sp._2)

        def seperated(i: Int, j: Int, s0: Int, s1: Int): Double =
            if(((i < s0 | i > s1) & j >= s0 & j <= s1) | (i >= s0 & i <= s1 & (j < s0 | j > s1))) 1d else 0.01d

        // Indicator matrix
        var indicatorMatrix = for((x,y) <- pairwiseIndices) yield for((s0, s1) <- splitsStartEndVector) yield seperated(x,y,s0, s1)
        //indicatorMatrix = indicatorMatrix
        var indicatorArray = indicatorMatrix.flatten.toArray

        val ata = new DenseMatrix(pairwiseIndices.size, pairwiseIndices.size, indicatorArray)
        val atb = DenseVector[Double](pairwiseDistances.toArray)

        //def seperatedB(i: Int, j: Int, s0: Int, s1: Int): Boolean =
        //    ((i < s0 | i > s1) & j >= s0 & j <= s1) | (i >= s0 & i <= s1 & (j < s0 | j > s1))
        //val builder = new CSCMatrix.Builder[Double](rows=pairwiseIndices.size, cols=pairwiseIndices.size)
        //for(((x,y),i) <- pairwiseIndices.zipWithIndex) for(((s0, s1),j) <- splitsStartEndVector.zipWithIndex){
        //    if(seperatedB(x,y,s0, s1)){
        //        builder.add(i,j, 1.0)
        //    }
        //}

        //val sata = builder.result()


        def powSum(total: Double, v: Double): Double = total + (v*v)

        def objectiveFunc(ata: DenseMatrix[Double], x: DenseVector[Double], atb: DenseVector[Double]): Double = {
            ((ata*x)-atb).toArray.map(x=> x*x).sum
        }

        val solver = new OpenSha()

        solver.init(indicatorArray, pairwiseArray)

        val x = solver.solve()

        val atx = DenseVector[Double](x)

        println(x.toList)
        println(pairwiseArray.toList)
        println(ata*atx)
        println(objectiveFunc(ata, atx, atb))

        val estimated = (ata*atx).toArray

        val data = List(
            (for((x,y) <- estimated.zipWithIndex.toList) yield (y,x)).toXYSeries("Est"),
            (for((x,y) <-pairwiseDistances.zipWithIndex) yield (y,x)).toXYSeries("Real"))

        val chart2 = XYLineChart(data)
        chart2.show()

    }
}
