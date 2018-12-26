package com.jgeof.mycorrhiza.splits

import breeze.linalg.{CSCMatrix, DenseMatrix, DenseVector}
import com.jgeof.mycorrhiza.samples.Sample
import com.jgeof.mycorrhiza.splits.solver._

class Splits extends scalax.chart.module.Charting {

    var order = new Array[Sample](0)

    def setOrdering(newOrdering: Seq[Sample]): Unit = {
        order = newOrdering.toArray
    }

    def run(): Unit = {

        val pairwiseIndices = for(x <- order.indices; y <- x+1 until order.size) yield (x,y)

        //for((x,y) <- pairwiseIndices) println(x+"\t"+y+"\t"+(order(x)-order(y)))

        // Vector of pairwise distances
        val pairwiseDistances = for((x,y) <- pairwiseIndices) yield (order(x)-order(y)).toDouble
        val pairwiseArray = pairwiseDistances.toArray

        // Vector of all possible splits
        var splitsStartEndVector = for(s <- 1 until order.size; p <- s until order.size) yield (s,p)
        splitsStartEndVector = splitsStartEndVector.sortBy(sp => sp._1 - sp._2)

        def seperated(i: Int, j: Int, s0: Int, s1: Int): Boolean =
            ((i < s0 | i > s1) & j >= s0 & j <= s1) | (i >= s0 & i <= s1 & (j < s0 | j > s1))

        //# construct indicator matrix A from splits compatible with circular order
        //def separated(i,j,t):
        //return ((i<t[0] or i>t[1]) and j>=t[0] and j<=t[1]) or \
        //(i>=t[0] and i<=t[1] and (j<t[0] or j>t[1]))

        // Indicator matrix
        //var indicatorMatrix = for((x,y) <- pairwiseIndices) yield {for((s0, s1) <- splitsStartEndVector) yield if(seperated(x,y,s0, s1)) 1d else 0d}


        //var indicatorArray = indicatorMatrix.flatten.toArray

        val builder = new CSCMatrix.Builder[Double](pairwiseIndices.length,pairwiseIndices.length)
        for(((x,y),j) <- pairwiseIndices.zipWithIndex){
           for (((s0, s1),k) <- splitsStartEndVector.zipWithIndex){
               if (seperated(x,y,s0, s1)) {
                    builder.add(j,k,1)
                }
            }
        }

        val ata = builder.result()

        val atb = DenseVector[Double](pairwiseDistances.toArray)

        val solver = new TSNNLS()

        solver.init(ata, atb)

        val xxx = solver.solve().map(_*1)

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
