package com.jgeof.mycorrhiza.splits.solver

import breeze.linalg._
import breeze.numerics._
import com.typesafe.scalalogging.LazyLogging

class TSNNLS extends Solver with LazyLogging{

    var AMatrix: CSCMatrix[Double] = _
    var bVector: SparseVector[Double] = _

    var xVector: SparseVector[Double] = _
    var yVector: SparseVector[Double] = _

    var pValue = 3

    var NValue = Float.PositiveInfinity

    def init(A: CSCMatrix[Double], b: DenseVector[Double]): Unit = {
        logger.debug("Initializing solver..")
        AMatrix = A

        bVector = SparseVector.tabulate(b.length)(b(_))
        bVector.compact()

        yVector = -(AMatrix.t*bVector)
        yVector.compact()

        xVector = SparseVector.zeros[Double](bVector.length)
        xVector.compact()

        pValue = 3
        NValue = Float.PositiveInfinity

        logger.debug("Solver initialized.")
    }

    def isFeasibleSolution(): Boolean = {
        var flag = true
        xVector.activeValuesIterator.foreach(x => if(x<0) flag = false)
        yVector.activeValuesIterator.foreach(x => if(x<0) flag = false)
        flag
    }

    def coutNegatives(): Int = {
        var count = 0
        xVector.activeValuesIterator.foreach(x => if(x<0d) count+=1)
        yVector.activeValuesIterator.foreach(x => if(x<0d) count+=1)
        count
    }

    def getNegativeIndices(vec: SparseVector[Double]): Iterator[Int] = {
        for((i, v) <- vec.activeIterator; if v < 0) yield i
    }

    def exchange(): Unit = {
        val xNegs = getNegativeIndices(xVector).toArray
        val yNegs = getNegativeIndices(yVector).toArray
        for(i <- xNegs) exchangeXtoY(i)
        for(i <- yNegs) exchangeYtoX(i)
        xVector.compact()
        yVector.compact()
        //println("F size: "+F.size+" G size: "+G.size)
    }

    def exchangeXtoY(index: Int): Unit = {
        yVector(index) = xVector(index)
        xVector(index) = 0d
    }
    def exchangeYtoX(index: Int): Unit = {
        xVector(index) = yVector(index)
        yVector(index) = 0d
    }

    def getAMatrix(vec: SparseVector[Double]): CSCMatrix[Double] = {
        val builder = new CSCMatrix.Builder[Double](rows=bVector.length, bVector.length)
        for(x <- 0 until AMatrix.rows; y <- vec.activeKeysIterator) builder.add(x,y,AMatrix(x,y))
        builder.result()
    }

    def solve(): DenseVector[Double] = {
        while(!isFeasibleSolution()){

            val n = coutNegatives()

            if(n < NValue){
                NValue = n
                pValue = 3
                println("We reduced the number of infeasible")
                exchange()
            }
            else{
                if(pValue > 0){
                    pValue = pValue-1
                    println("Nope. Try again anyway..")
                    exchange()
                }
                else{
                    println("Nope...!")
                    val x = getNegativeIndices(xVector).toArray
                    val y = getNegativeIndices(yVector).toArray
                    var mx = -1
                    var my = -1
                    if(x.length > 0) mx = x.max
                    if(y.length > 0) my = y.max
                    if(mx > my){
                        exchangeXtoY(x.max)
                    }else{
                        exchangeYtoX(y.max)
                    }

                    xVector.compact()
                    yVector.compact()
                }
            }

            val Af = getAMatrix(xVector)

            xVector = Af\bVector

            println(xVector.activeSize, xVector.activeSize)

            val Ag = getAMatrix(yVector)

            println(yVector.activeSize)
            yVector = Ag.t*((Af*xVector) - bVector)
            println(yVector.activeSize)

            for((k,v) <- xVector.activeIterator; if abs(v) < 1e-12) xVector(k) = 0d
            for((k,v) <- yVector.activeIterator; if abs(v) < 1e-12) yVector(k) = 0d

            xVector.compact()
            yVector.compact()
        }

        xVector.toDenseVector
    }


}
