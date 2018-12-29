package com.jgeof.mycorrhiza.splits.solver

import breeze.linalg.LSMR
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

    var negativeVariableCount = 0

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

    def isFeasibleSolution: Boolean = {
        logger.debug("Checking solution feasibility..")
        countNegatives()
        negativeVariableCount <= 0
    }

    def countNegatives(): Unit = {
        negativeVariableCount = 0
        xVector.activeValuesIterator.foreach(x => if(x < 0d) negativeVariableCount += 1)
        yVector.activeValuesIterator.foreach(x => if(x < 0d) negativeVariableCount += 1)
    }

    def getNegativeIndices(vec: SparseVector[Double]): Iterator[Int] = {
        logger.debug("Getting negative indices..")
        for((i, v) <- vec.activeIterator; if v < 0) yield i
    }

    def exchange(): Unit = {
        logger.debug("Exchanging variables..")
        getNegativeIndices(xVector).foreach(exchangeXtoY)
        getNegativeIndices(yVector).foreach(exchangeYtoX)
        xVector.compact()
        yVector.compact()
        logger.debug("Done exchanging variables.")
    }

    def exchangeXtoY(index: Int): Unit = {
        yVector(index) = xVector(index)
        xVector(index) = 0d
    }

    def exchangeYtoX(index: Int): Unit = {
        xVector(index)= yVector(index)
        yVector(index) = 0d
    }


    def getAMatrix(vec: SparseVector[Double]): CSCMatrix[Double] = {
        logger.debug("Subsetting indicator matrix..")
        val builder = new CSCMatrix.Builder[Double](rows=bVector.length, bVector.length)
        for(x <- 0 until AMatrix.rows; y <- vec.activeKeysIterator) builder.add(x,y,AMatrix(x,y))
        logger.debug("Returning submatrix.")
        builder.result()
    }

    def sliceMatrix(matrix: CSCMatrix[Double], rFrom: Int, rTo: Int, cFrom: Int, cTo: Int): CSCMatrix[Double] = {
        val newMatrix = new CSCMatrix.Builder[Double](rTo - rFrom, cTo - cFrom)

        for(((x,y), v) <- matrix.activeIterator){
            if(x >= rFrom & x < rTo & y >= cFrom & y < cTo){
                newMatrix.add(x-rFrom, y-cFrom, v)
            }
        }
        newMatrix.result()
    }





    def solve(): DenseVector[Double] = {
        while(!isFeasibleSolution){

            if(negativeVariableCount < NValue){
                logger.debug(s"The number of infeasible solutions has been reduced from $NValue to $negativeVariableCount.")
                NValue = negativeVariableCount
                pValue = 3
                exchange()
            }
            else{
                if(pValue > 0){
                    logger.debug(s"No improvement. Number of tries left $pValue.")
                    pValue = pValue-1
                    exchange()
                }
                else{
                    logger.debug(s"No improvement. Doing single variable switch.")
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

            logger.debug("Reestimating x and y complementary solutions..")
            val Af = getAMatrix(xVector)

            logger.debug("Solving for Af for b..")



            xVector = LSMR.solve(Af, bVector, quiet = false, tolerance = 1e-4, regularization = 10000d, maxIter = 20)

            val Ag = getAMatrix(yVector)

            logger.debug("Calculating y..")
            yVector = Ag.t*((Af*xVector) - bVector)

            for((k,v) <- xVector.activeIterator; if abs(v) < 1e-12) xVector(k) = 0d
            for((k,v) <- yVector.activeIterator; if abs(v) < 1e-12) yVector(k) = 0d

            xVector.compact()
            yVector.compact()
        }

        xVector.toDenseVector
    }


}
