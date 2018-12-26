package com.jgeof.mycorrhiza.splits.solver

import breeze.linalg.{CSCMatrix, DenseMatrix, DenseVector, SparseVector, Tensor}

abstract class Solver {

    //def init(aMatrix: Any, bVector: DenseVector[Double]): Unit

    def solve(): DenseVector[Double]
}

object Solver {

    def objectiveFunc(aMatrix: DenseMatrix[Double], x: DenseVector[Double], bVector: DenseVector[Double]): Double = {

        val dvar = bVector.map(x => x*x).toArray.sum / bVector.length
        val destim = aMatrix*x
        var resVar = (for((x,y) <- destim.toArray.zip(bVector.toArray)) yield (x-y)*(x-y)).sum / bVector.length.toDouble
        math.abs(math.sqrt(dvar)-math.sqrt(resVar))/math.sqrt(dvar)
    }

    def objectiveFunc(aMatrix: CSCMatrix[Double], x: DenseVector[Double], bVector: DenseVector[Double]): Double = {

        val dvar = bVector.map(x => x*x).toArray.sum / bVector.length
        val destim = aMatrix*x
        var resVar = (for((x,y) <- destim.toArray.zip(bVector.toArray)) yield (x-y)*(x-y)).sum / bVector.length.toDouble
        math.abs(math.sqrt(dvar)-math.sqrt(resVar))/math.sqrt(dvar)
    }

    def meanSquare(aMatrix: DenseMatrix[Double], xxx: DenseVector[Double], bVector: DenseVector[Double]): Double = {

        val destim = aMatrix*xxx
        var resVar = (for((x,y) <- destim.toArray.zip(bVector.toArray)) yield (x-y)*(x-y)).sum / bVector.length.toDouble
        resVar
    }

    def meanSquare(aMatrix: CSCMatrix[Double], xxx: DenseVector[Double], bVector: DenseVector[Double]): Double = {

        val destim = aMatrix*xxx
        var resVar = (for((x,y) <- destim.toArray.zip(bVector.toArray)) yield (x-y)*(x-y)).sum / bVector.length.toDouble
        resVar
    }

    def meanSquare(aMatrix: CSCMatrix[Double], xxx: SparseVector[Double], bVector: SparseVector[Double]): Double = {

        val destim = aMatrix*xxx
        var resVar = (for((x,y) <- destim.toArray.zip(bVector.toArray)) yield (x-y)*(x-y)).sum / bVector.length.toDouble
        resVar
    }
}