package com.jgeof.mycorrhiza.splits.solver

import org.opensha.commons.calc.nnls.NNLSWrapper

class OpenSha extends Solver {

    var nnlsEngine = new NNLSWrapper()
    var ata = new Array[Double](0)
    var atb = new Array[Double](0)

    var solution = Array[Double]()

    def init(aMatrix: Array[Double], bVector: Array[Double]): Unit = {
        ata = aMatrix
        atb = bVector

        nnlsEngine = new NNLSWrapper(ata,atb.length, atb.length)
        solution = Array.ofDim(atb.length)
    }

    def solve(): Array[Double] = {
        nnlsEngine.solve(atb, solution)
        solution
    }
}
