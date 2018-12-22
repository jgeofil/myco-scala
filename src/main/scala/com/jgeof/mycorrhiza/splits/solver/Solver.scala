package com.jgeof.mycorrhiza.splits.solver

abstract class Solver {

    def init(aMatrix: Array[Double], bVector: Array[Double]): Unit

    def solve(): Array[Double]
}

/**
val rand = new Random()

        def getRandomChild(w: DenseVector[Double], alpha: Double = 1d): DenseVector[Double] = {
            val pos = rand.nextInt(w.length)
            val step = (rand.nextDouble()-0.5)*alpha
            val newW = w.copy
            newW.update(pos, math.abs(step+newW.valueAt(pos)))
            newW
        }


        val threshold = 1e-6d
//w=nnls.minimize(sata.toDenseMatrix,atb)
        println("Solved")
        //w = w.map(x => if(x>0) x else 0)
        var residual = objectiveFunc(sata, w, atb)
        println(residual)
        var time = 1
        var t = 1000000d
        while(residual > threshold){
            time+=1
            t = 1000000d/time
            val child = getRandomChild(w)
            val childResidual = objectiveFunc(sata, child, atb)

            val diff = childResidual - residual
            val prob = math.exp(-diff/t)
            val probThresh = rand.nextDouble()
            if(time%100000==0) printf("%5.5s\t%5.5s\tRes: %5.5f \tNRes: %5.5f\tDiff: %5.5f\tE: %5.5f\tEt: %5.5f\tT: %5.5f \n",residual<childResidual, prob>probThresh, residual, childResidual, diff, prob, probThresh, t)
            if(diff < 0d || prob  > probThresh){
                residual = childResidual
                w = child
            }
            if(time%10000==0)chartFunc(time, residual)
            Thread.sleep(0)
        }
  */