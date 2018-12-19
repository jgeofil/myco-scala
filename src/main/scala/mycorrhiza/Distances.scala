package mycorrhiza



object Distances {

    type Metric = (Sample, Sample) => Float
    type DistMatrix = Array[Array[Float]]

    def toDistanceMatrix(samples: List[Sample], metric: Metric): DistMatrix = {
        var matrix = Array.ofDim[Float](samples.length, samples.length)
        for(i <- samples.indices){
            for(j <- i until samples.length){
                val f = metric(samples(i),samples(j))
                matrix(i)(j) = f
                matrix(j)(i) = f
            }
        }
        matrix
    }

    def jukesCantor(rate: Float=0.25f)(sa: Sample, sb:Sample):Float = {
        val same = sa == sb
        val valid = sa =? sb
        (valid-same)/valid.toFloat
    }

    def printDistanceMatrix(matrix: DistMatrix)={
        for(line <- matrix){
            for(i <- line){
                print(Util.fv(i));print("\t")
            }
            print("\n")
        }
    }
}
