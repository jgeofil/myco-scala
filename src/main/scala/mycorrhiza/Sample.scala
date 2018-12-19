package mycorrhiza

import scala.io.Source

class Sample(identifier:String, origin:String, rawGenotype: String) extends Genotype(rawGenotype) {

    def getName: String = identifier
}

object Sample {
    def readFromFile(fileName: String): Iterator[Sample] = {
        val lines = Source.fromFile(fileName).getLines()

        for(str <- lines) yield {
            val arr = str.split("\\s")
            new Sample(arr.head, arr(1), arr.last)
        }
    }
}
