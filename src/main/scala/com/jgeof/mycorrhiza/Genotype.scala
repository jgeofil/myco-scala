package com.jgeof.mycorrhiza

import scala.collection.{mutable => m}
import com.jgeof.mycorrhiza.Exceptions._

class Genotype(gen: (m.BitSet, m.BitSet), numLoci: Int) extends Iterable[Int] {
    import Genotype._

    val genotype = gen._1
    val invalid = gen._2

    def this(rawGenotype: String) = this(Genotype.stringToBitset(rawGenotype), rawGenotype.length)


    def valid(a:Genotype, b:Genotype):Int = numLoci-(a.invalid|b.invalid).size
    def same(a:Genotype, b:Genotype):Int = (a.genotype&b.genotype).size


    override def iterator: Iterator[Int] = genotype.toIterator
    def ==(that: Genotype): Float = same(this, that)
    def =?(that: Genotype): Float = valid(this, that)

}

object Genotype {

    val AlphaSize = 4

    def stringToBitset(chars: String): (m.BitSet, m.BitSet) = {
        val set = m.BitSet.empty
        val iv = m.BitSet.empty

        for(i <- chars.indices){
            val c = chars.charAt(i)
            c match {
                case 'A' => set.add((AlphaSize * i) + 0);
                case 'T' => set.add((AlphaSize * i) + 1);
                case 'G' => set.add((AlphaSize * i) + 2);
                case 'C' => set.add((AlphaSize * i) + 3);
                case 'N' => iv.add(i)
                case c: Char => throw InvalidCharacter(c.toString)
            }
        }
        (set, iv)
    }

    def naiveDistance(a: String, b: String): Float = {
        val same = a.zip(b).map({
            case ('N',_) => 0
            case (_,'N') => 0
            case (y,x) if x==y => 1
            case _ => 0
        })
        val valid = a.zip(b).count(x => x._2!='N' && x._1!='N')
        (valid.toFloat-same.sum)/valid.toFloat
    }
}

