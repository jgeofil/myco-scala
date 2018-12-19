package mycorrhiza

import annotation.tailrec
import scala.collection.{mutable => m}
import mycorrhiza.Exceptions._

class Genotype(rawGenotype: String) extends Iterable[Int] {
    def numLoci: Int = rawGenotype.length
    val (genotype , valid) = listOfCharToBitset(rawGenotype.toList)


    override def iterator: Iterator[Int] = genotype.toIterator
    def ==(that: Genotype): Float = Genotype.same(this, that)
    def =?(that: Genotype): Float = Genotype.valid(this, that)


    def listOfCharToBitset(chars: List[Char]): (m.BitSet, m.BitSet) = {
        @tailrec
        def bitSetBuilder(chars: List[Char], set: m.BitSet, valid: m.BitSet, count:Int): (m.BitSet, m.BitSet) = {
            chars match {
                case Nil => (set, valid)
                case x :: tail => bitSetBuilder(tail,
                    Genotype.addToSet(set, count, x),
                    Genotype.addValid(valid, count, x),
                    count+1)
            }
        }
        bitSetBuilder(chars, m.BitSet.empty, m.BitSet.empty, 0)
    }
}

object Genotype {

    val AlphaSize = 4

    val addValid = (set: m.BitSet, count: Int, char: Char) => if (char != 'N') set + count else set

    def addToSet(set: m.BitSet, count: Int, char: Char): m.BitSet = char match {
        case 'A' => set + ((AlphaSize * count) + 0)
        case 'T' => set + ((AlphaSize * count) + 1)
        case 'G' => set + ((AlphaSize * count) + 2)
        case 'C' => set + ((AlphaSize * count) + 3)
        case 'N' => set
        case c: Char => throw InvalidCharacter(c.toString)
    }

    def same(a:Genotype, b:Genotype):Int = (a.genotype&b.genotype).size
    def valid(a:Genotype, b:Genotype):Int = (a.valid&b.valid).size


    def distance(a: Genotype, b: Genotype): Float = {
        val same: m.BitSet = a.genotype & b.genotype
        val valid: m.BitSet = a.valid & b.valid
        (valid.size - same.size.toFloat) / valid.size
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

