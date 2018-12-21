package com.jgeof.mycorrhiza.samples

import com.jgeof.mycorrhiza.util.Exceptions._

import scala.collection.{mutable => m}

trait Genotype extends{
    import Genotype._

    private val genotype = m.BitSet.empty
    private val invalid = m.BitSet.empty
    var numLoci = 0

    def initFromString(rawGenotype: String): Unit = {
        numLoci = rawGenotype.length
        for(i <- rawGenotype.indices){
            val c = rawGenotype.charAt(i)
            c match {
                case 'A' => genotype.add((AlphaSize * i) + 0);
                case 'T' => genotype.add((AlphaSize * i) + 1);
                case 'G' => genotype.add((AlphaSize * i) + 2);
                case 'C' => genotype.add((AlphaSize * i) + 3);
                case 'N' => invalid.add(i)
                case c: Char => throw InvalidCharacter(c.toString)
            }
        }
    }

    def ===(that:Genotype):Int = numLoci - (this.invalid | that.invalid).size
    def =?(that:Genotype):Int = (this.genotype & that.genotype).size
}

object Genotype {

    val AlphaSize = 4

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

