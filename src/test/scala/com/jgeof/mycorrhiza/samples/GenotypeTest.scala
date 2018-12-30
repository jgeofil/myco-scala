package com.jgeof.mycorrhiza.samples

import org.scalatest.{BeforeAndAfter, FunSuite}

class GenotypeTest extends FunSuite with BeforeAndAfter{

    var genotype1: Genotype = _

    before{
        genotype1 = new Genotype {}
    }

    test("Genotype.initFromString | Size with even number"){
        genotype1.initFromString("ATGC")
        assert(genotype1.numLoci === 4)
    }

    test("Genotype.initFromString | Size with odd number"){
        genotype1.initFromString("ATGCC")
        assert(genotype1.numLoci === 5)
    }

    test("Genotype.initFromString | Size with ambiguous positions in the center"){
        genotype1.initFromString("ANTG-C")
        assert(genotype1.numLoci === 6)
    }

    test("Genotype.initFromString | Size with ambiguous positions at start and end"){
        genotype1.initFromString("NN--ATGCNN--")
        assert(genotype1.numLoci === 12)
    }

    test("Genotype.initFromString | Size with ambiguous positions"){
        genotype1.initFromString("ANTG-C")
        assert(genotype1.numLoci === 4)
    }
}
