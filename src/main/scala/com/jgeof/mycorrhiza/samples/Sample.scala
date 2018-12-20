package com.jgeof.mycorrhiza.samples

class Sample(identifier:String, origin:String) extends Genotype() {

    def getName: String = identifier

    override def toString(): String = s"[$identifier: $origin]"

    def -(that: Sample): Float = {
        val shared = this==that
        val same = this=?that
        (shared-same)/same.toFloat
    }

    def >(that: Sample): Boolean = this.getName > that.getName

}

object Sample {

    def apply(identifier: String, origin: String, genotype: String): Sample = {
        val sample = new Sample(identifier, origin)
        sample.initFromString(genotype)
        sample
    }

    def unapply(arg: Sample): Option[String] = Some(arg.getName)

}
