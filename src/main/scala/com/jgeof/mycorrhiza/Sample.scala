package com.jgeof.mycorrhiza

class Sample(identifier:String, origin:String) extends Genotype() {

    def getName: String = identifier

    override def toString(): String = s"[$identifier: $origin]"

    def -(that: Sample): Float = {
        val shared = this==that
        val same = this=?that
        (shared-same)/same.toFloat
    }

}

object Sample {

    def unapply(arg: Sample): Option[String] = Some(arg.getName)

}
