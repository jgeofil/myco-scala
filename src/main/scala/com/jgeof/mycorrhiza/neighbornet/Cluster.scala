package com.jgeof.mycorrhiza.neighbornet

import com.jgeof.mycorrhiza.Sample

class Cluster(val first: Sample, var second: Option[Sample]){

    def this(first: Sample) = this(first, None)

    def <-->(that: Cluster): Float = {
        (this, that) match {
            case (Cluster(Sample(n1), None), Cluster(Sample(n2), None)) => 1f
            case (Cluster(Sample(n1), Some(Sample(n2))), Cluster(Sample(n3), None)) => 1f
            case (Cluster(Sample(n1), None), Cluster(Sample(n2), Some(Sample(n3)))) => 1f
            case (Cluster(Sample(n1), Some(Sample(n2))), Cluster(Sample(n3), Some(Sample(n4)))) => 1f
        }
    }
}

object Cluster {
    def unapply(arg: Cluster): Option[(Sample, Option[Sample])] = Some((arg.first, arg.second))
}