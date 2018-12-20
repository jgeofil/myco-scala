package com.jgeof.mycorrhiza

import com.jgeof.mycorrhiza.Graph.{ClusterList, MainClusterList, TempClusterList}
import com.jgeof.mycorrhiza.samples.Sample

import scala.collection.mutable.{ArrayBuffer, TreeSet}
import scala.io.Source

class Graph { parent =>

    /** List of Samples */
    private val samples = ArrayBuffer.empty[Sample]

    /** List of Clusters */
    private val clusters = ArrayBuffer.empty[Cluster]

    /** List of unmerged clusters*/
    private var tempClusters = ArrayBuffer.empty[Cluster]

    /** Reads a list of samples from file.s
      *
      * @param fileName Path to the file containing the samples.
      */
    def readSamplesFromFile(fileName: String): Unit = {
        println("Reading samples form file...")
        val lines = Source.fromFile(fileName).getLines()
        var t0 = System.nanoTime()
        var sampleCount = 0
        var thresh = 20

        for(str <- lines) {
            val arr = str.split("\t")
            samples.append(Sample(arr.head, arr(1), arr.last))
            sampleCount += 1
            if(sampleCount % thresh == 0){
                val perSecond = thresh/((System.nanoTime()-t0)*1e-9)
                thresh = perSecond.toInt*10
                println(s"Reading $perSecond samples per second, with $sampleCount done...")
                t0 = System.nanoTime()
            }
        }
    }

    def populateClusters(): Unit = {
        samples.foreach(sample => newCluster(sample))
    }

    def populateTempClusters(cluster1: Cluster, cluster2: Cluster): Unit = {
        tempClusters = clusters.clone()
        cluster1 match { case clust: Cluster2 => removeAndTransferCluster2(clust)}
        cluster2 match { case clust: Cluster2 => removeAndTransferCluster2(clust)}
    }

    def findMinClusters(choice: ClusterList): (Cluster, Cluster) = {
        val cl = choice match {
            case MainClusterList() =>  clusters
            case TempClusterList() => tempClusters
        }

        var min = Float.PositiveInfinity
        var res: (Cluster, Cluster) = (cl.head, cl.last)
        cl.combinations(2).toList.foreach({comb =>
            val d = comb.head <-> comb.last
            if(d < min) min = d; res = (comb.head, comb.last)
        })
        res
    }

    def newCluster(sample: Sample): Unit = clusters += new Cluster1(sample)
    def newCluster(sample1: Sample, sample2: Sample): Unit = clusters += new Cluster2(sample1, sample2)

    def removeAndTransferCluster2(cluster2: Cluster2): Unit = (s1: Sample, s2: Sample) => {
        tempClusters.remove(tempClusters.indexOf(cluster2))
        tempClusters += new Cluster1(s1)
        tempClusters += new Cluster1(s2)
    }

    class Cluster extends Iterable[Sample] with Traversable[Sample]{
        var list: List[Sample] = List()
        override def iterator: Iterator[Sample] = list.toIterator

        def <->(that: Cluster): Float = {
            for(x <- this; y <- that) yield x-y
        }.sum / (this.size * that.size).toFloat

        def r: Float = {
            clusters.map(x => if(x != this) x <-> this else 0).sum / (clusters.size.toFloat - 2)
        }

        def <*>(that: Cluster): Float = {
            (this <-> that) - (this.r + that.r)
        }

        def >(that: Cluster): Boolean = this.r > that.r
    }

    class Cluster1(val first: Sample) extends Cluster{
        list = List(first)
    }

    class Cluster2(val first: Sample, val second: Sample) extends Cluster{
        list = List(first, second)
    }

    object Cluster1 {
        def unapply(arg: Cluster1): Option[Sample] = Some(arg.first)
    }

    object Cluster2 {
        def unapply(arg: Cluster2): Option[(Sample, Sample)] = Some(arg.first, arg.second)
    }
}


object Graph{
    case class ClusterList()
    case class MainClusterList() extends ClusterList
    case class TempClusterList() extends ClusterList
}