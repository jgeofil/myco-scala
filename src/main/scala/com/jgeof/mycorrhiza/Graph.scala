package com.jgeof.mycorrhiza

import com.jgeof.mycorrhiza.samples.Sample
import com.jgeof.mycorrhiza.util.Timer
import Console.{GREEN, RED, RESET, YELLOW_B, UNDERLINED, WHITE}
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class Graph(name: String = "default-graph") extends LazyLogging { parent =>

    /** List of Samples. */
    private val samples = ArrayBuffer.empty[Sample]

    /** List of Clusters. */
    private var clusters = ArrayBuffer.empty[Cluster]

    /** List of Samples set as neighbors. */
    private val neighbors = ArrayBuffer.empty[(Sample, Sample)]

    /** The number of Clusters in the Graph. */
    def numClusters: Int = clusters.size

    /** The number of Samples in the Graph. */
    def numSamples: Int = samples.size

    /** The number of neighbor pairs in the Graph. */
    def numNeighbors: Int = neighbors.size

    private val timer = new Timer()

    logger.debug(s"Instance of Graph [$name] initialized.")

    /** Reads a list of samples from file.
      *
      * @param fileName Path to the file containing the samples.
      */
    def readSamplesFromFile(fileName: String): Unit = {
        logger.info(s"Reading Samples from file $fileName..")

        val lines = Source.fromFile(fileName).getLines()
        var thresh = 20

        timer.start()

        for(str <- lines) {
            val arr = str.split("\t")
            samples.append(Sample(arr.head, arr(1), arr.last))

            logger.whenDebugEnabled({
                if(samples.size % thresh == 0){
                    val perSecond = numSamples / timer.now()
                    thresh = perSecond.toInt * 10
                    logger.debug(s"Reading $perSecond samples per second, with $numSamples done..")
                }
            })
        }
        logger.info(s"Read $numSamples samples in ${timer.now()} seconds.")
    }

    /** Launch the clustering process.
      *
      */
    def run(): Unit = {
        populateClusters()

        while(clusters.size > 2){
            clusterDistances()
            val minClusters = findMinClusters()
            mergeMinClusters(minClusters._1, minClusters._2)
        }
    }

    /**
      * Creates a new Cluster1 and adds it to the graph.
      * @param sample The sample in the cluster.
      */
    def newCluster(sample: Sample): Unit = clusters += new Cluster1(sample)

    /**
      * Creates a new Cluster2 and adds it to the graph.
      * @param sample1 The first sample in the cluster.
      * @param sample2 The second sample in the cluster.
      */
    def newCluster(sample1: Sample, sample2: Sample): Unit = clusters += new Cluster2(sample1, sample2)

    /** Populates the cluster list from the sample list.*/
    def populateClusters(): Unit = {
        logger.info("Populating Clusters from Samples..")
        samples.foreach(sample => newCluster(sample))
        logger.debug(s"Populated $numClusters clusters from $numSamples samples.")
    }

    /** Finds the pair of Clusters that with the minimal distance.*/
    def findMinClusters(): (Cluster, Cluster) = {
        logger.debug(s"Finding minimal clusters from the $numClusters remaining, only considering ${clusters.size} candidates..")
        val result = clusters.combinations(2).toList.minBy(arr => arr.head -- arr.last)
        logger.whenDebugEnabled({
            logger.debug(s"Found minimal clusters ${result.head} and ${result.last} with distance ${result.head -- result.last}")
        })
        (result.head, result.last)
    }
    def findMinClusters(compareThese: ArrayBuffer[Cluster1], toThose: ArrayBuffer[Cluster1]): (Cluster, Cluster) = {
        logger.debug(s"Finding minimal clusters comparing $compareThese and $toThose..")
        val result = (for(x <- compareThese; y <- toThose) yield (x,y)).minBy(xy => xy._1 -- xy._2)
        logger.whenDebugEnabled({
            logger.debug(s"Found minimal clusters ${result._1} and ${result._2} with distance ${result._1 -- result._2}")
        })
        (result._1, result._2)
    }

    /**
      * Splits a cluster.
      * @param cluster The cluster to splits.
      * @return A list of new clusters.
      */
    def splitCluster(cluster: Cluster): (Cluster, Option[Cluster]) = {
        logger.debug(s"Graph contains $numClusters clusters, now splitting $cluster ..")

        cluster match {
            case c: Cluster1 => {
                logger.debug(s"Cluster $cluster is Cluster1, nothing to do.")
                (c, None)
            }
            case Cluster2(s1, s2) => {
                logger.debug(s"Cluster $cluster is Cluster2, removing it from the graph..")
                clusters -= cluster
                val newCluster1 = new Cluster1(s1)
                val newCluster2 = new Cluster1(s2)
                clusters += newCluster1 += newCluster2
                logger.debug(s"Cluster $cluster removed. Added resulting clusters $newCluster1 and $newCluster2.")
                logger.debug(s"Graph now contains $numClusters clusters.")
                (newCluster1, Some(newCluster2))
            }
        }
    }

    /**
      * Merges two clusters by first splitting them to find the samples on which to merge.
      * @param cluster1 First cluster.
      * @param cluster2 Second cluster.
      */
    def mergeMinClusters(cluster1: Cluster, cluster2: Cluster): Unit = {
        val splitResult1 = splitCluster(cluster1)
        val splitResult2 = splitCluster(cluster2)
        val newClusters1 = ArrayBuffer.empty[Cluster1]
        val newClusters2 = ArrayBuffer.empty[Cluster1]

        (splitResult1, splitResult2) match{
            case ((w: Cluster1, None),(y: Cluster1, None)) =>
                newClusters1.append(w)
                newClusters2.append(y)
            case ((w: Cluster1, Some(x: Cluster1)),(y: Cluster1, None)) =>
                newClusters1.append(w, x)
                newClusters2.append(y)
            case ((w: Cluster1, None),(y: Cluster1, Some(z: Cluster1))) =>
                newClusters1.append(w)
                newClusters1.append(y, z)
            case ((w: Cluster1, Some(x: Cluster1)),(y: Cluster1, Some(z: Cluster1))) =>
                newClusters1.append(w, x)
                newClusters1.append(y, z)
        }

        logger.debug("Finding minimal clusters from the newly split clusters..")
        val (x: Cluster1, y: Cluster1) = findMinClusters(newClusters1, newClusters2)

        logger.debug(s"Setting ${x.first} and ${y.first} as neighbors..")
        neighbors.append((x.first, y.first))
        logger.debug(s"There are now $numNeighbors pairs in the graph.")
        logger.whenDebugEnabled(printNeighborPairs())

        logger.debug("Removing temporary clusters..")
        clusters --= newClusters1 --= newClusters2

        if(cluster1.has(x.first)){
            clusters += mergeClusters(cluster1, x.first, cluster2, y.first)
        }else{
            clusters += mergeClusters(cluster1, y.first, cluster2, x.first)
        }
        logger.debug(s"There are now $numClusters in the graph.")
    }

    /** Update the distances when merge a triple of samples.
      *
      * @param x External sample one.
      * @param y Internal sample.
      * @param z External sample two.
      */
    def upD(x: Sample, y: Sample, z: Sample): Unit = {
        val xw = samples.map(w => (x, w, (2f/3)*(x-w) + (1f/3)*(y-w)))
        val zw = samples.map(w => (z, w, (2f/3)*(z-w) + (1f/3)*(y-w)))
        val xz = (1f/3)*((x-y)+(x-z)+(z-y))
        x.updateDist(z, xz)
        z.updateDist(x, xz)
        (xw++zw).foreach{case (s1, s2, f) => s1.updateDist(s2, f); s2.updateDist(s1, f)}
    }

    /**
      * Merges two Clusters on the specified sample pairing. The sample distances are also updated.
      * @param c1 Cluster one.
      * @param s1 Pairing sample from Cluster one.
      * @param c2 Cluster two.
      * @param s2 Pairing sample from Cluster two.
      * @return The merged Cluster.
      */
    def mergeClusters(c1: Cluster, s1: Sample, c2: Cluster, s2: Sample): Cluster = {
        logger.debug(s"Merging clusters $c1 and $c2 on samples $s1 and $s2..")

        val merged = (c1, c2) match {
            case (cx: Cluster1, cy:Cluster1) =>
                cx + cy
            case (cx: Cluster1, cy:Cluster2) if cy.fis(s2) =>
                upD(s1, s2, cy.second)
                cx + cy
            case (cx: Cluster1, cy:Cluster2) if cy.sis(s2) =>
                upD(s1, s2, cy.first)
                cy + cx
            case (cx: Cluster2, cy: Cluster1) if cx.fis(s1) =>
                upD(cx.second, s1, s2)
                cy + cx
            case (cx: Cluster2, cy: Cluster1) if cx.sis(s1) =>
                upD(cx.first, s1, s2)
                cx + cy
            case (cx: Cluster2, cy: Cluster2) if cx.fis(s1) & cy.fis(s2) =>
                upD(cx.second, s1, s2)
                upD(cx.second, s2, cy.second)
                cx \+ cy
            case (cx: Cluster2, cy: Cluster2) if cx.fis(s1) & cy.sis(s2) =>
                upD(cx.second, s1, s2)
                upD(cx.second, s2, cy.first)
                cy + cx
            case (cx: Cluster2, cy: Cluster2) if cx.sis(s1) & cy.fis(s2) =>
                upD(cx.first, s1, s2)
                upD(cx.first, s2, cy.second)
                cx + cy
            case (cx: Cluster2, cy: Cluster2) if cx.sis(s1) & cy.sis(s2) =>
                upD(cx.first, s1, s2)
                upD(cx.first, s2, cy.first)
                cx /+ cy
        }
        logger.debug(s"New cluster $merged created.")
        merged
    }

    /**
      * Prints all the pairs of sample neighbors.
      */
    def printNeighborPairs(): Unit = {
        var count = 0
        for((x, i) <- neighbors.zipWithIndex){
            printf("%10.10s ", x + (if(i % 20 == 0) "\n" else ""))
        }
    }

    /**
      * Prints a distance matrix for all the clusters in the graph.
      */
    def clusterDistances(): Unit = {
        print("\n")
        val ds = new ArrayBuffer[Float]()
        for(c1 <- clusters){
            for(c2 <- clusters){
                ds += c1--c2
            }
        }
        val avg = ds.sum/ds.size.toFloat
        val min = ds.min

        for(i <- clusters(0)++clusters) printf("%5.5s  ",i)
        print("\n")
        for((c1,j) <- clusters.zipWithIndex){
            printf("%10.10s  ",c1)
            for(c2 <- clusters){
                val f = c1--c2
                if(f == min) printf("%10.10s  ", GREEN+util.Util.fv(f))
                else if(f < avg) printf("%10.10s  ", RED+util.Util.fv(f))
                else printf("%10.10s  ", WHITE+util.Util.fv(f))
            }
            print("\n")
        }
        print("\n")
    }

    /**
      * Cluster of Samples.
      */
    class Cluster extends Iterable[Sample] with Traversable[Sample]{
        var list: List[Sample] = List()
        override def iterator: Iterator[Sample] = list.toIterator

        def has(s: Sample): Boolean = list.contains(s)

        /**
          * Base distance between a pair of clusters.
          * @param that Cluster.
          * @return Distance between the clusters.
          */
        def -(that: Cluster): Float = { for(x <- this; y <- that) yield x-y }.sum / (this.size * that.size).toFloat

        /**
          * Calculate the R value of the Cluster.
          * @return The R value.
          */
        def r: Float = {
            clusters.map(x => if(x != this) x - this else 0).sum / (clusters.size.toFloat - 2)
        }

        /**
          * Calculate the adjusted distance between a pair of clusters.
          * @param that Cluster.
          * @return The adjusted distance between the clusters.
          */
        def --(that: Cluster): Float = {
            (this - that) - (this.r + that.r)
        }
    }

    /** Cluster of 1 sample.*/
    class Cluster1(val first: Sample) extends Cluster{
        list = List(first)

        override def toString():String = s"$first"

        def +(that: Cluster1): Cluster2 = new Cluster2(this.first, that.first)
        def +(that: Cluster2): Cluster2 = new Cluster2(this.first, that.second)
    }

    /** Cluster of 2 samples. */
    class Cluster2(val first: Sample, val second: Sample) extends Cluster{
        list = List(first, second)

        override def toString():String = s"$first+$second"

        def fis(s: Sample): Boolean = first == s
        def sis(s: Sample): Boolean = second == s

        def +(that: Cluster1): Cluster2 = new Cluster2(this.first, that.first)
        def +(that: Cluster2): Cluster2 = new Cluster2(this.first, that.second)
        def \+(that: Cluster2): Cluster2 = new Cluster2(this.first, that.first)
        def /+(that: Cluster2): Cluster2 = new Cluster2(this.second, that.second)
    }

    /** Companion object for class Cluster1 with 1 samples.*/
    object Cluster1 {
        def unapply(arg: Cluster1): Option[Sample] = Some(arg.first)
    }

    /** Companion object for class Cluster2 with 2 samples.*/
    object Cluster2 {
        def unapply(arg: Cluster2): Option[(Sample, Sample)] = Some(arg.first, arg.second)
    }
}


object Graph{

}