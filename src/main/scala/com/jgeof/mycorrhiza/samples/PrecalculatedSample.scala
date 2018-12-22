package com.jgeof.mycorrhiza.samples

class PrecaculatedSample(identifier: String, origin: String) extends Sample(identifier, origin){
    def initDist(that: PrecaculatedSample, distance: Float): Unit = {
        updateDist(that, distance)
    }
}