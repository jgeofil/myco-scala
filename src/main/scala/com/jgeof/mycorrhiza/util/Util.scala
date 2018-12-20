package com.jgeof.mycorrhiza.util

object Util {
    val fv = (x: Float) => BigDecimal(x).setScale(4, BigDecimal.RoundingMode.HALF_DOWN)
}
