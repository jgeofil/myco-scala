package com.jgeof.mycorrhiza.util

object Util {
    val fv = (x: Float) => BigDecimal(x).setScale(2, BigDecimal.RoundingMode.HALF_DOWN)
}
