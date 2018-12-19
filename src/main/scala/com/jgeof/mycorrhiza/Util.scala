package com.jgeof.mycorrhiza

object Util {
    val fv = (x: Float) => BigDecimal(x).setScale(4, BigDecimal.RoundingMode.HALF_DOWN)
}
