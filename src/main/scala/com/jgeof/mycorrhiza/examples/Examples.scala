package com.jgeof.mycorrhiza.examples

class Examples {


}
object Example1 extends Examples {
    /**
    d['A','B']=5
    d['A','C']=4
    d['A','D']=7
    d['A','E']=6
    d['A','F']=8
    d['B','C']=7
    d['B','D']=10
    d['B','E']=9
    d['B','F']=11
    d['C','D']=7
    d['C','E']=6
    d['C','F']=8
    d['D','E']=5
    d['D','F']=9
    d['E','F']=8
    */
    val s = List("A","B","C","D","E","F")
    val o = Map("A"->"0","B"->"0","C"->"0","D"->"0","E"->"0","F"->"0")
    var d = Map(
        ("A","B")->5f,
        ("A","C")->4f,
        ("A","D")->7f,
        ("A","E")->6f,
        ("A","F")->8f,
        ("B","C")->7f,
        ("B","D")->10f,
        ("B","E")->9f,
        ("B","F")->11f,
        ("C","D")->7f,
        ("C","E")->6f,
        ("C","F")->8f,
        ("D","E")->5f,
        ("D","F")->9f,
        ("E","F")->8f
    )
}

object Example2 extends Examples {
    /**
    d['A','B']=5
    d['A','C']=4
    d['A','D']=7
    d['A','E']=6
    d['A','F']=8
    d['B','C']=7
    d['B','D']=10
    d['B','E']=9
    d['B','F']=11
    d['C','D']=7
    d['C','E']=6
    d['C','F']=8
    d['D','E']=5
    d['D','F']=9
    d['E','F']=8
      */
    val s = List("A","B","C","D")
    val o = Map("A"->"0","B"->"0","C"->"0","D"->"0")
    val d = Map(
        ("A","B")->7f,
        ("A","C")->12f,
        ("A","D")->11f,
        ("B","C")->7f,
        ("B","D")->10f,
        ("C","D")->8f
    )
}