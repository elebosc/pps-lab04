package it.unibo.pps.tasks.typeclasses

import it.unibo.pps.tasks.typeclasses.Ex4Summables.{sumAll, sumAllInt}
import it.unibo.pps.u03.Sequences.Sequence.{Cons, Nil}
import org.junit.Assert.assertEquals
import org.junit.Test

class SummablesTest:
  
  @Test def testSumAllInt(): Unit =
    val intSeq = Cons(10, Cons(20, Cons(30, Nil())))
    val sum = sumAllInt(intSeq)
    val expectedSum = 60
    assertEquals(expectedSum, sum)
  
  @Test def testSumAllWithInts(): Unit =
    val intSeq = Cons(10, Cons(20, Cons(30, Nil())))
    val sum = sumAll(intSeq)
    val expectedSum = 60
    assertEquals(expectedSum, sum)
  
  @Test def testSumAllWithDoubles(): Unit =
    val doubleSeq = Cons(10.0, Cons(20.0, Cons(30.0, Nil())))
    val sum = sumAll(doubleSeq)
    val expectedSum = 60.0
    val delta = 10e-9
    assertEquals(expectedSum, sum, delta)
    
  @Test def testSumAllWithStrings(): Unit =
    val stringSeq = Cons("10", Cons("20", Cons("30", Nil())))  
    val sum = sumAll(stringSeq)
    val expectedSum = "102030"
    assertEquals(expectedSum, sum)
    