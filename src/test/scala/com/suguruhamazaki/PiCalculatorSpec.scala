package com.suguruhamazaki

import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class PiCalculatorSpec extends FlatSpec with Matchers with PiCalculator {

  "PiCalculator" should "calculate a portion of Pi" in {
    calculatePiFor(0, 1) should be (4.0 +- 0.001)
    calculatePiFor(1, 1) should be (-1.333 +- 0.001)
  }
}
