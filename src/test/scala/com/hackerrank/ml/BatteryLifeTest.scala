package com.hackerrank.ml

class BatteryLifeTest extends org.scalatest.FunSuite {

  test("should predict") {
    assertResult(3.0)(BatteryLife.predict(1.5))
  }

}
