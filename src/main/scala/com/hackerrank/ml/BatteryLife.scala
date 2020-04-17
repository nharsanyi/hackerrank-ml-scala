package com.hackerrank.ml

import scala.io.Source
import scala.math.pow

object BatteryLife {

  val training_path = "src/main/resources/battery_life/trainingdata.txt"
  val training_data: scala.List[(Double, Double)] = load_training_data()

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val timeCharged = stdin.readLine.trim.toDouble
    val y = predict(timeCharged)
    print(f"$y%1.2f")

  }

  def predict(x: Double): Double = {
    val max_y = training_data.map(_._2).max
    if (x <= 4) x * 2 else max_y
  }

  def predict_Lin_reg(x: Double) = {
    val (slope, intercept, _) = calculate_coefficients()
    val y = intercept + slope * x
    y
  }

  def calculate_coefficients() = {
    val n = training_data.length
    val sumX = training_data.foldLeft(0.0)(_ + _._1)
    val sumY = training_data.foldLeft(0.0)(_ + _._2)
    val sumXPow = training_data.foldLeft(0.0){case (res, tuple) => res + pow(tuple._1, 2)}
    val sumYPow = training_data.foldLeft(0.0){case (res, tuple) => res + pow(tuple._2, 2)}
    val sumXY = training_data.foldLeft(0.0){case (res, tuple) => {
      val x: Double = tuple._1
      val y: Double = tuple._2
      res + (x * y)
    }}

    val dn = (n * sumXPow) - pow(sumX, 2)

    val slope = ((n * sumXY) - (sumX * sumY)) / dn

    val intercept =  ((sumY * sumXPow) - (sumX * sumXY)) / dn

    val t1 = ((n * sumXY) - (sumX * sumY)) * ((n * sumXY) - (sumX * sumY))

    val t2 = (n * sumXPow) - pow(sumX, 2)
    val t3 = (n * sumYPow) - pow(sumY, 2)

    if (t2 * t3 != 0.0)
      (slope, intercept, t1 / (t2 * t3))
    else
      (slope, intercept, 0.0)

  }

  private def load_training_data(): scala.List[(Double, Double)] = {
    Source.fromFile(training_path).getLines.map(line => {
      val parts = line.split(",")
      (parts(0).toDouble, parts(1).toDouble)
    }).toList
  }
}
