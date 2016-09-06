package com.manning.chapter4

object EithersMaths {

  def mean(l: List[Double]): Either[String, Double] = {
    if (l.isEmpty) Left("The list provided is empty.")
    else Right(l.sum / l.length)
  }
}