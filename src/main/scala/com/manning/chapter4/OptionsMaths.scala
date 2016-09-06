package com.manning.chapter4

object OptionsMaths {

  def mean(l: List[Double]): Option[Double] = {
    if (l.isEmpty) None
    else Some(l.sum / l.length)
  }

  def variance(ls: List[Double]): Option[Double] = {
    mean(ls) flatMap (m => mean(ls.map(l => math.pow(l - m, 2))))
  }
}