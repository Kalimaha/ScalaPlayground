package com.rea

case class Product(id: String, name: String)

object SASUtils {
  val URL = "http://www.google.com?page="

  def url(page: Int): String = URL + page

  def page(url: String): Option[String] = ???

  def convert(xml: String): List[Product] = ???

  def products(acc: List[Product]): List[Product] = ???

  def toJSON(l: List[Product]): String = ???

  def writeFile(json: String): Option[String] = ???

  def main(args: Array[String]) {

  }
}