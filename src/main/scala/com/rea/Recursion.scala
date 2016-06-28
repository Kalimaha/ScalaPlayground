package com.rea

import scala.annotation.tailrec


object Recursion {

  def plusOne(n: Int): Int = n + 1

  def minusOne(n: Int): Int = n - 1

  @tailrec
  def add(a: Int, b: Int): Int = 
    if (a == 0) b
    else add(minusOne(a), plusOne(b))

  def sum(l: List[Int]): Int = {
      @tailrec
      def loop(acc: Int, l: List[Int]): Int = l match {
        case Nil => acc
        case h :: t => loop(acc + h, t)
      }
      loop(0, l)
  }

  def length(l: List[Int]): Int = {
      @tailrec
      def loop(acc: Int, l: List[Int]): Int = l match {
        case Nil => acc
        case h :: t => loop(1 + acc, t)
      }
      loop(0, l)
  }

  def map[A, B](as: List[A], f: A => B):List[B] = as match {
    case Nil => Nil
    case h :: t => f(h) :: map(t, f)
  }

  def filter[A](as: List[A], f: A => Boolean): List[A] = {
      def loop(acc: List[A], as: List[A]): List[A] = as match {
        case Nil => acc
        case h :: t => if (f(h)) h :: loop(acc, t) else loop(acc, t)
      }
      loop(Nil, as)
  }

  def append[A](a: List[A], b: List[A]): List[A] = a match {
    case Nil => b
    case h :: t => h :: append(t, b)
  }

  def flatten[A](l: List[List[A]]): List[A] = {
      @tailrec
      def loop(acc: List[A], l: List[List[A]]): List[A] = l match {
        case Nil => acc
        case h :: t => loop(append(acc, h), t)
      }
      loop(Nil, l)
  }

  def maximum(x: List[Int]): Int = {
      @tailrec
      def loop(currentMaximum: Int, x: List[Int]): Int = x match {
        case Nil => currentMaximum
        case h :: t => if (h > currentMaximum) loop(h, t) else loop(currentMaximum, t)
      }
      loop(0, x)
  }

  def reverse[A](x: List[A]): List[A] = {
      @tailrec
      def loop(acc: List[A], x: List[A]): List[A] = x match {
        case Nil => acc
        case h :: t => loop(h :: acc, t)
      }
      loop(Nil, x)
  }

}