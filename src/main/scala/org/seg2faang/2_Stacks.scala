package org.seg2faang

import scala.annotation.tailrec

enum StackElement[+A] { self =>
  case Empty
  case Element(value: A, next: StackElement[A])

  def print: Unit =
    @tailrec
    def loop(ln: StackElement[A], idx: Int, acc: String): String = ln match {
      case Empty =>
        acc
      case Element(e, nx) =>
        if (acc != "")
          loop(nx, idx + 1, s"$acc, ${e.toString()}")
        else
          loop(nx, idx + 1, e.toString())
    }
    println(loop(self, 0, ""))

  private def reverse[A](elem: StackElement[A]): StackElement[A] = {
    @tailrec
    def loop(current: StackElement[A], acc: StackElement[A]): StackElement[A] = {
      current match {
        case Empty => acc
        case Element(value, next) => loop(next, Element(value, acc))
      }
    }
    loop(elem, Empty)
  }

  def push[B >: A](value: B): StackElement[B] = {
    @tailrec
    def loop(current: StackElement[B], acc: StackElement[B]): StackElement[B] = {
      current match {
        case Empty =>
          reverse(Element(value, acc))
        case Element(value, next) =>
          loop(next, Element(value, acc))
      }
    }
    loop(self, Empty)
  }

  def pop[B >: A](): (StackElement[B], StackElement[B]) = {
    @tailrec
    def loop(
        previous: StackElement[B],
        current: StackElement[B],
        acc: StackElement[B]
    ): (StackElement[B], StackElement[B]) = {
      current match {
        case Empty =>
          (previous, reverse(acc))
        case Element(value, next) =>
          next match {
            case Empty =>
              (Element(value, next), reverse(acc))
            case _ =>
              loop(current, next, Element(value, acc))
          }
      }
    }
    loop(Empty, self, Empty)
  }

  def peek[B >: A](): Option[B] = {
    @tailrec
    def loop(previous: StackElement[B], current: StackElement[B]): Option[B] =
      current match {
        case Empty =>
          previous match {
            case Element(value, _) => Some(value)
            case Empty => None
          }
        case Element(value, next) =>
          loop(current, next)
      }
    loop(Empty, self)
  }
}

@main def runStack: Unit =
  val node1 = StackElement.Element(1, StackElement.Empty)
  node1.print
  val node2 = node1.push(2)
  node2.print
  val (popped, node3) = node2.pop()
  node3.print
  println(s"Test: $popped")
  val peekTest = node2.peek()
  println(s"Peek: $peekTest")
  val peekTest2 = node1.peek()
  println(s"Peek2: $peekTest2")
