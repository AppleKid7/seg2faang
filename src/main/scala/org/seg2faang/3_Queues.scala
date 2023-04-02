package org.seg2faang

import scala.annotation.tailrec

enum ImmutableQueueNode[+A] { self =>
  case ListNode(value: A, next: ImmutableQueueNode[A])
  case Empty

  def print: Unit =
    @tailrec
    def loop(current: ImmutableQueueNode[A], acc: String): String = current match {
      case Empty =>
        acc
      case ListNode(e, nx) =>
        if (acc != "")
          loop(nx, s"$acc, ${e.toString()}")
        else
          loop(nx, e.toString())
    }
    println(loop(self, ""))

  def enqueue[B >: A](value: B): ImmutableQueueNode[B] = {
    @tailrec
    def loop(current: ImmutableQueueNode[B], acc: ImmutableQueueNode[B]): ImmutableQueueNode[B] = {
      current match {
        case Empty =>
          reverse(ListNode(value, acc))
        case ListNode(value, next) =>
          loop(next, ListNode(value, acc))
      }
    }
    loop(self, Empty)
  }

  def dequeue[B >: A](): (ImmutableQueueNode[B], ImmutableQueueNode[B]) = {
    // @tailrec
    def loop(
        previous: ImmutableQueueNode[B],
        current: ImmutableQueueNode[B],
        acc: ImmutableQueueNode[B]
    ): (ImmutableQueueNode[B], ImmutableQueueNode[B]) = {
      current match {
        case Empty =>
          (previous, reverse(acc))
        case ListNode(value, next) =>
          (ListNode(value, Empty), next)
      }
    }
    loop(Empty, self, Empty)
  }

  private def reverse[A](elem: ImmutableQueueNode[A]): ImmutableQueueNode[A] = {
    @tailrec
    def loop(current: ImmutableQueueNode[A], acc: ImmutableQueueNode[A]): ImmutableQueueNode[A] = {
      current match {
        case Empty => acc
        case ListNode(value, next) => loop(next, ListNode(value, acc))
      }
    }
    loop(elem, Empty)
  }
}

@main def runImmutableQueue = {
  val q = ImmutableQueueNode.Empty
  val q1 = ImmutableQueueNode.ListNode(1, ImmutableQueueNode.Empty)
  val q2 = q1.enqueue(2)
  val q3 = q2.enqueue(3)
  val q4 = q3.enqueue(4)
  val q5 = q4.enqueue(5)
  q5.print
  val (q6, q7) = q5.dequeue()
  q6.print
  q7.print
  val (q8, q9) = q7.dequeue()
  q8.print
  q9.print
  val empty = ImmutableQueueNode.Empty
  empty.print
  val (emptyNode, emptyTest) = empty.dequeue()
  emptyNode.print
  emptyTest.print
}
