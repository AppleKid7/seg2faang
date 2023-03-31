package org.seg2faang

import scala.annotation.tailrec

// enum QueueNode[+A] { self =>
//   case ListNode(value: A, next: QueueNode[A])
//   case Empty
// }
case class QueueNode[A](value: A, var next: Option[QueueNode[A]])

case class Queue[A](head: Option[QueueNode[A]], tail: Option[QueueNode[A]]) { self =>
  def enqueue(value: A): Queue[A] =
    self match {
      case Queue(None, _) =>
        Queue(Some(QueueNode(value, None)), Some(QueueNode(value, None)))
      case Queue(_, Some(tail)) =>
        val newTail = Some(QueueNode(value, None))
        tail.next = newTail
        self.copy(tail = newTail)
      case Queue(Some(head), None) =>
        throw new RuntimeException("Should never be the case")
    }
  def dequeue(): A = ???
  def peek(): A = ???
  def print: Unit =
    @tailrec
    def loop(current: Option[QueueNode[A]], acc: String): String = current match {
      case None =>
        acc
      case Some(QueueNode(e, nx)) =>
        if (acc != "")
          loop(nx, s"$acc, ${e.toString()}")
        else
          loop(nx, e.toString())
    }
    println(loop(self.head, ""))
}

@main def runQueue = ???

