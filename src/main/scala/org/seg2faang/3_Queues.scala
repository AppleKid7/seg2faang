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

  def peek: Option[A] = {
    self match {
      case Empty => None
      case ListNode(value, _) =>
        Some(value)
    }
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
  val qpeek1 = q5.peek
  println(s"peek1: $qpeek1")
  q5.print
  val (q6, q7) = q5.dequeue()
  q6.print
  q7.print
  val qpeek2 = q7.peek
  println(s"peek2: $qpeek2")
  val (q8, q9) = q7.dequeue()
  q8.print
  q9.print
  val empty = ImmutableQueueNode.Empty
  empty.print
  val (emptyNode, emptyTest) = empty.dequeue()
  emptyNode.print
  emptyTest.print
  val qpeek3 = empty.peek
  println(s"peek3: $qpeek3")
}

case class MutableQueueNode[A](value: A, var next: Option[MutableQueueNode[A]])

class Queue[A](var head: Option[MutableQueueNode[A]], var tail: Option[MutableQueueNode[A]]) { self =>
  def print: Unit = {
    @tailrec
    def loop(current: Option[MutableQueueNode[A]], acc: String): String = current match {
      case None =>
        acc
      case Some(MutableQueueNode(value, Some(next))) =>
        if (acc != "")
          loop(Some(next), s"$acc, ${value.toString()}")
        else
          loop(Some(next), value.toString())
      case Some(MutableQueueNode(value, None)) =>
        s"$acc, ${value.toString()}"
    }
    println(loop(head, ""))
  }

  def enqueue(value: A): Unit = {
    val node = MutableQueueNode[A](value, None)
    head match {
      case None =>
        self.head = Some(node)
        self.tail = Some(node)
      case Some(head) =>
        self.tail.map(n => n.next = Some(node))
        self.tail = Some(node)
    }
  }

  def dequeue(): Option[A] = {
    self.head match {
      case None =>
        None
      case Some(node) =>
        val retVal = Some(node.value)
        if (self.head == self.tail)
          val toRet = self.head
          self.head = None
          self.tail = None
          toRet.map(_.value)
        else
          val oldHead: Option[MutableQueueNode[A]] = self.head
          self.head = self.head.flatMap(_.next)
          oldHead match {
            case Some(node) =>
              node.next = None
              retVal
            case None =>
              retVal
          }
    }
  }

  def peek: Option[A] =
    self.head match {
      case Some(node) =>
        Some(node.value)
      case None =>
        None
    }
}

@main def runMutableQueue = {
  val q = Queue[Int](None, None)
  val q1 = q.enqueue(1)
  q.enqueue(2)
  q.enqueue(3)
  q.enqueue(4)
  q.enqueue(5)
  q.print
  println(s"peek: ${q.peek}")
  val a = q.dequeue()
  q.print
  println(a)
  println(s"peek: ${q.peek}")
  val b = q.dequeue()
  q.print
  println(b)
  println(s"peek: ${q.peek}")
  val empty = Queue[Int](None, None)
  val c = empty.dequeue()
  empty.print
  println(c)
  println(s"peek: ${empty.peek}")
}
