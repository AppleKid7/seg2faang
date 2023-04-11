package org.seg2faang
import scala.annotation.tailrec

case class MutableDoubleLinkedNode[A](var value: A, var next: Option[MutableDoubleLinkedNode[A]], var prev: Option[MutableDoubleLinkedNode[A]])

class MutableDoubleLinkedList[A](var head: Option[MutableDoubleLinkedNode[A]], var tail: Option[MutableDoubleLinkedNode[A]]) { self =>
  def print = {
    @tailrec
    def loop(current: Option[MutableDoubleLinkedNode[A]], acc: String): String =
      current match {
        case Some(current) =>
          if (acc == "")
            loop(current.next, current.value.toString())
          else
            loop(current.next, s"$acc, ${current.value.toString()}")
        case None =>
          acc
      }
    loop(self.head, "")
  }

  def prepend(data: A) =
    val wasEmpty = head.isEmpty && tail.isEmpty
    val newNode = MutableDoubleLinkedNode[A](data, None, None)
    head match {
      case Some(currHead) =>
        newNode.next = head
        currHead.prev = Some(newNode)
        head = Some(newNode)
      case _ =>
        head = Some(newNode)
    }
    if (wasEmpty)
      tail = head


  def append(data: A) =
    val wasEmpty = head.isEmpty && tail.isEmpty
    val newNode = MutableDoubleLinkedNode[A](data, None, None)
    tail match {
      case Some(currTail) =>
        currTail.next = Some(newNode)
        newNode.prev = tail
        tail = Some(newNode)
      case None =>
        tail = Some(newNode)
    }
    if (wasEmpty)
      head = tail

  def inserAfter(prevNode: MutableDoubleLinkedNode[A], data: A) = ???
  def delete(nodeToDelete: MutableDoubleLinkedNode[A]) = ???
}

@main def runMutableDLL =
  val dll = new MutableDoubleLinkedList[Int](None, None)
  println(dll.print)
  dll.prepend(2)
  dll.prepend(1)
  println(dll.print)
  dll.append(3)
  println(dll.print)


enum DoubleLinkedNode[+A]:
  case Node(value: A, next: DoubleLinkedNode[A], prev: DoubleLinkedNode[A])
  case Empty