package org.seg2faang

import scala.annotation.tailrec

// Scala 2 version:
// sealed trait LLNode[+A] { self =>
//   def print: Unit =
//     def loop(ln: LLNode[A], idx: Int): Unit = ln match {
//       case LLNode.Empty =>
//         ()
//       case LLNode.Node(e, nx) =>
//         println(e)
//         loop(nx, idx + 1)
//     }
//     loop(self, 0)

//   def insertNode[B >: A](x: B, idx: Int): LLNode[B] = {
//     def loop(ln: LLNode[B], i: Int): LLNode[B] = ln match {
//       case LLNode.Empty =>
//         if (i < idx)
//           LLNode.Empty
//         else
//           LLNode.Node(x, LLNode.Empty)
//       case LLNode.Node(e, nx) =>
//         if (i < idx)
//           LLNode.Node(e, loop(nx, i + 1))
//         else
//           LLNode.Node(x, ln)
//     }
//     loop(self, 0)
//   }
// }
// object LLNode {
//   case class Node[A](value: A, next: LLNode[A]) extends LLNode[A]
//   case object Empty extends LLNode[Nothing]
// }

// Scala 3 version:
enum LLNode[+A] { self =>
  case Node(value: A, next: LLNode[A])
  case Empty

  def print: Unit =
    @tailrec
    def loop(ln: LLNode[A], idx: Int, acc: String): String = ln match {
      case Empty =>
        acc
      case Node(e, nx) =>
        if (acc != "")
          loop(nx, idx + 1, s"$acc, ${e.toString()}")
        else
          loop(nx, idx + 1, e.toString())
    }
    println(loop(self, 0, ""))

  def insertNode[B >: A](x: B, idx: Int): LLNode[B] = {
    def loop(ln: LLNode[B], i: Int): LLNode[B] = ln match {
      case Empty =>
        if (i < idx)
          Empty
        else
          Node(x, Empty)
      case Node(e, nx) =>
        if (i < idx)
          Node(e, loop(nx, i + 1))
        else
          Node(x, ln)
    }
    loop(self, 0)
  }

  private def reverse[A](ln: LLNode[A]): LLNode[A] = {
    @tailrec
    def loop(ln: LLNode[A], acc: LLNode[A]): LLNode[A] = ln match {
      case Empty =>
        acc
      case Node(e, nx) =>
        loop(nx, Node(e, acc))
    }
    loop(ln, Empty)
  }

  def insertNodeTailrec[B >: A](x: B, idx: Int): LLNode[B] = {
    @tailrec
    def loop(ln: LLNode[B], i: Int, acc: LLNode[B]): LLNode[B] = ln match {
      case Empty =>
        if (i < idx)
          reverse(acc)
        else
          reverse(Node(x, acc))
      case Node(e, nx) =>
        if (i < idx)
          loop(nx, i + 1, Node(e, acc))
        else
          reverse(Node(e, Node(x, acc)))
    }
    loop(self, 0, Empty)
  }

  def insertNodeLastTailrec[B >: A](x: B): LLNode[B] = {
    @tailrec
    def loop(ln: LLNode[B], acc: LLNode[B]): LLNode[B] = ln match {
      case Empty =>
        reverse(Node(x, acc))
      case Node(e, nx) =>
        loop(nx, Node(e, acc))
    }
    loop(self, Empty)
  }

  def deleteNode(idx: Int): LLNode[A] = {
    def loop(ln: LLNode[A], i: Int): LLNode[A] = ln match {
      case Empty =>
        Empty
      case Node(e, nx) =>
        if (idx == 0)
          nx
        else if (i < idx - 1)
          Node(e, loop(nx, i + 1))
        else
          nx match {
            case Empty => Node(e, Empty)
            case Node(_, nx) => Node(e, nx)
          }
    }
    loop(self, 0)
  }

  def deleteNodeTailrec(idx: Int): LLNode[A] = {
    @tailrec
    def loop(ln: LLNode[A], i: Int, acc: LLNode[A]): LLNode[A] = ln match {
      case Empty =>
        reverse(acc)
      case Node(e, nx) =>
        if (idx == 0)
          nx
        else if (i < idx - 1)
          loop(nx, i + 1, Node(e, acc))
        else
          nx match {
            case Empty => Node(e, Empty)
            case Node(_, nx) => reverse(Node(e, nx))
          }
    }
    loop(self, 0, Empty)
  }

  def deleteNodeByValueTailrec[B >: A](value: B): LLNode[B] = {
    @tailrec
    def loop(ln: LLNode[B], value: B, acc: LLNode[B]): LLNode[B] = ln match {
      case Empty =>
        reverse(acc)
      case Node(e, nx) =>
        if (e == value)
          reverse(acc)
        else
          loop(nx, value, Node(e, acc))
    }
    loop(self, value, Empty)
  }
}

@main def run: Unit =
  val newList = LLNode.Node(1, LLNode.Empty)
  newList.print
  val newListAdd = newList.insertNodeTailrec(2, 1)
  newListAdd.print
  val newListAdd2 = newListAdd.insertNodeTailrec(3, 1)
  newListAdd2.print
  val remove = newListAdd2.deleteNode(2)
  remove.print
  val insertLast = remove.insertNodeLastTailrec(4)
  insertLast.print
  val deleteByValue = insertLast.deleteNodeByValueTailrec(4)
  deleteByValue.print
