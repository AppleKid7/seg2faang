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

  def deleteNode(idx: Int): LLNode[A] = {
    def loop(ln: LLNode[A], i: Int): LLNode[A] = ln match {
      case Empty =>
        Empty
      case Node(e, nx) =>
        if (idx == 0)
          nx
        else
          if (i < idx - 1)
            Node(e, loop(nx, i + 1))
          else
            nx match {
              case Empty => Node(e, Empty)
              case Node(_, nx) => Node(e, nx)
            }
    }
    loop(self, 0)
  }
}

@main def run: Unit =
  val newList = LLNode.Node(1, LLNode.Empty)
  newList.print
  val newListAdd = newList.insertNode(2, 1)
  newListAdd.print
