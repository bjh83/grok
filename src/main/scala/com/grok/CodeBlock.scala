package com.grok

import java.util
import java.util.{AbstractSequentialList, ListIterator}

import com.grok.CodeBlock.CodeBlockNode

import scala.collection.mutable

/**
 * Created by brendan.
 */
class CodeBlock extends AbstractSequentialList[Instruction] {
  private class CodeBlockIterator(var currentNode: CodeBlockNode) extends ListIterator[Instruction] {
    var currentIndex = -1

    override def hasNext: Boolean = currentNode.next != tailNode

    override def next(): Instruction = {
      currentNode = currentNode.next
      currentIndex += 1
      currentNode.instruction
    }

    override def set(e: Instruction): Unit = {
      currentNode.instruction = e
    }

    override def nextIndex(): Int = currentIndex + 1

    override def remove(): Unit = {
      val prev = currentNode.prev
      val next = currentNode.next
      prev.next = next
      next.prev = prev
    }

    override def previousIndex(): Int = currentIndex

    override def hasPrevious: Boolean = currentNode.prev != null

    override def add(e: Instruction): Unit = {
      val newNode = CodeBlockNode(currentNode, currentNode.next, e)
      currentNode.next.prev = newNode
      currentNode.next = newNode
    }

    override def previous(): Instruction = {
      val prev = currentNode.instruction
      currentNode = currentNode.prev
      currentIndex -= 1
      prev
    }
  }

  case class LateBindingLabel(node: CodeBlockNode)(modifier: CodeBlockNode => CodeBlockNode) extends Label {
    def toRealizedLabel = RealizedLabel(modifier(node).index)
  }

  private val headNode: CodeBlockNode = CodeBlockNode(null, null, null)
  private val tailNode: CodeBlockNode = CodeBlockNode(null, null, null)

  {
    headNode.next = tailNode
    tailNode.prev = headNode
  }

  def blockStart: Label = LateBindingLabel(headNode.next)(node => node)

  def blockEnd: Label = LateBindingLabel(tailNode.prev)(node => node.next)

  override def listIterator(index: Int): ListIterator[Instruction] = {
    val iter = new CodeBlockIterator(headNode)
    for (i <- 0 until index) {
      iter.next()
    }
    iter
  }

  override def size(): Int = {
    val iter = listIterator(0)
    while (iter.hasNext) {
      iter.next()
    }
    iter.previousIndex() + 1
  }

  // OpenJDK does not follow the Java specification on this >:-(
  override def addAll(index: Int, collection: util.Collection[_ <: Instruction]): Boolean = {
    val iterThis = listIterator(index)
    val iterThat = collection.iterator()
    var modified = false
    while (iterThat.hasNext) {
      iterThis.add(iterThat.next())
      iterThis.next()
      modified = true
    }
    modified
  }

  def prepend(instruction: Instruction): CodeBlock = {
    add(0, instruction)
    this
  }

  def prepend(block: CodeBlock): CodeBlock = {
    block.append(this)
  }

  def append(instruction: Instruction): CodeBlock = {
    add(size(), instruction)
    this
  }

  def append(block: CodeBlock): CodeBlock = {
    val thisLast = tailNode.prev
    val thatFirst = block.headNode.next
    thisLast.next = thatFirst
    thatFirst.prev = thisLast

    val thatLast = block.tailNode.prev
    tailNode.prev = thatLast
    thatLast.next = tailNode
    this
  }

  override def toString: String = {
    val labelSet = mutable.Set[Int]()
    val list = toList
    list.foreach {
      case goto: GotoInterface => labelSet.add(goto.label.toRealizedLabel.index)
      case _ =>
    }
    list.zipWithIndex.map {
      case (instr, index) => if (labelSet.contains(index)) {
        "L%03d: ".format(index) + instr
      } else {
        "      " + instr
      }
    }.foldLeft("")((left, right) => left + "\n" + right)
  }

  def toList: scala.List[Instruction] = {
    val list = mutable.MutableList[Instruction]()
    val iter = listIterator()
    while (iter.hasNext) {
      list += iter.next()
    }

    list.map {
      case goto: GotoInterface => goto.realizeLabel
      case nonGoto => nonGoto
    }.toList
  }
}

object CodeBlock {
  class CodeBlockNode(var prev: CodeBlockNode, var next: CodeBlockNode, var instruction: Instruction) {
    def index: Int = {
      if (prev == null) {
        // We are the headNode.
        -1
      } else {
        prev.index + 1
      }
    }
  }

  object CodeBlockNode {
    def apply(prev: CodeBlockNode, next: CodeBlockNode, instruction: Instruction): CodeBlockNode =
      new CodeBlockNode(prev, next, instruction)
  }

  def apply(): CodeBlock = new CodeBlock

  // DAMN YOU TYPE ERASURE!!!!
  def apply(instruction: Instruction): CodeBlock = {
    apply.append(instruction)
  }

  def apply(instruction: Instruction, instructions: Instruction*): CodeBlock = {
    val block = apply()
    (instruction +: instructions).foreach(block.append)
    block
  }

  def apply(block: CodeBlock): CodeBlock = block

  def apply(block: CodeBlock, blocks: CodeBlock*): CodeBlock = {
    blocks.foreach(block.append)
    block
  }
}