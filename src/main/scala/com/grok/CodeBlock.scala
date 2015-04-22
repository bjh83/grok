package com.grok

import java.util.ListIterator
import java.util.AbstractSequentialList

import scala.collection.JavaConverters._

/**
 * Created by brendan.
 */
class CodeBlock extends AbstractSequentialList[Instruction] {
  class CodeBlockNode(var prev: CodeBlockNode, var next: CodeBlockNode, var instruction: Instruction)

  private object CodeBlockNode {
    def apply(prev: CodeBlockNode, next: CodeBlockNode, instruction: Instruction): CodeBlockNode =
      new CodeBlockNode(prev, next, instruction)
  }

  private class CodeBlockIterator(var currentNode: CodeBlockNode) extends ListIterator[Instruction] {
    var currentIndex = -1

    override def hasNext: Boolean = currentNode.next != null

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

    override def previousIndex(): Int = currentIndex - 1

    override def hasPrevious: Boolean = currentNode.prev != null

    override def add(e: Instruction): Unit = {
      val newNode = CodeBlockNode(currentNode, currentNode.next, e)
      currentNode.next.prev = newNode
      currentNode.next = newNode
    }

    override def previous(): Instruction = {
      currentNode = currentNode.prev
      currentIndex -= 1
      currentNode.instruction
    }
  }

  case class LateBindingLabel(node: CodeBlockNode)(modifier: CodeBlockNode => CodeBlockNode) extends Label

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

  def prepend(instruction: Instruction): CodeBlock = {
    add(0, instruction)
    this
  }

  def prepend(block: CodeBlock): CodeBlock = {
    addAll(0, block)
    this
  }

  def append(instruction: Instruction): CodeBlock = {
    add(size(), instruction)
    this
  }

  def append(block: CodeBlock): CodeBlock = {
    addAll(size(), block)
    this
  }

  override def toString: String = {
    this.asScala.map(_.toString).reduce((left, right) => left + "\n" + right)
  }
}

object CodeBlock {
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