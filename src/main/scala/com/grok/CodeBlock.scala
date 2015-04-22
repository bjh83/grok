package com.grok

import java.util.ListIterator
import java.util.AbstractSequentialList

import scala.collection.JavaConverters._

/**
 * Created by brendan.
 */
class CodeBlock extends AbstractSequentialList[Instruction] {
  private class CodeBlockNode(var prev: CodeBlockNode, var next: CodeBlockNode, var instruction: Instruction)

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

  case class CodeBlockLabel(node: => CodeBlockNode) extends Label

  private val headNode: CodeBlockNode = CodeBlockNode(null, null, null)
  private val tailNode: CodeBlockNode = CodeBlockNode(null, null, null)

  {
    headNode.next = tailNode
    tailNode.prev = headNode
  }

  def blockStart: Label = {
    val next = headNode.next
    CodeBlockLabel(next)
  }
  def blockEnd: Label = {
    val prev = tailNode.prev
    CodeBlockLabel(prev.next)
  }

  override def listIterator: ListIterator[Instruction] = new CodeBlockIterator(headNode)

  override def size(): Int = {
    val iter = listIterator
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

  override def toString(): String = {
    this.asScala.map(_.toString).reduce((left, right) => left + "\n" + right)
  }
}

object CodeBlock {
  def apply(): CodeBlock = new CodeBlock

  def apply(instructions: Instruction*): CodeBlock = {
    val block = apply()
    instructions.foreach(block.append)
    block
  }

  def apply(blocks: CodeBlock*): CodeBlock = {
    val block = apply()
    blocks.foreach(block.append)
    block
  }
}