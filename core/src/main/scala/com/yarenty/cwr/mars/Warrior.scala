package com.yarenty.cwr.mars

import java.awt.Color
import java.io.Reader

import com.yarenty.cwr.assembler.Assembler

class Warrior(val file: Reader, val maxLength: Int, var myColor: Color, var dColor: Color) {

  var wInst: Array[Memory] = null
  var wOffset = 0
  protected var pSpace: Array[Int] = null
  var name: String = null
  var author: String = null
  var numProc = 0
  var Alive = false

  val parser = new Assembler(file, maxLength)
  if (parser.assemble) {
    wInst = parser.getWarrior
    wOffset = parser.getOffset
    name = parser.getName
    author = parser.getAuthor
    Alive = true
  }
  else {
    wInst = new Array[Memory](0)
    wOffset = 0
    Alive = false
  }


  def getMemory(coreSize: Int): Array[Memory] = {
    for (i <- 0 until wInst.length) {
      while ( {
        wInst(i).aValue < 0
      }) wInst(i).aValue += coreSize
      wInst(i).aValue %= coreSize
      while ( {
        wInst(i).bValue < 0
      }) wInst(i).bValue += coreSize
      wInst(i).bValue %= coreSize
    }
    wInst
  }

  def getOffset: Int = wOffset

  def initPSpace(length: Int): Unit = {
    pSpace = new Array[Int](length)
  }

  def getPSpace: Array[Int] = {
    val p = new Array[Int](pSpace.length)
    for (i <- 0 until pSpace.length) {
      p(i) = pSpace(i)
    }
    p
  }

  def getNormalizedPSpace(coreSize: Int): Array[Int] = {
    val p = new Array[Int](pSpace.length)
    for (i <- 0 until pSpace.length) {
      var j = pSpace(i)
      while ( {
        j < 0
      }) j += coreSize
      j %= coreSize
      p(i) = j
    }
    p
  }

  def setPSpace(p: Array[Int]): Unit = {
    pSpace = p
  }

  def getPCell(index: Int): Int = {
    if (pSpace == null || index < 0 || index >= pSpace.length) return 0
    pSpace(index)
  }

  def setPCell(index: Int, value: Int): Boolean = {
    if (index < 0 || index >= pSpace.length) return false
    pSpace(index) = value
    true
  }
}

