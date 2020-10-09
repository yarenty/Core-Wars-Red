package com.yarenty.cwr.mars

class Report() {
  protected var readAddr = new Array[Int](4)
  protected var numRead = 0
  protected var indirReadAddr = new Array[Int](4)
  protected var numIndirRead = 0
  protected var writeAddr = new Array[Int](4)
  protected var numWrite = 0
  protected var decAddr = new Array[Int](5)
  protected var numDec = 0
  protected var incAddr = new Array[Int](5)
  protected var numInc = 0
  protected var execAddr: Int = -1
  protected var pDieAddr: Int  = -1

  protected var warrior: Warrior = null
  protected var numProc = 0
  protected var wDeath = false

  // Warrior ID
  def warrior(warr: Warrior): Unit = {
    warrior = warr
  }

  // Read location
  def read(addr: Int): Unit = {
    readAddr(numRead) = addr
    numRead += 1
  }

  // read from indirection
  def indirRead(addr: Int): Unit = {
    indirReadAddr(numIndirRead) = addr
    numIndirRead += 1
  }

  // Write location
  def write(addr: Int): Unit = {
    writeAddr(numWrite) = addr
    numWrite += 1
  }

  // Decrement location
  def decrement(addr: Int): Unit = {
    decAddr(numDec) = addr
    numDec += 1
  }

  // Increment location
  def increment(addr: Int): Unit = {
    incAddr(numInc) = addr
    numInc += 1
  }

  // Execute location
  def execute(addr: Int): Unit = {
    execAddr = addr
  }

  //set the number of processes
  def numProc(numP: Int): Unit = {
    numProc = numP
  }

  // Process die
  def pDie(addr: Int): Unit = {
    pDieAddr = addr
  }

  // Warrior die
  def wDie(): Unit = {
    wDeath = true
  }


  // Get addresses read
  def addrRead: Array[Int] = {
    val value = new Array[Int](numRead)
    for (i <- 0 until numRead) {
      value(i) = readAddr(i)
    }
    value
  }

  // Get addresses read through indirection
  def addrIndirRead: Array[Int] = {
    val value = new Array[Int](numIndirRead)
    for (i <- 0 until numIndirRead) {
      value(i) = indirReadAddr(i)
    }
    value
  }

  // Get addresses written to
  def addrWrite: Array[Int] = {
    val value = new Array[Int](numWrite)
    for (i <- 0 until numWrite) {
      value(i) = writeAddr(i)
    }
    value
  }

  // Get addresses decremented
  def addrDec: Array[Int] = {
    val value = new Array[Int](numDec)
    for (i <- 0 until numDec) {
      value(i) = decAddr(i)
    }
    value
  }

  // Get addresses incremented
  def addrInc: Array[Int] = {
    val value = new Array[Int](numInc)
    for (i <- 0 until numInc) {
      value(i) = incAddr(i)
    }
    value
  }

  // Get addresses executed
  def addrExec: Int = execAddr

  // Get addresses of process death
  def addrPDeath: Int = pDieAddr

}
