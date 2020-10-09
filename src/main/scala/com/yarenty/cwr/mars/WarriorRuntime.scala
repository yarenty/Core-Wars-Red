package com.yarenty.cwr.mars

class WarriorRuntime {
  var warrior: Warrior = null
  protected var pspace: Array[Int] = null
  //	protected Vector pQueue;
  protected var pQueue: Array[Int] = null
  protected var pQFirst = 0
  protected var pQLast = 0
  protected var numProc = 0 // Current number of processes

  protected var prev: WarriorRuntime = null // previous warrior in the execute queue

  protected var next: WarriorRuntime = null // next warrior in the execute queue

  def this(war: Warrior, FirstInst: Int, p: Array[Int]) = {
    this()
    warrior = war
    //		pQueue = new Vector();
    //		pQueue.addElement(new Integer(FirstInst));
    pQueue = new Array[Int](10000)
    pQueue(0) = FirstInst
    pQFirst = 0
    pQLast = 1
    numProc = 1
    pspace = p
    prev = this
    next = this
  }

  def Insert(prevWarr: WarriorRuntime, nextWarr: WarriorRuntime): Unit = {
    prev = prevWarr
    next = nextWarr
    prevWarr.next = this
    nextWarr.prev = this
  }

  def Remove(): Unit = {
    prev.next = next
    next.prev = prev
    prev = this
    next = this
  }


  def addProc(inst: Int): Unit = { //		pQueue.addElement(new Integer(inst));
    //		numProc++;
    val l = pQueue.length
    pQueue(pQLast) = inst
    pQLast = (pQLast + 1) % l
    if (pQLast == pQFirst) {
      val nQ = new Array[Int]((l * 1.3).toInt)
      for (i <- 0 until l) {
        nQ(i) = pQueue((pQFirst + i) % l)
      }
      pQueue = nQ
      pQFirst = 0
      pQLast = l
    }
    numProc += 1
  }

  def getProc: Int = {
    /*		int i = ((Integer) pQueue.firstElement()).intValue();
       pQueue.removeElementAt(0);
       numProc--;
   */ val i = pQueue(pQFirst)
    pQFirst += 1
    pQFirst %= pQueue.length
    numProc -= 1
    i
  }

  def getPrevWarrior: WarriorRuntime = prev

  def getNextWarrior: WarriorRuntime = next

  def NumProc: Int = numProc

  def setPCell(addr: Int, value: Int): Unit = {
    pspace(addr % pspace.length) = value
  }

  def getPCell(addr: Int): Int = pspace(addr % pspace.length)

  def getPSpace: Array[Int] = pspace
}

