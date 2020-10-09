package com.yarenty.cwr.mars

class MarsVM(var coreSize: Int, var maxProc: Int, var pSize: Int) {
  var core: Array[Memory] = new Array[Memory](coreSize)
  for (i <- core.indices) {
    core(i) = new Memory
  }

  protected var IP  = 0 // current instruction pointer

  protected var currentW: WarriorRuntime = null // current warrior

  private var numWarriors  = 0

  def reset(): Unit = {
    currentW = null
    numWarriors = 0
    for (i <- core.indices) {
      core(i) = new Memory
    }
  }

  /* Load a warrior into the core.
     * start = first memory location occupied by warrior
     * length = number of cells warrior is long
     * offset = the offset into the warrior to be first executed
     * warrior = actual memory of warrior
     */
  def loadWarrior(warrior: Warrior, startPosition: Int): Boolean = {

    val wMemory  = warrior.getMemory(coreSize)
    if ((startPosition + wMemory.length - 1) > coreSize) return false// check that warrior fits in memory
    numWarriors += 1
    for (i <- wMemory.indices) {
      core(startPosition + i).copy(wMemory(i))
    }

    val newWarrior: WarriorRuntime = new WarriorRuntime(warrior, startPosition + warrior.getOffset, warrior.getNormalizedPSpace(coreSize))

    if (currentW == null) currentW = newWarrior
    else newWarrior.Insert(currentW, currentW.getNextWarrior)
    true
  }

  def getCurrentWarrior: Warrior = currentW.warrior

  protected def killProc(report: Report, addr: Int): Unit = { // delete the current process
    report.pDie(addr)
    report.numProc(currentW.NumProc)
    if (currentW.NumProc > 0) {
      currentW = currentW.getNextWarrior
      return
    }
    // else if that was the last process in that warrior kill it
    report.wDie()
    numWarriors -= 1
    currentW.setPCell(0, numWarriors)
    currentW.warrior.setPSpace(currentW.getPSpace)
    currentW = currentW.getNextWarrior
    currentW.getPrevWarrior.Remove()
    }

  def executeInstr: Report = {
    val instr = new Memory // copy of current instruction
    var tempAddr = 0 // temporary address for use in mode evaluation
    var addrA = 0 // A's address
    val instrA = new Memory
    var addrAAValue = 0 // address A's A Value
    var addrABValue = 0 // address B's B Value
    var addrB = 0 // address B
    val instrB = new Memory
    var addrBAValue = 0 // address B's A Value
    var addrBBValue = 0
    val report = new Report
    // get instruction pointer
    IP = currentW.getProc
    report.warrior(currentW.warrior)
    report.numProc(currentW.NumProc)
    // Get a Pointer to the current instruction
    instr.copy(core(IP))
    // evaluate A operand
    tempAddr = (instr.aValue + IP) % coreSize // temporary address stuffed with the direct value to evaluate actions and help with indirect mode

    // do Pre timed actions
    if ((instr.aTiming == Memory.PRE) && (instr.aAction != Memory.NONE))
      if (instr.aAction == Memory.DECREMENT) {
        if (instr.aTarget == Memory.A) if ( {
          core(tempAddr).aValue -= 1;
          core(tempAddr).aValue
        } < 0) core(tempAddr).aValue = coreSize - 1
        else if ( {
          core(tempAddr).bValue -= 1;
          core(tempAddr).bValue
        } < 0) core(tempAddr).bValue = coreSize - 1

        report.decrement(tempAddr)
      }
      else {
        if (instr.aTarget == Memory.A) core(tempAddr).aValue = {
          core(tempAddr).aValue += 1;
          core(tempAddr).aValue
        } % coreSize
        else core(tempAddr).bValue = {
          core(tempAddr).bValue += 1;
          core(tempAddr).bValue
        } % coreSize

        report.increment(tempAddr)
      }


    // evaluate indirection
    instr.aIndir match {
      case Memory.IMMEDIATE =>
        addrA = IP
        instrA.copy(core(IP))
        addrAAValue = instr.aValue
        addrABValue = instr.bValue

      case Memory.DIRECT =>
        addrA = (IP + instr.aValue) % coreSize
        instrA.copy(core((IP + instr.aValue) % coreSize))
        addrAAValue = core(addrA).aValue
        addrABValue = core(addrA).bValue

      case Memory.INDIRECT =>
        if (instr.aTarget == Memory.A) addrA = (core(tempAddr).aValue + tempAddr) % coreSize
        else addrA = (core(tempAddr).bValue + tempAddr) % coreSize
        instrA.copy(core(addrA))
        addrAAValue = core(addrA).aValue
        addrABValue = core(addrA).bValue
        report.increment(tempAddr)

    }
    // do Post actions
    if ((instr.aTiming == Memory.POST) && (instr.aAction != Memory.NONE)) if (instr.aAction == Memory.DECREMENT) {
        if (instr.aTarget == Memory.A) if ( {
            core(tempAddr).aValue -= 1; core(tempAddr).aValue
          } < 0) core(tempAddr).aValue = coreSize - 1
        else if ( {
            core(tempAddr).bValue -= 1; core(tempAddr).bValue
          } < 0) core(tempAddr).bValue = coreSize - 1
        report.decrement(tempAddr)
      }
      else {
        if (instr.aTarget == Memory.A) core(tempAddr).aValue = {
            core(tempAddr).aValue += 1; core(tempAddr).aValue
          } % coreSize
        else core(tempAddr).bValue = {
            core(tempAddr).bValue += 1; core(tempAddr).bValue
          } % coreSize
        report.increment(tempAddr)
      }
    // evaluate B operand
    tempAddr = (instr.bValue + IP) % coreSize
    if ((instr.bTiming == Memory.PRE) && (instr.bAction != Memory.NONE)) if (instr.bAction == Memory.DECREMENT) {
        if (instr.bTarget == Memory.A) if ( {
            core(tempAddr).aValue -= 1; core(tempAddr).aValue
          } < 0) core(tempAddr).aValue = coreSize - 1
        else if ( {
            core(tempAddr).bValue -= 1; core(tempAddr).bValue
          } < 0) core(tempAddr).bValue = coreSize - 1
        report.decrement(tempAddr)
      }
      else {
        if (instr.bTarget == Memory.A) core(tempAddr).aValue = {
            core(tempAddr).aValue += 1; core(tempAddr).aValue
          } % coreSize
        else core(tempAddr).bValue = {
            core(tempAddr).bValue += 1; core(tempAddr).bValue
          } % coreSize
        report.increment(tempAddr)
      }
    instr.bIndir match {
      case Memory.IMMEDIATE =>
        addrB = IP
        instrB.copy(core(IP))
        addrBAValue = instr.aValue
        addrBBValue = instr.bValue

      case Memory.DIRECT =>
        addrB = (IP + instr.bValue) % coreSize
        instrB.copy(core((IP + instr.bValue) % coreSize))
        addrBAValue = core(addrB).aValue
        addrBBValue = core(addrB).bValue

      case Memory.INDIRECT =>
        if (instr.bTarget == Memory.A) addrB = (core(tempAddr).aValue + tempAddr) % coreSize
        else addrB = (core(tempAddr).bValue + tempAddr) % coreSize
        instrB.copy(core(addrB))
        addrBAValue = core(addrB).aValue
        addrBBValue = core(addrB).bValue
        report.increment(tempAddr)

    }
    if ((instr.bTiming == Memory.POST) && (instr.bAction != Memory.NONE)) if (instr.bAction == Memory.DECREMENT) {
        if (instr.bTarget == Memory.A) if ( {
            core(tempAddr).aValue -= 1; core(tempAddr).aValue
          } < 0) core(tempAddr).aValue = coreSize - 1
        else if ( {
            core(tempAddr).bValue -= 1; core(tempAddr).bValue
          } < 0) core(tempAddr).bValue = coreSize - 1
        report.decrement(tempAddr)
      }
      else {
        if (instr.bTarget == Memory.A) core(tempAddr).aValue = {
            core(tempAddr).aValue += 1; core(tempAddr).aValue
          } % coreSize
        else core(tempAddr).bValue = {
            core(tempAddr).bValue += 1; core(tempAddr).bValue
          } % coreSize
        report.increment(tempAddr)
      }
    // execute instruction
    report.execute(IP)
    instr.opcode match {
      case Memory.DAT =>
        killProc(report, IP)
        return report
      case Memory.MOV =>
        instr.modifier match {
          case Memory.mI =>
            core(addrB).copy(instrA)

          case Memory.mA =>
            core(addrB).aValue = addrAAValue

          case Memory.mF =>
            core(addrB).aValue = addrAAValue
          // fallthrough for rest
          case Memory.mB =>
            core(addrB).bValue = addrABValue

          case Memory.mAB =>
            core(addrB).bValue = addrAAValue

          case Memory.mX =>
            core(addrB).bValue = addrAAValue
          case Memory.mBA =>
            core(addrB).aValue = addrABValue

        }
        report.read(addrA)
        report.write(addrB)

      case Memory.ADD =>
        instr.modifier match {
          case Memory.mA =>
            core(addrB).aValue = (addrAAValue + addrBAValue) % coreSize

          case Memory.mI =>
          case Memory.mF =>
            core(addrB).aValue = (addrAAValue + addrBAValue) % coreSize
          case Memory.mB =>
            core(addrB).bValue = (addrABValue + addrBBValue) % coreSize

          case Memory.mAB =>
            core(addrB).bValue = (addrAAValue + addrBBValue) % coreSize

          case Memory.mX =>
            core(addrB).bValue = (addrAAValue + addrBBValue) % coreSize
          case Memory.mBA =>
            core(addrB).aValue = (addrABValue + addrBAValue) % coreSize

        }
        report.read(addrA)
        report.write(addrB)

      case Memory.SUB =>
        instr.modifier match {
          case Memory.mA =>
            core(addrB).aValue = addrBAValue - addrAAValue
            if (core(addrB).aValue < 0) core(addrB).aValue += coreSize

          case Memory.mI =>
          case Memory.mF =>
            core(addrB).aValue = addrBAValue - addrAAValue
            if (core(addrB).aValue  < 0) core(addrB).aValue += coreSize
          case Memory.mB =>
            core(addrB).bValue = addrBBValue - addrABValue
            if (core(addrB).bValue < 0) core(addrB).bValue += coreSize

          case Memory.mAB =>
            core(addrB).bValue = addrBBValue - addrAAValue
            if (core(addrB).bValue < 0) core(addrB).bValue += coreSize

          case Memory.mX =>
            core(addrB).bValue = addrBBValue - addrAAValue
            if (core(addrB).bValue  < 0) core(addrB).aValue += coreSize
          // falthrough for rest
          case Memory.mBA =>
            core(addrB).aValue = addrBAValue - addrABValue
            if (core(addrB).aValue  < 0) core(addrB).aValue += coreSize

        }
        report.read(addrA)
        report.write(addrB)

      case Memory.MUL =>
        instr.modifier match { // the cast prevents overflow, i hope ;)
          case Memory.mA =>
            core(addrB).aValue = (addrBAValue.toLong * addrAAValue % coreSize).toInt

          case Memory.mI =>
          case Memory.mF =>
            core(addrB).aValue = (addrBAValue.toLong * addrAAValue % coreSize).toInt
          case Memory.mB =>
            core(addrB).bValue = (addrBBValue.toLong * addrABValue % coreSize).toInt

          case Memory.mAB =>
            core(addrB).bValue = (addrBBValue.toLong * addrAAValue % coreSize).toInt

          case Memory.mX =>
            core(addrB).bValue = (addrBBValue.toLong * addrAAValue % coreSize).toInt
          case Memory.mBA =>
            core(addrB).aValue = (addrBAValue.toLong * addrABValue % coreSize).toInt

        }
        report.read(addrA)
        report.write(addrB)

      case Memory.DIV =>
        report.read(addrA)
        instr.modifier match {
          case Memory.mA =>
            if (addrAAValue == 0) {
              killProc(report, IP)
              return report
            }
            core(addrB).aValue = addrBAValue / addrAAValue

          case Memory.mB =>
            if (addrABValue == 0) {
              killProc(report, IP)
              return report
            }
            core(addrB).bValue = addrBBValue / addrABValue

          case Memory.mAB =>
            if (addrAAValue == 0) {
              killProc(report, IP)
              return report
            }
            core(addrB).bValue = addrBBValue / addrAAValue

          case Memory.mBA =>
            if (addrABValue == 0) {
              killProc(report, IP)
              return report
            }
            core(addrB).aValue = addrBAValue / addrABValue

          case Memory.mI =>
          case Memory.mF =>
            if (addrAAValue != 0) {
              core(addrB).aValue = addrBAValue / addrAAValue
              if (addrABValue == 0) {
                killProc(report, IP)
                return report
              }
              core(addrB).bValue = addrBBValue / addrABValue

            }
            else {
              if (addrABValue == 0) {
                killProc(report, IP)
                return report
              }
              core(addrB).bValue = addrBBValue / addrABValue
              report.write(addrB)
              killProc(report, IP)
              return report
            }
          case Memory.mX =>
            if (addrABValue != 0) {
              core(addrB).aValue = addrBAValue / addrABValue
              if (addrAAValue == 0) {
                killProc(report, IP)
                return report
              }
              core(addrB).bValue = addrBBValue / addrAAValue

            }
            else {
              if (addrAAValue == 0) {
                killProc(report, IP)
                return report
              }
              core(addrB).bValue = addrBBValue / addrAAValue
              report.write(addrB)
              killProc(report, IP)
              return report
            }
        }
        report.write(addrB)

      case Memory.MOD =>
        report.read(addrA)
        instr.modifier match {
          case Memory.mA =>
            if (addrAAValue == 0) {
              killProc(report, IP)
              return report
            }
            core(addrB).aValue = addrBAValue % addrAAValue

          case Memory.mB =>
            if (addrABValue == 0) {
              killProc(report, IP)
              return report
            }
            core(addrB).bValue = addrBBValue % addrABValue

          case Memory.mAB =>
            if (addrAAValue == 0) {
              killProc(report, IP)
              return report
            }
            core(addrB).bValue = addrBBValue % addrAAValue

          case Memory.mBA =>
            if (addrABValue == 0) {
              killProc(report, IP)
              return report
            }
            core(addrB).aValue = addrBAValue % addrABValue

          case Memory.mI =>
          case Memory.mF =>
            if (addrAAValue != 0) {
              core(addrB).aValue = addrBAValue % addrAAValue
              if (addrABValue == 0) {
                killProc(report, IP)
                return report
              }
              core(addrB).bValue = addrBBValue % addrABValue

            }
            else {
              if (addrABValue == 0) {
                killProc(report, IP)
                return report
              }
              core(addrB).bValue = addrBBValue % addrABValue
              report.write(addrB)
              killProc(report, IP)
              return report
            }
          case Memory.mX =>
            if (addrABValue != 0) {
              core(addrB).aValue = addrBAValue % addrABValue
              if (addrAAValue == 0) {
                killProc(report, IP)
                return report
              }
              core(addrB).bValue = addrBBValue % addrAAValue

            }
            else {
              if (addrAAValue == 0) {
                killProc(report, IP)
                return report
              }
              core(addrB).bValue = addrBBValue % addrAAValue
              report.write(addrB)
              killProc(report, IP)
              return report
            }
        }
        report.write(addrB)

      case Memory.JMZ =>
        report.read(addrB)
        instr.modifier match {
          case Memory.mA =>
          case Memory.mBA =>
            if (addrBAValue != 0) return null//todo: return is not supported
            currentW.addProc(addrA)
            currentW = currentW.getNextWarrior
            return report
          case Memory.mF =>
          case Memory.mX =>
          case Memory.mI =>
            if (addrBAValue != 0) return null//todo: return is not supported
          // fallthrough
          case Memory.mB =>
          case Memory.mAB =>
            if (addrBBValue != 0) return null//todo: return is not supported
            currentW.addProc(addrA)
            currentW = currentW.getNextWarrior
            return report
        }

      case Memory.JMN =>
        report.read(addrB)
        instr.modifier match {
          case Memory.mA =>
          case Memory.mBA =>
            if (addrBAValue == 0) return null//todo: return is not supported
            currentW.addProc(addrA)
            currentW = currentW.getNextWarrior
            return report
          case Memory.mF =>
          case Memory.mX =>
          case Memory.mI =>
            if ((addrBAValue == 0) && (addrBBValue == 0)) return null//todo: return is not supported
            currentW.addProc(addrA)

          case Memory.mB =>
          case Memory.mAB =>
            if (addrBBValue == 0) return null//todo: return is not supported
            currentW.addProc(addrA)
            currentW = currentW.getNextWarrior
            return report
        }

      case Memory.DJN =>
        report.decrement(addrB)
        instr.modifier match {
          case Memory.mA =>
          case Memory.mBA =>
            if ( {
              core(addrB).aValue -= 1; core(addrB).aValue
            } < 0) core(addrB).aValue = coreSize - 1
            if (addrBAValue == 1) return null//todo: return is not supported
            currentW.addProc(addrA)
            currentW = currentW.getNextWarrior
            return report
          case Memory.mB =>
          case Memory.mAB =>
            if ( {
              core(addrB).bValue -= 1; core(addrB).bValue
            } < 0) core(addrB).bValue = coreSize - 1
            if (addrBBValue == 1) return null//todo: return is not supported
            currentW.addProc(addrA)
            currentW = currentW.getNextWarrior
            return report
          case Memory.mF =>
          case Memory.mI =>
          case Memory.mX =>
            if ( {
              core(addrB).bValue -= 1; core(addrB).bValue
            } < 0) core(addrB).bValue = coreSize - 1
            if ( {
              core(addrB).aValue -= 1; core(addrB).aValue
            } < 0) core(addrB).aValue = coreSize - 1
            if ((addrBAValue == 1) && (addrBBValue == 1)) return null //todo: return is not supported
            currentW.addProc(addrA)
            currentW = currentW.getNextWarrior
            return report
        }

      case Memory.SEQ =>
        report.read(addrA)
        report.read(addrB)
        instr.modifier match {
          case Memory.mA =>
            if (addrBAValue != addrAAValue) return null //todo: return is not supported
            currentW.addProc((IP + 2) % coreSize)
            currentW = currentW.getNextWarrior
            return report
          case Memory.mI =>
            if (! core(addrB).equals(core(addrA)) ) return null //todo: return is not supported
          case Memory.mF =>
            if (addrBAValue != addrAAValue) return null //todo: return is not supported
          case Memory.mB =>
            if (addrBBValue != addrABValue) return null //todo: return is not supported
            currentW.addProc((IP + 2) % coreSize)
            currentW = currentW.getNextWarrior
            return report
          case Memory.mAB =>
            if (addrBBValue != addrAAValue) return null //todo: return is not supported
            currentW.addProc((IP + 2) % coreSize)
            currentW = currentW.getNextWarrior
            return report
          case Memory.mX =>
            if (addrBBValue != addrAAValue) return null //todo: return is not supported
          case Memory.mBA =>
            if (addrBAValue != addrABValue) return null //todo: return is not supported
            currentW.addProc((IP + 2) % coreSize)
            currentW = currentW.getNextWarrior
            return report
        }

      case Memory.SNE =>
        report.read(addrA)
        report.read(addrB)
        instr.modifier match {
          case Memory.mA =>
            if (addrBAValue == addrAAValue) return null //todo: return is not supported
            currentW.addProc((IP + 2) % coreSize)
            currentW = currentW.getNextWarrior
            return report
          case Memory.mI =>
            if (core(addrB).equals(core(addrA))) return null //todo: return is not supported
            currentW.addProc((IP + 2) % coreSize)
            currentW = currentW.getNextWarrior
            return report
          case Memory.mF =>
            if ((addrBAValue == addrAAValue) && (addrBBValue == addrABValue)) return null //todo: return is not supported
            currentW.addProc((IP + 2) % coreSize)
            currentW = currentW.getNextWarrior
            return report
          case Memory.mB =>
            if (addrBBValue == addrABValue) return null //todo: return is not supported
            currentW.addProc((IP + 2) % coreSize)
            currentW = currentW.getNextWarrior
            return report
          case Memory.mAB =>
            if (addrBBValue == addrAAValue) return null //todo: return is not supported
            currentW.addProc((IP + 2) % coreSize)
            currentW = currentW.getNextWarrior
            return report
          case Memory.mX =>
            if ((addrBBValue == addrAAValue) && (addrBAValue == addrABValue)) return null //todo: return is not supported
            currentW.addProc((IP + 2) % coreSize)
            currentW = currentW.getNextWarrior
            return report
          case Memory.mBA =>
            if (addrBAValue == addrABValue) return null //todo: return is not supported
            currentW.addProc((IP + 2) % coreSize)
            currentW = currentW.getNextWarrior
            return report
        }

      case Memory.SLT =>
        report.read(addrA)
        report.read(addrB)
        instr.modifier match {
          case Memory.mA =>
            if (addrBAValue <= addrAAValue) return null //todo: return is not supported
            currentW.addProc((IP + 2) % coreSize)
            currentW = currentW.getNextWarrior
            return report
          case Memory.mF =>
          case Memory.mI =>
            if (addrBAValue <= addrAAValue) return null //todo: return is not supported
          case Memory.mB =>
            if (addrBBValue <= addrABValue) return null //todo: return is not supported
            currentW.addProc((IP + 2) % coreSize)
            currentW = currentW.getNextWarrior
            return report
          case Memory.mAB =>
            if (addrBBValue <= addrAAValue) return null //todo: return is not supported
            currentW.addProc((IP + 2) % coreSize)
            currentW = currentW.getNextWarrior
            return report
          case Memory.mX =>
            if (addrBBValue <= addrAAValue) return null //todo: return is not supported
          case Memory.mBA =>
            if (addrBAValue <= addrABValue) return null //todo: return is not supported
            currentW.addProc((IP + 2) % coreSize)
            currentW = currentW.getNextWarrior
            return report
        }

      case Memory.JMP =>
        currentW.addProc(addrA)
        currentW = currentW.getNextWarrior
        return report
      case Memory.SPL =>
        currentW.addProc((IP + 1) % coreSize)
        if (currentW.NumProc >= maxProc) {
          currentW = currentW.getNextWarrior
          return report
        }
        currentW.addProc(addrA)
        report.numProc(currentW.NumProc)
        currentW = currentW.getNextWarrior
        return report
      case Memory.NOP =>

      case Memory.LDP =>
        report.read(addrA)
        instr.modifier match {
          case Memory.mA =>
            core(addrB).aValue = currentW.getPCell(addrAAValue)

          case Memory.mF =>
          case Memory.mX =>
          case Memory.mI =>
          case Memory.mB =>
            core(addrB).bValue = currentW.getPCell(addrABValue)

          case Memory.mAB =>
            core(addrB).bValue = currentW.getPCell(addrAAValue)

          case Memory.mBA =>
            core(addrB).aValue = currentW.getPCell(addrABValue)

        }
        report.write(addrB)

      case Memory.STP =>
        report.read(addrA)
        instr.modifier match {
          case Memory.mA =>
            currentW.setPCell(addrBAValue, addrAAValue)

          case Memory.mF =>
          case Memory.mX =>
          case Memory.mI =>
          case Memory.mB =>
            currentW.setPCell(addrBBValue, addrABValue)

          case Memory.mAB =>
            currentW.setPCell(addrBBValue, addrAAValue)

          case Memory.mBA =>
            currentW.setPCell(addrBAValue, addrABValue)

        }

      case _ =>
        return report
    }
    // point the IP to the next instruction
    currentW.addProc((IP + 1) % coreSize)
    currentW = currentW.getNextWarrior
    report
  }
}
