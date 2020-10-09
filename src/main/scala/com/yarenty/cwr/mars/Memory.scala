package com.yarenty.cwr.mars


object Memory { // valid opcodes
  val MOV = 0
  val ADD = 1
  val SUB = 2
  val MUL = 3
  val DIV = 4
  val MOD = 5
  val JMZ = 6
  val JMN = 7
  val DJN = 8
  val CMP = 9 // two names for same instruction

  val SEQ = 9
  val SNE = 10
  val SLT = 11
  val SPL = 12
  val DAT = 13
  val JMP = 14
  val NOP = 15
  val LDP = 16
  val STP = 17
  // valid modifiers
  val mA = 0
  val mB = 1
  val mAB = 2
  val mBA = 3
  val mF = 4
  val mX = 5
  val mI = 6
  // valid Indirections
  val IMMEDIATE = 0
  val DIRECT = 1
  val INDIRECT = 2
  // valid Targets
  val A = 0
  val B = 1
  // valid Timings
  val PRE:Int = 0
  val POST = 1
  // valid Actions
  val NONE:Int  = 0
  val DECREMENT = 1
  val INCREMENT = 2
}

class Memory() {

  // fields of a memory cell
  var opcode:Int = Memory.DAT
  var modifier:Int = Memory.mF
  var aIndir:Int = Memory.DIRECT
  var bIndir:Int = Memory.DIRECT // Immediate, Direct or Indirect

  var aTarget:Int = Memory.B
  var bTarget:Int  = Memory.B // A or B indirection

  var aTiming: Int = Memory.PRE
  var bTiming: Int  = Memory.PRE // Pre or Post

  var aAction: Int = Memory.NONE
  var bAction: Int = Memory.NONE // decrement or increment

  var aValue:Int = 0
  var bValue:Int = 0




  def copy(src: Memory): Unit = {
    opcode = src.opcode
    modifier = src.modifier
    aIndir = src.aIndir
    bIndir = src.bIndir
    aTarget = src.aTarget
    bTarget = src.bTarget
    aTiming = src.aTiming
    bTiming = src.bTiming
    aAction = src.aAction
    bAction = src.bAction
    aValue = src.aValue
    bValue = src.bValue
  }

  def equals(comp: Memory): Boolean = {
    if ((opcode != comp.opcode) || (modifier != comp.modifier) || (aIndir != comp.aIndir) || (aAction != comp.aAction) || (aTarget != comp.aTarget) || (aTiming != comp.aTiming) || (aValue != comp.aValue) || (bIndir != comp.bIndir) || (bAction != comp.bAction) || (bTarget != comp.bTarget) || (bTiming != comp.bTiming) || (bValue != comp.bValue)) return false
    true
  }

  override def toString: String = {
    val str = new StringBuffer
    opcode match {
      case Memory.MOV =>
        str.append("MOV")

      case Memory.ADD =>
        str.append("ADD")

      case Memory.SUB =>
        str.append("SUB")

      case Memory.MUL =>
        str.append("MUL")

      case Memory.DIV =>
        str.append("DIV")

      case Memory.MOD =>
        str.append("MOD")

      case Memory.JMZ =>
        str.append("JMZ")

      case Memory.JMN =>
        str.append("JMN")

      case Memory.DJN =>
        str.append("DJN")

      case Memory.SEQ =>
        str.append("SEQ")

      case Memory.SNE =>
        str.append("SNE")

      case Memory.SLT =>
        str.append("SLT")

      case Memory.SPL =>
        str.append("SPL")

      case Memory.DAT =>
        str.append("DAT")

      case Memory.JMP =>
        str.append("JMP")

      case Memory.NOP =>
        str.append("NOP")

      case Memory.LDP =>
        str.append("LDP")

      case Memory.STP =>
        str.append("STP")

    }
    modifier match {
      case Memory.mA =>
        str.append(".A  ")

      case Memory.mB =>
        str.append(".B  ")

      case Memory.mAB =>
        str.append(".AB ")

      case Memory.mBA =>
        str.append(".BA ")

      case Memory.mF =>
        str.append(".F  ")

      case Memory.mX =>
        str.append(".X  ")

      case Memory.mI =>
        str.append(".I  ")

    }
    if ((aIndir == Memory.INDIRECT) && (aTiming == Memory.PRE) && (aAction == Memory.DECREMENT)) aTarget match {
      case Memory.A =>
        str.append("{ ")

      case Memory.B =>
        str.append("< ")
    }
    else if ((aIndir == Memory.INDIRECT) && (aTiming == Memory.POST) && (aAction == Memory.INCREMENT)) aTarget match {
      case Memory.A =>
        str.append("} ")

      case Memory.B =>
        str.append("> ")
    }
    else {
      aIndir match {
        case Memory.IMMEDIATE =>
          str.append("#")

        case Memory.DIRECT =>
          str.append("$")

        case Memory.INDIRECT =>
          if (aAction == Memory.NONE) aTarget match {
            case Memory.A =>
              str.append("*")

            case Memory.B =>
              str.append("@")

          }
          else str.append("@")

      }
      if (aAction != Memory.NONE) {
        aTiming match {
          case Memory.PRE =>
            str.append("<")

          case Memory.POST =>
            str.append(">")

        }
        aAction match {
          case Memory.DECREMENT =>
            str.append("-")

          case Memory.INCREMENT =>
            str.append("+")

        }
        aTarget match {
          case Memory.A =>
            str.append("A")

          case Memory.B =>
            str.append("B")

        }
      }
      else str.append(" ")
    }
    var i = 6 - Integer.toString(aValue).length

    while ( {
      i > 0
    }) {
      str.append(" ")

      i -= 1
    }
    str.append(" " + aValue + ", ")
    if ((bIndir == Memory.INDIRECT) && (bTiming == Memory.PRE) && (bAction == Memory.DECREMENT)) bTarget match {
      case Memory.A =>
        str.append("{ ")

      case Memory.B =>
        str.append("< ")
    }
    else if ((bIndir == Memory.INDIRECT) && (bTiming == Memory.POST) && (bAction == Memory.INCREMENT)) bTarget match {
      case Memory.A =>
        str.append("} ")

      case Memory.B =>
        str.append("> ")
    }
    else {
      bIndir match {
        case Memory.IMMEDIATE =>
          str.append("#")

        case Memory.DIRECT =>
          str.append("$")

        case Memory.INDIRECT =>
          if (aAction == Memory.NONE) aTarget match {
            case Memory.A =>
              str.append("*")

            case Memory.B =>
              str.append("@")

          }
          else str.append("@")

      }
      if (bAction != Memory.NONE) {
        bTiming match {
          case Memory.PRE =>
            str.append("<")

          case Memory.POST =>
            str.append(">")

        }
        bAction match {
          case Memory.DECREMENT =>
            str.append("-")

          case Memory.INCREMENT =>
            str.append("+")

        }
        bTarget match {
          case Memory.A =>
            str.append("A")

          case Memory.B =>
            str.append("B")

        }
      }
      else str.append(" ")
    }
    i = 6 - Integer.toString(bValue).length

    while ( {
      i > 0
    }) {
      str.append(" ")

      i -= 1
    }
    str.append(" " + bValue)
    str.toString
  }
};


