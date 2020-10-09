package com.yarenty.cwr.assembler

import com.yarenty.cwr.mars.Memory
import java.io.{BufferedReader, IOException, Reader, StreamTokenizer}

class Assembler(val reader: Reader, var maxLength: Int) {
  protected var in: BufferedReader = new BufferedReader(reader)
  protected var tok: StreamTokenizer = new StreamTokenizer(in)
  tok.lowerCaseMode(true)
  tok.ordinaryChar('/')
  tok.eolIsSignificant(true)
  tok.parseNumbers()
  tok.ordinaryChar('.')
  tok.ordinaryChar(',')

  protected var IP = 0


  protected var start = 0

  protected var war: Array[Memory] = new Array[Memory](maxLength)
  for (i <- 0 until maxLength) {
    war(i) = new Memory
  }
  // meta values
  protected var name: String = null
  protected var author: String = null


  def getWarrior: Array[Memory] = {
    val wMem = new Array[Memory](IP)
    for (i <- 0 until IP) {
      wMem(i) = war(i)
    }
    wMem
  }

  def getOffset: Int = start

  def getName: String = {
    if (name != null) return new String(name)
    ""
  }

  def getAuthor: String = {
    if (author != null) return new String(author)
    ""
  }

  def length: Int = IP

  def assemble: Boolean = {
    try { //begin //todo: labels are not supported
      while ( {
        tok.nextToken != StreamTokenizer.TT_EOF
      }) {
        if (tok.ttype == ';') pComment()
        else if (tok.ttype == StreamTokenizer.TT_WORD && tok.sval == "org") {
          if (tok.nextToken != StreamTokenizer.TT_NUMBER) return false
          start = tok.nval.toInt
          tok.nextToken
          if (tok.ttype == ';') pComment()
        }
        else if (tok.ttype == StreamTokenizer.TT_WORD) {
          if (tok.sval == "mov") war(IP).opcode = Memory.MOV
          else if (tok.sval == "add") war(IP).opcode = Memory.ADD
          else if (tok.sval == "sub") war(IP).opcode = Memory.SUB
          else if (tok.sval == "mul") war(IP).opcode = Memory.MUL
          else if (tok.sval == "div") war(IP).opcode = Memory.DIV
          else if (tok.sval == "mod") war(IP).opcode = Memory.MOD
          else if (tok.sval == "jmz") war(IP).opcode = Memory.JMZ
          else if (tok.sval == "jmn") war(IP).opcode = Memory.JMN
          else if (tok.sval == "djn") war(IP).opcode = Memory.DJN
          else if (tok.sval == "cmp") war(IP).opcode = Memory.CMP
          else if (tok.sval == "seq") war(IP).opcode = Memory.SEQ
          else if (tok.sval == "slt") war(IP).opcode = Memory.SLT
          else if (tok.sval == "spl") war(IP).opcode = Memory.SPL
          else if (tok.sval == "dat") war(IP).opcode = Memory.DAT
          else if (tok.sval == "jmp") war(IP).opcode = Memory.JMP
          else if (tok.sval == "sne") war(IP).opcode = Memory.SNE
          else if (tok.sval == "nop") war(IP).opcode = Memory.NOP
          else if (tok.sval == "ldp") war(IP).opcode = Memory.LDP
          else if (tok.sval == "stp") war(IP).opcode = Memory.STP
          else if (tok.sval == "end") {
            if (tok.nextToken == StreamTokenizer.TT_NUMBER) start = tok.nval.toInt
            return true
          }
          else return false
          if (!pModifier) return false
          if ( {
            IP += 1;
            IP
          } > maxLength) return false
          if (tok.ttype == ';') pComment()
        }
        if (tok.ttype != StreamTokenizer.TT_EOL) return false
      }
    }
    catch
    {
      case e: IOException =>
        System.out.println(e.toString)
        return false
    }
    true
  }

  def pComment(): Unit = { // this function is in place to get meta data
    try {
      if (tok.nextToken == StreamTokenizer.TT_WORD) if (tok.sval == "name") name = in.readLine
      else if (tok.sval == "author") author = in.readLine
      else in.readLine
      else in.readLine
      tok.ttype = StreamTokenizer.TT_EOL
    } catch {
      case e: IOException =>
        System.out.println(e.toString)
        return
    }
  }

  def pModifier: Boolean = try if (tok.nextToken != '.') pAOperand
  else if (tok.nextToken == StreamTokenizer.TT_WORD) {
    if (tok.sval == "a") war(IP).modifier = Memory.mA
    else if (tok.sval == "b") war(IP).modifier = Memory.mB
    else if (tok.sval == "ab") war(IP).modifier = Memory.mAB
    else if (tok.sval == "ba") war(IP).modifier = Memory.mBA
    else if (tok.sval == "f") war(IP).modifier = Memory.mF
    else if (tok.sval == "x") war(IP).modifier = Memory.mX
    else if (tok.sval == "i") war(IP).modifier = Memory.mI
    else return false
    tok.nextToken
    pAOperand
  }
  else false
  catch {
    case e: IOException =>
      System.out.println(e.toString)
      false
  }

  def pAOperand: Boolean = {
    tok.ttype match {
      case StreamTokenizer.TT_NUMBER =>
        return pAValue
      case '#' =>
        war(IP).aIndir = Memory.IMMEDIATE
        war(IP).aTiming = Memory.PRE
        war(IP).aAction = Memory.NONE
        war(IP).aTarget = Memory.B

      case '$' =>
        war(IP).aIndir = Memory.DIRECT
        war(IP).aTiming = Memory.POST
        war(IP).aAction = Memory.NONE
        war(IP).aTarget = Memory.B

      case '@' =>
        war(IP).aIndir = Memory.INDIRECT
        war(IP).aTiming = Memory.POST
        war(IP).aAction = Memory.NONE
        war(IP).aTarget = Memory.B

      case '<' =>
        war(IP).aIndir = Memory.INDIRECT
        war(IP).aTiming = Memory.PRE
        war(IP).aAction = Memory.DECREMENT
        war(IP).aTarget = Memory.B

      case '>' =>
        war(IP).aIndir = Memory.INDIRECT
        war(IP).aTiming = Memory.POST
        war(IP).aAction = Memory.INCREMENT
        war(IP).aTarget = Memory.B

      case '*' =>
        war(IP).aIndir = Memory.INDIRECT
        war(IP).aTiming = Memory.POST
        war(IP).aAction = Memory.NONE
        war(IP).aTarget = Memory.A

      case '{' =>
        war(IP).aIndir = Memory.INDIRECT
        war(IP).aTiming = Memory.PRE
        war(IP).aAction = Memory.DECREMENT
        war(IP).aTarget = Memory.A

      case '}' =>
        war(IP).aIndir = Memory.INDIRECT
        war(IP).aTiming = Memory.PRE
        war(IP).aAction = Memory.INCREMENT
        war(IP).aTarget = Memory.A

      case _ =>
        return false
    }
    try tok.nextToken
    catch {
      case e: IOException =>
        System.out.println(e.toString)
        return false
    }
    if (!pAValue) return false
    true
  }

  def pAValue: Boolean = {
    if (tok.ttype != StreamTokenizer.TT_NUMBER) return false
    war(IP).aValue = tok.nval.toInt
    try {
      if (tok.nextToken != ',') {
        System.out.println("no comma after aValue")
        return false
      }
      tok.nextToken
    } catch {
      case e: IOException =>
        System.out.println(e.toString)
        return false
    }
    pBOperand
  }

  def pBOperand: Boolean = {
    tok.ttype match {
      case StreamTokenizer.TT_NUMBER =>
        return pBValue
      case '#' =>
        war(IP).bIndir = Memory.IMMEDIATE
        war(IP).bTiming = Memory.PRE
        war(IP).bAction = Memory.NONE
        war(IP).bTarget = Memory.B

      case '$' =>
        war(IP).bIndir = Memory.DIRECT
        war(IP).bTiming = Memory.POST
        war(IP).bAction = Memory.NONE
        war(IP).bTarget = Memory.B

      case '@' =>
        war(IP).bIndir = Memory.INDIRECT
        war(IP).bTiming = Memory.POST
        war(IP).bAction = Memory.NONE
        war(IP).bTarget = Memory.B

      case '<' =>
        war(IP).bIndir = Memory.INDIRECT
        war(IP).bTiming = Memory.PRE
        war(IP).bAction = Memory.DECREMENT
        war(IP).bTarget = Memory.B

      case '>' =>
        war(IP).bIndir = Memory.INDIRECT
        war(IP).bTiming = Memory.POST
        war(IP).bAction = Memory.INCREMENT
        war(IP).bTarget = Memory.B

      case '*' =>
        war(IP).bIndir = Memory.INDIRECT
        war(IP).bTiming = Memory.POST
        war(IP).bAction = Memory.NONE
        war(IP).bTarget = Memory.A

      case '{' =>
        war(IP).bIndir = Memory.INDIRECT
        war(IP).bTiming = Memory.PRE
        war(IP).bAction = Memory.DECREMENT
        war(IP).bTarget = Memory.A

      case '}' =>
        war(IP).bIndir = Memory.INDIRECT
        war(IP).bTiming = Memory.POST
        war(IP).bAction = Memory.INCREMENT
        war(IP).bTarget = Memory.A

      case _ =>
        return false
    }
    try tok.nextToken
    catch {
      case e: IOException =>
        System.out.println(e.toString)
        return false
    }
    pBValue
  }

  def pBValue: Boolean = {
    if (tok.ttype != StreamTokenizer.TT_NUMBER) return false
    war(IP).bValue = tok.nval.toInt
    try tok.nextToken
    catch {
      case e: IOException =>
        System.out.println(e.toString)
        return false
    }
    true
  }
}

