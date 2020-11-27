package com.yarenty.cwr

import java.io.{FileNotFoundException, FileReader}

import com.yarenty.cwr.assembler.Assembler
import com.yarenty.cwr.mars.Memory

import org.junit.Assert._
import org.junit.{Before, Test}
import org.scalatest._



class CodeTester  extends Suite {



  @Before def initialize(): Unit = {
  }


  @Test
  def testImp(): Unit = {

      println(System.getProperty("user.dir"))

//      val file = new FileReader("src/main/resources/imp.red")
      val file = new FileReader("src/main/resources/ElectricHead.red")
      assert(file.ready(), "No file")
      val parser = new Assembler(file, 100)
      var warrior:Array[Memory] = null
      if (!parser.assemble) {
        System.out.println("error in warrior file, possibly near instruction " + parser.length)
        warrior = parser.getWarrior
        System.out.println("last instruction " + warrior(warrior.length - 1))
        return
      }
      System.out.println("name: " + parser.getName)
      System.out.println("author: " + parser.getAuthor + "\n")
      System.out.println("offset: ORG	" + parser.getOffset)
      warrior = parser.getWarrior
    println("BODY:")
      for (i <- 0 until warrior.length) {
        System.out.println(warrior(i))
      }

  }

}
