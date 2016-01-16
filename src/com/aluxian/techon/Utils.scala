package com.aluxian.techon

import java.io.{File, FilenameFilter, PrintWriter}

import com.aluxian.techon.objects.Bug

import scala.io.Source

object Utils {

  def readGame(id: String): Game = {
    val inputFile = new File("sets").listFiles(new FilenameFilter {
      override def accept(dir: File, name: String): Boolean = name.take(2).equals(id)
    }).head

    val blocks = Source.fromFile(inputFile).mkString.split("\n\n")
    val settings = blocks(0).lines.toSeq.map(_.split("=")(1).toInt)
    val map = blocks(2).lines.toArray.map(_.split(" ").map(_.toCharArray.head))

    val bugs = blocks(1).lines.toSeq.map { line =>
      val props = line.split(" ")
      val colours = props.take(props.length - 1).tail.map(_.split("=")).map { parts => (parts(0), parts(1).toInt)}
      new Bug(props.head, colours.toMap, props.last.split("=")(1).toInt)
    }

    new Game(settings(0), settings(1), settings(2), settings(3), settings(4), bugs, map)
  }

  def writeSolution(solution: String, id: String): Unit = {
    val setName = new File("sets").listFiles(new FilenameFilter {
      override def accept(dir: File, name: String): Boolean = name.take(2).equals(id)
    }).head.getName

    val outputFile = new File("solutions/" + setName.dropRight(4) + ".solution.txt")
    outputFile.createNewFile()

    val writer = new PrintWriter(outputFile)
    writer.write(solution)
    writer.close()
  }

}
