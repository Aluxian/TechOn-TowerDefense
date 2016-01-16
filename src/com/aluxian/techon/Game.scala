package com.aluxian.techon

import java.util.concurrent.atomic.AtomicInteger

import com.aluxian.techon.Game._
import com.aluxian.techon.actions.{Action, NewTowerAction, ShootAction}
import com.aluxian.techon.objects.{Bug, Tower}

import scala.collection.mutable.ListBuffer

case class Game(startLife: Int, initialMoney: Int, towerRange: Int, towerCost: Int, reward: Int, bugs: Seq[Bug])(implicit grid: Grid) {

  val towers = ListBuffer[Tower]()
  val actions = ListBuffer[Action]()

  val towerIdGen = new AtomicInteger()

  var config: Config = _

  var life = startLife
  var money = initialMoney

  var frame = 0
  var gameEnded = false

  def play(config: Config): Unit = {
    this.config = config

    while (!gameEnded) {
      buildTowers()
      moveBugs()
      spawnBugs()
      shootBugs()
      findDeadBugs()
      checkGameEnd()

      println("frame: " + frame)
      println(grid.stringify)
      println()
      bugs.foreach(println)
      println()

      frame += 1
    }
  }

  def solution(): String = actions.mkString("\n")

  // FRAME-CYCLE

  def shouldBuildTower(): Boolean = {
    if (towers.length == 0)
      return true

    /*val shots = towers.map(numberOfPossibleShots).reduce({ (a, b) => a + b})
    if (!canKillBugsInShots(towers, shots)) {
      return true
    }*/

    false
  }

  def bestTowerPosition(): (Int, Int) = {
    val influenceMap: InfluenceMap = Array[Array[Double]]()

    for (line <- 0 to influenceMap.size._1 - 1) {
      for (col <- 0 to influenceMap.size._2 - 1) {
        if (grid(line)(col) == 1)
          influenceMap(line)(col) -= 100

        // Number of path cells in range
        influenceMap(line)(col) += pathCellsInRange(line, col, towerRange) * cellsInRangeWeight
      }
    }

    posOfGreatestValue(influenceMap)
  }

  def estimateColors(): Map[String, Int] = {

  }

  def buildTowers(): Unit = {

    /*if (frame == 0) {
      buildTower(Map("red" -> 9), 2, 2)
      buildTower(Map("red" -> 7, "blue" -> 13), 2, 3)
    }*/
  }

  def moveBugs(): Unit = {
    bugs.foreach { bug =>
      if (frame > bug.frame && !bug.isDead) {
        val nextCell = getNextCell(bug.line, bug.col)
        bug.line = nextCell._1
        bug.col = nextCell._2

        if (nextCell == grid.endPos) {
          life -= bug.totalHealth
          bug.isDead = true
        }
      }
    }
  }

  def spawnBugs(): Unit = {
    bugs.foreach { bug =>
      if (frame == bug.frame) {
        bug.line = grid.spawnPos._1
        bug.col = grid.spawnPos._2
      }
    }
  }

  def shootBugs(): Unit = {
    frame match {
      case 0 =>
        shoot(bugs(0), towers(0))
      case 1 =>
        shoot(bugs(0), towers(1))
        shoot(bugs(0), towers(0))
      case 2 =>
        shoot(bugs(0), towers(0))
        shoot(bugs(0), towers(1))
      case 3 =>
        shoot(bugs(1), towers(0))
        shoot(bugs(0), towers(1))
      case 4 =>
        shoot(bugs(1), towers(0))
        shoot(bugs(1), towers(1))
      case 5 =>
        shoot(bugs(1), towers(1))
      case 6 =>
        shoot(bugs(0), towers(0))
      case 7 =>
        shoot(bugs(2), towers(1))
      case 8 =>
        shoot(bugs(2), towers(1))
      case 9 =>
        shoot(bugs(2), towers(1))
      case 10 =>
        shoot(bugs(2), towers(1))
      case 11 =>
        shoot(bugs(2), towers(1))
    }
  }

  def findDeadBugs(): Unit = {
    bugs.foreach { bug =>
      if (!bug.isDead && frame >= bug.frame && bug.totalHealth <= 0) {
        bug.isDead = true
        money += reward
        life += bug.totalHealth
      }
    }
  }

  def checkGameEnd(): Unit = {
    if (life <= 0 || !bugs.exists(!_.isDead)) {
      gameEnded = true
    }
  }

  // HELPERS

  def buildTower(colors: Map[String, Int], line: Int, col: Int): Unit = {
    towers += new Tower(towerIdGen.getAndIncrement, colors, line, col, towerRange)
    actions += new NewTowerAction(frame, towers.last)
    money -= towerCost
    grid(line)(col) = 'T'
  }

  def shoot(bug: Bug, tower: Tower): Unit = {
    bug.colors = bug.colors.map({
      case (name, value) =>
        var newValue = value

        if (value > 0) {
          if (tower.colors.contains(name)) {
            newValue -= tower.colors(name)
          }

          if (newValue < 0) {
            life += newValue
          }
        }

        (name, newValue)
    })

    actions += new ShootAction(frame, bug, tower)
  }

}

object Game {

  type Grid = Array[Array[Char]]
  type InfluenceMap = Array[Array[Double]]

  implicit class extendedGrid(grid: Grid) {
    lazy val numbered = createNumberedGrid(grid)
    lazy val size = (grid.length, grid(0).length)

    lazy val spawnPos = findPos('E')(grid)
    lazy val endPos = findPos('X')(grid)

    def stringify: String = grid.map(_.mkString(" ")).mkString("\n")
    def duplicate: Grid = grid.duplicate
  }

  implicit class extendedInfluenceMap(influenceMap: InfluenceMap) {
    lazy val size = (influenceMap.length, influenceMap(0).length)

    def stringify: String = influenceMap.map(_.mkString(" ")).mkString("\n")
  }

  /**
   * @return The coordinates of the given char.
   */
  def findPos(char: Char)(implicit grid: Grid): (Int, Int) = {
    val indexes = grid.map(_.indexOf(char))
    (indexes.indexWhere(_ != -1), indexes.find(_ != -1).get)
  }

  /**
   * @return The same grid with a numbered (i.e. E,1,2,...X instead of E,1,1,...X) path.
   */
  def createNumberedGrid(grid: Grid): Grid = {
    val newGrid = grid.duplicate
    fillNumbers(grid.spawnPos._1, grid.spawnPos._2, 0)(newGrid)
    newGrid(grid.spawnPos._1)(grid.spawnPos._2) = 'E'
    newGrid
  }

  /**
   * Used to create a numbered-path grid.
   */
  def fillNumbers(line: Int, col: Int, index: Int)(implicit grid: Grid): Unit = {
    if (grid(line)(col) == 'X') {
      return
    }

    grid(line)(col) = index.toChar

    if (col + 1 < grid.size._2 && grid(line)(col + 1) == 1) {
      fillNumbers(line, col + 1, index + 1)
    }

    else if (line + 1 < grid.size._1 && grid(line + 1)(col) == 1) {
      fillNumbers(line + 1, col, index + 1)
    }

    else if (col - 1 >= 0 && grid(line)(col - 1) == 1) {
      fillNumbers(line, col - 1, index + 1)
    }

    else if (line - 1 >= 0 && grid(line - 1)(col) == 1) {
      fillNumbers(line - 1, col, index + 1)
    }
  }

  /**
   * @return The coordinates of the next cell in the path, given that (line, col) is on the path.
   */
  def getNextCell(line: Int, col: Int)(implicit grid: Grid): (Int, Int) = {
    val numberedGrid = grid.numbered
    val size = grid.size

    if (col + 1 < size._2 && numberedGrid(line)(col + 1) > numberedGrid(line)(col)) {
      (line, col + 1)
    }

    else if (line + 1 < size._1 && numberedGrid(line + 1)(col) > numberedGrid(line)(col)) {
      (line + 1, col)
    }

    else if (col - 1 >= 0 && numberedGrid(line)(col - 1) > numberedGrid(line)(col)) {
      (line, col - 1)
    }

    else if (line - 1 >= 0 && numberedGrid(line - 1)(col) > numberedGrid(line)(col)) {
      (line - 1, col)
    }

    else {
      grid.endPos
    }
  }

  /**
   * @return The number of path cells a tower built on (line, col) would be able to shoot.
   */
  def pathCellsInRange(line: Int, col: Int, range: Int)(implicit grid: Grid): Int = {
    var cells = 0

    for (i <- line - range to line + range) {
      if (i >= 0 && i < grid.size._1) {
        for (j <- col - range to col + range) {
          if (j >= 0 && j < grid.size._2) {

            if (i != line && j != col && grid(i)(j) == 1) {
              cells += 1
            }

          }
        }
      }
    }

    if (cells == 0)
      cells = -100

    cells
  }

  /**
   * @return The position of the greatest value in the map.
   */
  def posOfGreatestValue(influenceMap: InfluenceMap): (Int, Int) = {
    var max: Double = -100
    var maxPos: (Int, Int) = (0, 0)

    for (line <- 0 to influenceMap.size._1 - 1) {
      for (col <- 0 to influenceMap.size._2 - 1) {
        if (influenceMap(line)(col) > max) {
          max = influenceMap(line)(col)
          maxPos = (line, col)
        }
      }
    }

    maxPos
  }

  def towerHasInRange(tower: Tower, bugs: Seq[Bug]): Boolean = {
    bugs.foreach { bug =>
      if (Math.abs(bug.line - tower.line) <= tower.range && Math.abs(bug.col - tower.col) <= tower.range) {
        return true
      }
    }

    false
  }

  def numberOfPossibleShots(tower: Tower)(implicit grid: Grid, bugs: Seq[Bug], startLife: Int): Int = {
    val tempGrid = grid.duplicate
    val tempBugs = bugs.map(_.clone().asInstanceOf[Bug])

    var life = startLife
    var frame = 0
    var shots = 0

    // Reset bugs
    tempBugs.foreach(_.isDead = false)

    while (frame != -1) {
      // Move bugs
      tempBugs.foreach { bug =>
        if (frame > bug.frame && !bug.isDead) {
          val nextCell = getNextCell(bug.line, bug.col)
          bug.line = nextCell._1
          bug.col = nextCell._2

          if (nextCell == grid.endPos) {
            life -= bug.totalHealth
            bug.isDead = true
          }
        }
      }

      // Spawn bugs
      tempBugs.foreach { bug =>
        if (frame == bug.frame) {
          bug.line = tempGrid.spawnPos._1
          bug.col = tempGrid.spawnPos._2
        }
      }

      // Calculate possibilities
      if (towerHasInRange(tower, tempBugs))
        shots += 1

      if (!tempBugs.exists(!_.isDead))
        frame = -1

      frame += 1
    }

    shots
  }

  /*def canKillBugsInShots(towers: List[Tower], shots: Int)(implicit grid: Grid, bugs: Seq[Bug]): Boolean = {
    val tempGrid = grid.duplicate
    val tempBugs = bugs.map(_.clone().asInstanceOf[Bug])

    var frame = 0
    var shots = 0

    // Reset bugs
    tempBugs.foreach(_.isDead = false)

    while (frame != -1) {
      // Move bugs
      tempBugs.foreach { bug =>
        if (frame > bug.frame && !bug.isDead) {
          val nextCell = getNextCell(bug.line, bug.col)
          bug.line = nextCell._1
          bug.col = nextCell._2

          if (nextCell == grid.endPos) {
            life -= bug.totalHealth
            bug.isDead = true
          }
        }
      }

      // Spawn bugs
      tempBugs.foreach { bug =>
        if (frame == bug.frame) {
          bug.line = tempGrid.spawnPos._1
          bug.col = tempGrid.spawnPos._2
        }
      }

      // Calculate possibilities
      if (towerHasInRange(tower, tempBugs))
        shots += 1

      if (!tempBugs.exists(!_.isDead))
        frame = -1

      frame += 1
    }

    shots
  }*/

}
