package com.aluxian.techon

import com.aluxian.techon.Utils._

object Main {
  def main(args: Array[String]) {
    val game = readGame(args(0))
    game.play(new Config(1))

    println("=== SOLUTION ===")
    val solution = game.solution()
    writeSolution(solution, args(0))
    println(solution)

    println("=== SOLUTION STATS ===")
    println("Life: " + game.life)
    println("Money: " + game.money)
  }
}
