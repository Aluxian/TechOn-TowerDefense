package com.aluxian.techon.objects

case class Tower(id: Int, colors: Map[String, Int], var line: Int, var col: Int, range: Int) {
  val name = "tower_" + id
  val position = (line, col)
}
