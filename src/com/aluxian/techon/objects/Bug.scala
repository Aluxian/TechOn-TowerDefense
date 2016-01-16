package com.aluxian.techon.objects

case class Bug(name: String, var colors: Map[String, Int], frame: Int, var line: Int = 0, var col: Int = 0, var isDead: Boolean = false) {
  def totalHealth = colors.values.reduce({ (a, b) => a + b})
}
