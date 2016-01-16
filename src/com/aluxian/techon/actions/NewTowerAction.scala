package com.aluxian.techon.actions

import com.aluxian.techon.objects.Tower

case class NewTowerAction(frame: Int, tower: Tower) extends Action {
  override def toString: String = {
    val colors = tower.colors.map({ case (color, value) => color + ":" + value}).mkString(",")
    val position = tower.col + "," + tower.line
    val name = tower.name

    s"""|action=new_tower
        |frame=$frame
        |name=$name
        |position=$position
        |colors=$colors
        |""".stripMargin
  }
}
