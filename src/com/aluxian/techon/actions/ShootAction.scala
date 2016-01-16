package com.aluxian.techon.actions

import com.aluxian.techon.objects.{Bug, Tower}

case class ShootAction(frame: Int, bug: Bug, tower: Tower) extends Action {
  override def toString: String =
    s"""|action=shoot
        |frame=$frame
        |tower_name=${tower.name}
        |bug_name=${bug.name}
        |""".stripMargin
}
