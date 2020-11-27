package com.yarenty.cwr

import indigo._
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("IndigoGame")
object Main extends IndigoSandbox[Unit, Unit] {

  val config: GameConfig =
    GameConfig.default

  val animations: Set[Animation] =
    Set()

  val assets: Set[AssetType] =
    Set()

  val fonts: Set[FontInfo] =
    Set()

  def setup(
             assetCollection: AssetCollection,
             dice: Dice
           ): Startup[Unit] =
    Startup.Success(())

  def initialModel(startupData: Unit): Unit =
    ()

  def updateModel(
                   context: FrameContext[Unit],
                   model: Unit
                 ): GlobalEvent => Outcome[Unit] =
    _ => Outcome(())

  def present(
               context: FrameContext[Unit],
               model: Unit
             ): SceneUpdateFragment =
    SceneUpdateFragment.empty


  def main(args: Array[String]): Unit = {
    Main.launch()
  }

}
