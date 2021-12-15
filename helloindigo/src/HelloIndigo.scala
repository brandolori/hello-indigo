import indigo._
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("IndigoGame")
object HelloIndigo extends IndigoSandbox[Unit, Model] {

  val magnification = 3

  val config: GameConfig =
    GameConfig.default.withMagnification(magnification)

  val animations: Set[Animation] =
    Set()

  val easy      = AssetName("easy")
  val medium    = AssetName("medium")
  val hard      = AssetName("hard")
  val nightmare = AssetName("nightmare")

  val assets: Set[AssetType] =
    Set(
      AssetType.Image(easy, AssetPath("assets/90.png")),
      AssetType.Image(medium, AssetPath("assets/60.png")),
      AssetType.Image(hard, AssetPath("assets/47.png")),
      AssetType.Image(nightmare, AssetPath("assets/10.png"))
    )

  val fonts: Set[FontInfo] =
    Set()

  val shaders: Set[Shader] =
    Set()

  def setup(
      assetCollection: AssetCollection,
      dice: Dice
  ): Outcome[Startup[Unit]] =
    Outcome(Startup.Success(()))

  def initialModel(startupData: Unit): Outcome[Model] =
    Outcome(
      Model.initial(
        config.viewport.giveDimensions(magnification).center
      )
    )

  def updateModel(
      context: FrameContext[Unit],
      model: Model
  ): GlobalEvent => Outcome[Model] = {
    case MouseEvent.Click(pt) =>
      val adjustedPosition = pt - model.center

      Outcome(
        model
          .addDot(
            Dot(
              Point.distanceBetween(model.center, pt).toInt,
              Radians(
                Math.atan2(
                  adjustedPosition.x.toDouble,
                  adjustedPosition.y.toDouble
                )
              )
            )
          )
          .reverse()
      )

    case FrameTick =>
      Outcome(model.update(context.delta))

    case _ =>
      Outcome(model)
  }

  def present(
      context: FrameContext[Unit],
      model: Model
  ): Outcome[SceneUpdateFragment] =
    Outcome(
      SceneUpdateFragment(
        Graphic(Rectangle(0, 0, 32, 32), 1, Material.Bitmap(easy))
      )
    )

  // def drawDots(
  //     center: Point,
  //     dots: List[Dot]
  // ): List[Graphic[_]] =
  //   dots.map { dot =>
  //     val position = Point(
  //       (Math.sin(dot.angle.toDouble) * dot.orbitDistance + center.x).toInt,
  //       (Math.cos(dot.angle.toDouble) * dot.orbitDistance + center.y).toInt
  //     )

  //     Graphic(Rectangle(0, 0, 32, 32), 1, Material.Bitmap(assetName))
  //       .withCrop(Rectangle(16, 16, 16, 16))
  //       .withRef(8, 8)
  //       .moveTo(position)
  //   }

}

enum DotsDirection:
  case Clockwise, CounterClockwise

case class Model(center: Point, dots: List[Dot], direction: DotsDirection) {
  def addDot(dot: Dot): Model =
    this.copy(dots = dot :: dots)

  def reverse(): Model =
    this.copy(direction =
      if (direction == DotsDirection.Clockwise) DotsDirection.CounterClockwise else DotsDirection.Clockwise
    )

  def update(timeDelta: Seconds): Model =
    this.direction match {
      case DotsDirection.Clockwise        => this.copy(dots = dots.map(_.update(timeDelta)))
      case DotsDirection.CounterClockwise => this.copy(dots = dots.map(_.reverseUpdate(timeDelta)))
    }

}

object Model {
  def initial(center: Point): Model = Model(center, Nil, DotsDirection.Clockwise)
}

case class Dot(orbitDistance: Int, angle: Radians) {
  def update(timeDelta: Seconds): Dot =
    this.copy(angle = angle + Radians.fromSeconds(timeDelta))
  def reverseUpdate(timeDelta: Seconds): Dot =
    this.copy(angle = angle - Radians.fromSeconds(timeDelta))
}
