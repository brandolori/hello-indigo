import indigo._
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("IndigoGame")
object HelloIndigo extends IndigoSandbox[Unit, Model] {

  val magnification = 3

  val config: indigo.GameConfig =
    GameConfig.default.withMagnification(magnification).withClearColor(RGBA.White)

  val animations: Set[Animation] =
    Set()

  val assets: Set[indigo.AssetType] = Set(
    AssetType.Image(easy, AssetPath("assets/90.png")),
    AssetType.Image(medium, AssetPath("assets/60.png")),
    AssetType.Image(hard, AssetPath("assets/47.png")),
    AssetType.Image(nightmare, AssetPath("assets/17.png")),
    AssetType.Image(arrow, AssetPath("assets/downarrow.png"))
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
    Outcome(Model.initial())

  def updateModel(
      context: FrameContext[Unit],
      model: Model
  ): GlobalEvent => Outcome[Model] = {
    case MouseEvent.Click(pt) =>
      Outcome(
        model.click
      )

    case FrameTick =>
      Outcome(model.update(context.delta))

    case _ =>
      Outcome(model)
  }

  val circleSize = 64

  def present(
      context: FrameContext[Unit],
      model: Model
  ): Outcome[SceneUpdateFragment] =
    Outcome(
      SceneUpdateFragment(
        Graphic(Rectangle(0, 0, circleSize, circleSize), 1, Material.Bitmap(getLevelParams(model.level).image).stretch)
          .withRef(circleSize / 2, circleSize / 2)
          .moveTo(config.viewport.giveDimensions(magnification).center)
          .rotateTo(model.angle),
        Graphic(Rectangle(0, 0, 24, 24), 1, Material.Bitmap(arrow).stretch)
          .withRef(12, 0)
          .moveTo(Point(config.viewport.giveDimensions(magnification).center.x, 0))
      )
    )
}

val easy      = AssetName("easy")
val medium    = AssetName("medium")
val hard      = AssetName("hard")
val nightmare = AssetName("nightmare")
val arrow     = AssetName("downarrow")

def getLevelParams(level: Int) =
  level match {
    case 0 => Level(.1d, Radians.fromDegrees(90), easy)
    case 1 => Level(-.3d, Radians.fromDegrees(60), medium)
    case 2 => Level(.5d, Radians.fromDegrees(47), hard)
    case 3 => Level(-1d, Radians.fromDegrees(17), nightmare)
  }

final case class Level(speed: Double, margin: Radians, image: AssetName)

final case class Model(level: Int, angle: Radians) {

  def update(timeDelta: Seconds): Model =
    this.copy(angle = angle + Radians.fromSeconds(timeDelta) * getLevelParams(level).speed)

  def click: Model =
    println(angle.wrap)
    val win = angle.wrap.toDouble < getLevelParams(level).margin.toDouble
    this.copy(level = if (win) Math.min(3, level + 1) else Math.max(0, level - 1))
}

object Model {
  def initial(): Model = Model(0, Radians(0))
}
