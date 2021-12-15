import indigo._
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("IndigoGame")
object HelloIndigo extends IndigoSandbox[Unit, Model] {

  val magnification = 3

  val config: indigo.GameConfig =
    GameConfig.default.withMagnification(magnification)

  val animations: Set[Animation] =
    Set()

  val assetName = AssetName("dots")
  val easy      = AssetName("easy")

  val assets: Set[indigo.AssetType] = Set(
    AssetType.Image(assetName, AssetPath("assets/dots.png")),
    AssetType.Image(easy, AssetPath("assets/90.png"))
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
  ): GlobalEvent => Outcome[Model] =
    _ => Outcome(model)

  def present(
      context: FrameContext[Unit],
      model: Model
  ): Outcome[SceneUpdateFragment] =
    Outcome(
      SceneUpdateFragment(
        Graphic(Rectangle(0, 0, 96, 96), 1, Material.Bitmap(easy).stretch)
          .withRef(48, 48)
          .moveTo(config.viewport.giveDimensions(magnification).center)
      )
    )
}

final case class Model(level: Int, angle: Radians) {
  def update(timeDelta: Seconds): Model =
    this.copy(angle = angle + Radians.fromSeconds(timeDelta))
}

object Model {
  def initial(): Model = Model(0, Radians(0))
}
