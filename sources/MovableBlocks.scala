package user.sjrd.movableblocks

import com.funlabyrinthe.core.*
import com.funlabyrinthe.mazes.*
import com.funlabyrinthe.mazes.std.*

object MovableBlocks extends Module

@definition def movableBlockTemplate(using Universe) = new MovableBlock().asTemplate()

class MovableBlock(using ComponentInit) extends PosComponent {
  category = ComponentCategory("movableBlocks", "Movable blocks")
  painter += "Rocks/BigRock"

  protected def isDestSquareValid(square: Square): Boolean = square match
    case Square(_: Ground, e, t, o) => e.isEmpty && t.isEmpty && o.isEmpty
    case _                          => false

  protected def isDestRefValid(target: SquareRef): Boolean =
    isDestSquareValid(target())
      && target.posComponentsBottomUp.isEmpty

  protected def isMoveAllowed(context: EnteringContext, target: SquareRef): Boolean =
    isDestRefValid(target)

  override def hookPushing(context: EnteringContext): Unit = {
    import context.*

    val target = pos +> player.direction
    if isMoveAllowed(context, target) then
      applyMove(context, target)
      hooked = false
    else
      context.cancel()
  }

  protected def applyMove(context: EnteringContext, target: SquareRef): Unit =
    position = Some(target)
}

class AnchoredMovableBlock(using ComponentInit) extends MovableBlock {
  @noinspect
  var originalPosition: Option[SquareRef] = None

  override protected def startGame(): Unit =
    fixThere()

  def reset(): Unit =
    position = originalPosition

  def fixThere(): Unit =
    originalPosition = position
}

class ConstrainedMovableBlock(using ComponentInit) extends AnchoredMovableBlock {
  var canCrossZones: Boolean = false
  var allowedDirs: Set[Direction] = Direction.values.toSet
  var maximumMoveCount: Int = -1

  @noinspect
  var movesDoneSoFar: Int = 0

  override protected def isMoveAllowed(context: EnteringContext, target: SquareRef): Boolean =
    super.isMoveAllowed(context, target)
      && (canCrossZones || target.zone == context.dest.zone)
      && allowedDirs.contains(context.player.direction)
      && (maximumMoveCount < 0 || movesDoneSoFar < maximumMoveCount)

  override protected def applyMove(context: EnteringContext, target: SquareRef): Unit =
    super.applyMove(context, target)
    movesDoneSoFar += 1
}
