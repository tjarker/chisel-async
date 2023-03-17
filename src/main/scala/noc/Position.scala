package noc

abstract class Position {
  def isCorner = this match {
    case NorthEasternCorner | SouthEasternCorner | SouthWesternCorner | NorthWesternCorner => true
    case _ => false
  }
  def isHorizontalEdge = this match {
    case NorthernEdge | SouthernEdge => true
    case _ => false
  }
  def isVerticalEdge = this match {
    case EasternEdge | WesternEdge => true
    case _ => false
  }
}

case object Inside extends Position

case object NorthernEdge extends Position

case object EasternEdge extends Position

case object SouthernEdge extends Position

case object WesternEdge extends Position

case object NorthEasternCorner extends Position

case object SouthEasternCorner extends Position

case object SouthWesternCorner extends Position

case object NorthWesternCorner extends Position