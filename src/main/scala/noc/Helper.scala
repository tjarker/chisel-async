package noc

object Helper {

  case class Grid(n: Int, m: Int)

  implicit class GridBuilder(n: Int) {
    def by(m: Int): Grid = Grid(n, m)
  }

}
