package sexprs

case class Position(line: Int, col: Int) extends Ordered[Position] {

  def compare(that: Position) = {
    val ld = this.line - that.line
    if (ld == 0) {
      this.col - that.col
    } else {
      ld
    }
  }

}

case class Range(start: Position, end: Position)

trait Ranged {

  private var _range: Option[Range] = None

  def setRange(start: Position, end: Position): this.type = {
    _range = Some(Range(start, end))
    this
  }

  def setRange(range: Range): this.type = {
    _range = Some(range)
    this
  }

  def setRange(that: Ranged): this.type = {
    _range = Some(that.getRange)
    this
  }

  def getRange: Range = {
    _range.get
  }
}
