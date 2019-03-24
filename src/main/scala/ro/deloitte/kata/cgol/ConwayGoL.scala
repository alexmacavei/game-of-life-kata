package ro.deloitte.kata.cgol

object ConwayGoL {
  type Grid = Array[Array[Boolean]]

  def grid(i: Int): Grid = Array.ofDim[Boolean](i, i)


  def gameOfLife(firstGeneration: Grid): Grid = {
    if (firstGeneration.length != firstGeneration(0).length) {
      throw new IllegalArgumentException("Grid is not a square.")
    }
    val nextGeneration = grid(firstGeneration.length)
    for {
      i <- firstGeneration.indices
      j <- firstGeneration(0).indices
    } nextGeneration(i)(j) = cellLives(firstGeneration)(i)(j)
    nextGeneration
  }

  private def getAliveNeighbours(lookupGrid: Grid)(i: Int)(j: Int): Int = {
    var aliveNeighbours = 0
    for {
      x <- i-1 to i+1; if isNotOutOfGrid(lookupGrid, x)
      y <- j-1 to j+1; if isNotOutOfGrid(lookupGrid, y)
    } {
      if (lookupGrid(x)(y)) aliveNeighbours += 1
    }
    if (lookupGrid(i)(j)) aliveNeighbours - 1 else aliveNeighbours
  }

  private def isNotOutOfGrid(lookupGrid: Grid, pos: Int) = {
    pos >= 0 && pos <= lookupGrid.length - 1
  }

  private def cellLives(grid: Grid)(i: Int)(j: Int): Boolean =
    if (grid(i)(j)) {
      validRuleOne(grid)(i)(j) && validRuleTwo(grid)(i)(j) && validRuleThree(grid)(i)(j)
    } else {
      validRuleFour(grid)(i)(j)
    }

  //  1. Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
  private def validRuleOne(grid: Grid)(i: Int)(j: Int): Boolean = !(grid(i)(j) && getAliveNeighbours(grid)(i)(j) < 2)

  //  2. Any live cell with more than three live neighbours dies, as if by overcrowding.
  private def validRuleTwo(grid: Grid)(i: Int)(j: Int): Boolean = !(grid(i)(j) && getAliveNeighbours(grid)(i)(j) > 3)

  //  3. Any live cell with two or three live neighbours lives on to the next generation.
  private def validRuleThree(grid: Grid)(i: Int)(j: Int): Boolean = grid(i)(j) && (2 to 3 contains getAliveNeighbours(grid)(i)(j))

  //  4. Any dead cell with exactly three live neighbours becomes a live cell.
  private def validRuleFour(grid: Grid)(i: Int)(j: Int): Boolean = !grid(i)(j) && getAliveNeighbours(grid)(i)(j) == 3
}