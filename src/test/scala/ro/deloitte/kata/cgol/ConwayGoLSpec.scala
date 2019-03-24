package ro.deloitte.kata.cgol

import ro.deloitte.kata.cgol.ConwayGoL._

class ConwayGoLSpec extends UnitSpec {

  "Game of life" should "evolve into empty 1x1 grid when input is one dead cell" in {
    val firstGeneration = grid(1)
    firstGeneration(0)(0) = false
    assert(gameOfLife(firstGeneration).deep == Array(Array(false)).deep)
  }

  it should "provide only square grids" in {
    val firstGeneration = Array.ofDim[Boolean](1, 3)
    assertThrows[IllegalArgumentException] {
    gameOfLife(firstGeneration)
    }
  }

  it should "correctly evolve a 3x3 grid like\n" +
            """*..    ...
              |*.. -> ...
              |...    ...""".stripMargin in {
    val firstGeneration = grid(3)
    firstGeneration(0)(0) = true
    firstGeneration(0)(1) = true
    assert(gameOfLife(firstGeneration).deep == grid(3).deep)
  }

  it should "correctly evolve a 4x4 grid like\n" +
            """....    ....
              |.*.. -> **..
              |**..    **..
              |....    ....""".stripMargin in {
    val firstGeneration = grid(4)
    firstGeneration(0)(2) = true
    firstGeneration(1)(1) = true
    firstGeneration(1)(2) = true

    val expectedGeneration = grid(4)
    expectedGeneration(0)(1) = true
    expectedGeneration(0)(2) = true
    expectedGeneration(1)(1) = true
    expectedGeneration(1)(2) = true

    assert(gameOfLife(firstGeneration).deep == expectedGeneration.deep)
  }

  it should "correctly evolve a 5x5 grid like\n" +
    """....*    .....
      |.*... -> ***..
      |***..    ***..
      |.....    ...*.
      |..**.    .....""".stripMargin in {
    val firstGeneration = grid(5)
    firstGeneration(0)(2) = true
    firstGeneration(1)(1) = true
    firstGeneration(1)(2) = true
    firstGeneration(2)(2) = true
    firstGeneration(2)(4) = true
    firstGeneration(3)(4) = true
    firstGeneration(4)(0) = true

    val expectedGeneration = grid(5)
    expectedGeneration(0)(1) = true
    expectedGeneration(0)(2) = true
    expectedGeneration(1)(1) = true
    expectedGeneration(1)(2) = true
    expectedGeneration(2)(1) = true
    expectedGeneration(2)(2) = true
    expectedGeneration(3)(3) = true

    assert(gameOfLife(firstGeneration).deep == expectedGeneration.deep)
  }

  def traverseAndPrint(grid: Grid): Unit = {
    for {
      j <- grid(0).indices
    } {
      for {
        i <- grid.indices
      } print(if (grid(i)(j)) "*" else ".")
      print("\n")
    }
  }
}
