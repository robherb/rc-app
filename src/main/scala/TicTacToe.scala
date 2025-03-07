import scala.io.StdIn.readLine
import scala.util.Try

object TicTacToe {

  private type Loc = (Int, Int) // implied to be bounded to a 3x3 grid
  private type XO = Option[Boolean] // None == empty, true = X, false = O
  private type Grid = Map[Loc, XO]

  // 3x3 grid, initial state, indexed by row then column
  private val initialGrid: Grid =
    (1 to 3).flatMap { col =>
      (1 to 3).map { row =>
        (col, row) -> None
      }
    }.toMap

  def main(args: Array[String]): Unit = {
    println("TicTacToe")
    println("Player 1 plays X's, player 2 plays O's")
    println("Enter moves using coordinates for row then column.\nFor example, top left: 1 1, bottom right: 3 3")
    gameLoop(initialGrid)
  }

  // alternate between player 1 and 2 making moves unless move is invalid or until game over
  private def gameLoop(initialGrid: Grid): Unit = {
    def loop(grid: Grid, isPlayer1: Boolean): Unit = {
      val gameOverState: Either[Unit, Boolean] = isGameOver(grid)

      gameOverState match {
        case Right(false) =>
          printGrid(grid)
          print(s"\nEnter move Player ${if (isPlayer1) { "1 (X)"} else {"2 (O)"}}: ")

          val playerInput: String = readLine()
          val playerLoc = parseLoc(playerInput)

          playerLoc match {
            case Some(loc) =>
              val xo: XO = Some(isPlayer1)
              makeMove(loc, xo, grid).fold {
                println("Invalid move, try again!")
                loop(grid, isPlayer1)
              }(loop(_, !isPlayer1))

            case None =>
              println("Unable to parse coordinate, try again!")
              loop(grid, isPlayer1)
          }

        case Right(true) =>
          println("\nGame Over!")
          // player who made last move before game over is the winner
          println(s"Player ${if (isPlayer1) { "2" } else { "1" }} Wins!")
          printGrid(grid)

        case Left(()) =>
          println("Stalemate!")
          printGrid(grid)
      }
    }

    // begin game loop, player one makes first move
    loop(initialGrid, isPlayer1 = true)
  }

  // Grid rendering //

  private def printGrid(grid: Grid): Unit = {
    println()
    (1 to 3).foreach { row =>
      printGridRow(grid, row)
    }
  }

  private def printGridRow(grid: Grid, row: Int): Unit = {
    print(renderXO(grid((row, 1))))
    print("|")
    print(renderXO(grid((row, 2))))
    print("|")
    println(renderXO(grid((row, 3))))
    if (row < 3) {
      println("-----")
    }
  }

  private def renderXO(xo: XO): String =
    xo.fold(" ")(xxoo => if (xxoo) { "X" } else { "O" })

  // Game logic //

  // Left = stalemate, right is game over due to win
  private def isGameOver(grid: Grid): Either[Unit, Boolean] = {
    val rowChecks: Seq[(XO, XO, XO)] =
      (1 to 3).map { row =>
        (grid(row, 1), grid(row, 2), grid(row, 3))
      }

    val columnChecks: Seq[(XO, XO, XO)] =
      (1 to 3).map { col =>
        (grid(1, col), grid(2, col), grid(3, col))
      }

    val diagonalChecks: Seq[(XO, XO, XO)] =
      Seq(
        (grid(1, 1), grid(2, 2), grid(3, 3)),
        (grid(1, 3), grid(2, 2), grid(3, 1))
      )

    // check if every space is filled
    lazy val isStalemate: Boolean =
      grid.forall { case (_, xo) => xo.isDefined }

    val hasCompletedRowColumnOrDiagonal =
      (rowChecks ++ columnChecks ++ diagonalChecks).foldLeft(false) { (acc, check) =>
        acc ||
          (check match {
            case (Some(true), Some(true), Some(true)) => true // all X's
            case (Some(false), Some(false), Some(false)) => true // all O's
            case _ => false
          })
      }

    if (hasCompletedRowColumnOrDiagonal) {
      Right(true)
    } else if (isStalemate) {
      Left(())
    } else {
      Right(false)
    }
  }

  // return grid state after making move, or None if move is invalid
  private def makeMove(loc: Loc, xo: XO, grid: Grid): Option[Grid] = {
    if (grid(loc).isEmpty) {
      Some(grid + (loc -> xo))
    } else {
      None
    }
  }

  // coordinates to be entered as two digits between 1 and 3 separated by a single whitespace
  private def parseLoc(loc: String): Option[Loc] = {
    val split = loc.split(" ", 2)
    val validatedCoords: Option[Array[Int]] =
      Try(split.map(_.toInt).filter(i => i > 0 && i < 4)).toOption.filter(_.length == 2)
    validatedCoords.map(arr => (arr(0), arr(1)))
  }

}
