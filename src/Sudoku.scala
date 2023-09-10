/*
 * CS3210 - Principles of Programming Languages - Fall 2022
 * Instructor: Thyago Mota
 * Description: Prg 02 - Sudoku Puzzle
 * Students: Cameron Colliver, Stilian Balasopoulov
 */

import scala.io._

object Sudoku {

  // #1 return an 2D array of Int representing a sudoku board given a filename
  def readBoard(fileName: String): Array[Array[Int]] = {
    var board: Array[Array[Int]] = Array()
    Source.fromFile(fileName).getLines().foreach{ line =>
      board = board :+ line.map(c => c.asDigit).toArray
    }
    board
  }

  // #2 return a String representation from a given sudoku board
  def boardToString(board: Array[Array[Int]]): String = board.map(_.mkString).mkString("\n")

  // #3 return a specific row from a sudoku board as a sequence of numbers
  def getRow(board: Array[Array[Int]], row: Int): Array[Int] = board(row)

  // #4 return a specific column from a sudoku board as a sequence of numbers
  def getCol(board: Array[Array[Int]], col: Int): Array[Int] = board.map{_(col-1)}.toArray

  // #5: return a specific box from a sudoku board as a sequence of numbers
  def getBox(board: Array[Array[Int]], x: Int, y: Int): Array[Int] = {
    {
      for {
        yb <- (y * 3) until (y * 3 + 3)
        xb <- (x * 3) until (x * 3 + 3)
      } yield board(xb)(yb)
    }.toArray
  }
  // #6: a sequence is valid if it has 9 numbers in [0-9] with possibly repeated zeros
  def isValid(seq: Array[Int]): Boolean = {
    seq.filter(_ != 0).distinct.mkString == seq.filter(_ != 0).mkString &&
      seq.length == 9 && seq.mkString.forall(_.isDigit)
  }

  // #7: return whether all rows of the given board are valid sequences
  def allRowsValid(board: Array[Array[Int]]): Boolean = board.forall{row => isValid(row)}

  // #8: return whether all columns of the given board are valid sequences
  def allColsValid(board: Array[Array[Int]]) = (1 to 9).map(getCol(board, _)).filterNot(isValid).isEmpty
  // #9: return whether all boxes of the given board are valid sequences
  def allBoxesValid(board: Array[Array[Int]]): Boolean = {
    {
      for {
        x <- 0 to 2
        y <- 0 to 2
      } yield getBox(board, x, y)
    }.filterNot(isValid).isEmpty
  }
  // #10: a board is valid if all of its rows, columns, and boxes are also valid
  def isValid(board: Array[Array[Int]]): Boolean = allRowsValid(board) && allColsValid(board) && allBoxesValid(board)

  // #11: a board is complete it there is no zero
  def isComplete(board: Array[Array[Int]]): Boolean = board.flatMap(_.toList).indexOf(0) == -1

  // #12: a board is solved if is complete and valid
  def isSolved(board: Array[Array[Int]]): Boolean = isComplete(board) && isValid(board)

  // #13 return a new board configuration from the given one by setting a digit at a specific (row, col) location
  def getChoice(board: Array[Array[Int]], row: Int, col: Int, d: Int): Array[Array[Int]] = {
    var new_board = board.map(_.clone)
    new_board(row)(col) = d
    new_board
  }
  // #14: return all possible new board configurations from the given one
  def getChoices(board: Array[Array[Int]]): IndexedSeq[Array[Array[Int]]] = {
    var zero = board.flatMap(_.toList).indexOf(0)
    if (zero == -1) return null
    var row = zero / 9
    var col = zero % 9
    (1 to 9).map(getChoice(board, row, col, _)).filter(isValid)
  }

  // #15: return a solution to the puzzle (null if there is no solution)
  def solve(board: Array[Array[Int]]): Array[Array[Int]] = {
    try{
      if (getChoices(board) != null) {
        solve(getChoices(board)(0))
      } else {
        if (isSolved(board)) {
          return board
        } else {
          return null
        }
      }
    } catch{
      case e: IndexOutOfBoundsException => null
    }
  }



  def main(args: Array[String]): Unit = {
    var board = readBoard("../sudoku1.txt")
    try {
      val sol = solve(board)
      println(boardToString(sol))
    } catch{
      case e: NullPointerException => println("No solution")
    }
  }
}
