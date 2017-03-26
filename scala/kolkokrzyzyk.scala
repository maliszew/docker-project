/**
  * Created by osboxes on 21/03/17.
  */

println("Kolko i krzyzyk")

print("1 2 3\n4 5 6\n7 8 9\n\n")

val matrix = Array.ofDim[String](3,3) // potrzebujemy macierz stringow a nie intow.
// matrix: Array[Array[Int]] = Array(Array(0, 0), Array(0, 0))
val moves = Array.ofDim[Boolean](3,3)

def printBoard {
  for(i <- 0 until 3) {
    for(j <- 0 until 3) {
      print(matrix(i)(j) + " ")
    }
    println()
  }
}

def resetBoard {
  for(i <- 0 until 3) {
    for(j <- 0 until 3) {
      matrix(i)(j) = "_"
      moves(i)(j) = false
    }
  }
}

def calculateColRow(where: Int): (Int, Int) = {
  var column = (where + 2) % 3
  var row = where / 3
  if(where % 3 == 0) row -= 1
  // println(who + "! " + row + ", " + column)
  (row, column)
}

def makeMove(where:Int, who:String): Boolean =  {
  var (row, column) = calculateColRow(where)

  if(!moves(row)(column)) {
    matrix(row)(column) = who
    moves(row)(column) = true
    true
  }
  else {
    println("Do not cheat! There is something in here already.")
    false
  }
}

def findWinner(where: Int, who: String): Boolean = {
  var (row, column) = calculateColRow(where)
  var (row1, column1) = calculateColRow(1)
  var (row2, column2) = calculateColRow(2)
  var (row3, column3) = calculateColRow(3)
  var (row4, column4) = calculateColRow(4)
  var (row5, column5) = calculateColRow(5)
  var (row6, column6) = calculateColRow(6)
  var (row7, column7) = calculateColRow(7)
  var (row8, column8) = calculateColRow(8)
  var (row9, column9) = calculateColRow(9)

  if(where == 1) {
    if((matrix(row2)(column2) == who && matrix(row3)(column3) == who) ||
      (matrix(row4)(column4) == who && matrix(row7)(column7) == who) ||
      (matrix(row5)(column5) == who && matrix(row9)(row9) == who)) {
      println("Winner is " + who)
      return true
    }
  }
  else if(where == 2) {
    if((matrix(row1)(column1) == who && matrix(row3)(column3) == who) ||
      (matrix(row5)(column5) == who && matrix(row8)(row8) == who)) {
      println("Winner is " + who)
      return true
    }
  }
  else if(where == 3) {
    if((matrix(row2)(column2) == who && matrix(row1)(column1) == who) ||
      (matrix(row6)(column6) == who && matrix(row9)(column9) == who) ||
      (matrix(row5)(column5) == who && matrix(row7)(row7) == who)) {
      println("Winner is " + who)
      return true
    }
  }
  else if(where == 4) {
    if((matrix(row1)(column1) == who && matrix(row7)(column7) == who) ||
      (matrix(row5)(column5) == who && matrix(row6)(row6) == who)) {
      println("Winner is " + who)
      return true
    }
  }
  else if(where == 5) {
    if((matrix(row2)(column2) == who && matrix(row8)(column8) == who) ||
      (matrix(row4)(column4) == who && matrix(row6)(column6) == who) ||
      (matrix(row1)(column1) == who && matrix(row9)(column9) == who) ||
      (matrix(row3)(column3) == who && matrix(row7)(row7) == who)) {
      println("Winner is " + who)
      return true
    }
  }
  else if(where == 6) {
    if((matrix(row3)(column3) == who && matrix(row9)(column9) == who) ||
      (matrix(row4)(column4) == who && matrix(row5)(row5) == who)) {
      println("Winner is " + who)
      return true
    }
  }
  else if(where == 7) {
    if((matrix(row1)(column1) == who && matrix(row4)(column4) == who) ||
      (matrix(row8)(column8) == who && matrix(row9)(column9) == who) ||
      (matrix(row5)(column5) == who && matrix(row3)(row3) == who)) {
      println("Winner is " + who)
      return true
    }
  }
  else if(where == 8) {
    if((matrix(row2)(column2) == who && matrix(row5)(column5) == who) ||
      (matrix(row7)(column7) == who && matrix(row9)(column9) == who)) {
      println("Winner is " + who)
      return true
    }
  }
  else if(where == 9) {
    if((matrix(row1)(column1) == who && matrix(row5)(column5) == who) ||
      (matrix(row6)(column6) == who && matrix(row3)(column3) == who) ||
      (matrix(row8)(column8) == who && matrix(row9)(row9) == who)) {
      println("Winner is " + who)
      return true
    }
  }
  return false
}

resetBoard
printBoard

var step = 0
while (step < 10) {
  var move = -1
  if(step % 2 == 0) {
    println("Go Circle! O!")
    move = scala.io.StdIn.readInt()
    // println(move)
    if (!makeMove(move, "O")) step -= 1
    if (step >= 0) {
      if (findWinner(move, "O")) {
        step = 10
      }
    }
  }
  else {
    println("Go Cross! X!")
    move = scala.io.StdIn.readInt()
    // println(move)
    if(!makeMove(move, "X")) step -= 1
    if(step >= 0) {
      if(findWinner(move, "X"))  {
        step = 10
      }
    }
  }
  step += 1
  printBoard


}
