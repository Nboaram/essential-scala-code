package part2

import part2.Calculator.eval

// ----------------------------------------------

// Step 1. Write a definition for Expr here!

sealed abstract class Expr

final case class Num(int:Double) extends Expr
final case class Addition(int1: Expr, int2: Expr) extends Expr
final case class Substraction(int1: Expr, int2: Expr) extends Expr
final case class Multiplication(int1: Expr, int2: Expr) extends Expr
final case class Division(int1: Expr, int2: Expr) extends Expr
final case class Squareroot(int1: Expr) extends Expr
// Handle the following types of equation:
// - addition
// - subtraction
// - multiplication
// - division
// - square root

// Give it a `stringify` method
// that renders the expression as a string.


// ----------------------------------------------

// Step 2. Implement eval
// for each of the "calculator" objects below:

object Calculator {
  def stringify(calc: Expr): String = {
    calc match {
      case Num(expr1) => s"${expr1}"
      case Addition(int1, int2) => s"${stringify(int1)} + ${stringify(int2)} = ${eval(calc)}"
      case Substraction(int1, int2) => s"${stringify(int1)} - ${stringify(int2)} = ${eval(calc)}"
      case Multiplication(int1, int2) => s"${stringify(int1)} * ${stringify(int2)} = ${eval(calc)}"
      case Division(int1, int2) => s"${stringify(int1)} / ${stringify(int2)} = ${eval(calc)}"
      case Squareroot(x) => s" Square Root of $x = ${Math.sqrt(eval(x))}"

    }
  }

  def eval(calc: Expr): Double = {
    calc match {
      case Num(x) => x
      case Addition(expr1, expr2) => eval(expr1) + eval(expr2)
      case Substraction(expr1, expr2) => eval(expr1) - eval(expr2)
      case Multiplication(expr1, expr2) => eval(expr1) * eval(expr2)
      case Division(expr1, expr2)  => eval(expr1) / eval(expr2)
      case Squareroot(expr1) => Math.sqrt(eval(expr1))
    }
  }
}

object IntCalculator {
  def eval(calc: Expr): Int = {
    calc match {
      case Num(x) => x.toInt
      case Addition(expr1, expr2) => eval(expr1) + eval(expr2)
      case Substraction(expr1, expr2) => eval(expr1) - eval(expr2)
      case Multiplication(expr1, expr2) => eval(expr1) * eval(expr2)
      case Division(expr1,expr2) => eval(expr1) / eval(expr2)
      case Squareroot(expr1) =>Math.sqrt(eval(expr1)).round.toInt
    }
  }
}

// ----------------------------------------------

// Step 3. Write some convenience methods
// for constructing common calculations:

// ----------------------------------------------

object Expr {
  // def pythag(a: Double, b: Double): Expr = {
  //   ???
  // }

  // def factorial(n: Int): Expr = {
  //   ???
  // }
}

object Exercise11Calculator {
  val calc1: Expr = Addition(Num(1.1), Multiplication(Num(2.2), Num(3.3)))
  val calc2: Expr = Addition(Multiplication(Num(1.1), Num(2.2)), Num(3.3))



  def main(args: Array[String]): Unit = {
    println("stringify")

     println(Calculator.stringify(calc1))
     println(Calculator.stringify(calc2))

    println()

    println("Calculator.eval")
     println(Calculator.eval(calc1))
     println(Calculator.eval(calc2))
    println()

    println("IntCalculator.eval")
    // println(IntCalculator.eval(calc1))
    // println(IntCalculator.eval(calc2))

    println("pythag")
    // println(Expr.pythag(3, 4))
    // println(Calculator.eval(Expr.pythag(3, 4)))
    // println(IntCalculator.eval(Expr.pythag(3, 4)))

    println("factorial")
    // println(Expr.factorial(4))
    // println(Calculator.eval(Expr.factorial(4)))
    // println(IntCalculator.eval(Expr.factorial(4)))
  }
}
