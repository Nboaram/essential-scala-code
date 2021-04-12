package part2

import part2.SafeCalculator.eval

// ----------------------------------------------

// Step 1. Implement a `Result` type

sealed abstract class Result

final case class Success(a: Double) extends Result

final case class Fail(message: String = "Math Error") extends Result


// A Result is:
//   - a Pass containing a Double result, or
//   - a Fail containing a String error message

// ----------------------------------------------

// Step 2. Implement eval below:

object SafeCalculator {
  def eval(calc: Expr): Result = {

    calc match {
      case Num(x) => Success(x)
      case Addition(expr1, expr2) => eval(expr1) match {
        case Success(x) => eval(expr2) match {
          case Success(y) => Success(x + y)
          case Fail(message) => Fail(message)
        }
        case Fail(message) => Fail(message)
      }

      case Substraction(expr1, expr2) => eval(expr1) match {
        case Success(x) => eval(expr2) match {
          case Success(y) => Success(x - y)
          case Fail(message) => Fail(message)
        }
        case Fail(message) => Fail(message)
      }

      case Multiplication(expr1, expr2) => eval(expr1) match {
        case Success(x) => eval(expr2) match {
          case Success(y) => Success(x * y)
          case Fail(message) => Fail(message)
        }
        case Fail(message) => Fail(message)
      }
      case Division(expr1, expr2) => eval(expr1) match {
        case Success(x) => if (x == 0) {
          Success(0)
        }
        else {
          eval(expr2) match {
            case Success(y) => if (y == 0) {
              Fail("cannot divide by 0")
            }
            else {
              Success(x / y)
            }
          }

        }
      }
      case Squareroot(expr1) => eval(expr1) match {
        case Success(x) => Success(Math.sqrt(x))
        case Fail(message) => Fail(message)
      }
    }
  }

}

object Exercise13SafeCalculator {
  def main(args: Array[String]): Unit = {
    println("SaveCalculator.eval")
    // println(SafeCalculator.eval(Add(Num(1), Num(2))))
    // println(SafeCalculator.eval(Sqrt(Num(-1))))
    // println(SafeCalculator.eval(Div(Num(1), Num(0))))
  }
}
