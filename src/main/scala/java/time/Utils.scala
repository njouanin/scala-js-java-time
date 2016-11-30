/*
 *********************************************************************************
 * "THE BEER-WARE LICENSE" (Revision 42):
 * <nico@beerfactory.org> wrote this file.  As long as you retain this notice you
 * can do whatever you want with this stuff. If we meet some day, and you think
 * this stuff is worth it, you can buy me a beer in return.   Nicolas JOUANIN
 *********************************************************************************
 */
package java.time

object Utils {
  def floorDiv(a: Long, b: Long): Long = {
    if (a >= 0)
      a / b
    else
      ((a + 1) / b) - 1
  }

  def floorMod(a: Long, b: Long): Long = ((a % b) + b) % b

  def safeMultiply(a: Int, b: Int): Int = {
    val total: Long = a.toLong * b.toLong
    if (total < Int.MinValue || total > Int.MaxValue) {
      throw new ArithmeticException(
        "Multiplication overflows an int: " + a + " * " + b)
    }
    total.toInt
  }

  def safeMultiply(a: Long, b: Int): Long = {
    b match {
      case -1 ⇒
        if (a == Long.MinValue)
          throw new ArithmeticException(
            "Multiplication overflows a long: " + a + " * " + b)
        else -a
      case 0 ⇒ 0L
      case 1 ⇒ a
      case _ ⇒
        val total: Long = a * b
        if (total / b != a) {
          throw new ArithmeticException(
            "Multiplication overflows a long: " + a + " * " + b)
        }
        total
    }
  }

  def safeMultiply(a: Long, b: Long): Long = {
    if (b == 1) {
      a
    } else {
      if (a == 1)
        b
      else {
        if (a == 0 || b == 0) {
          0
        } else {
          val total = a * b
          if (total / b != a || (a == Long.MinValue && b == -1) || (b == Long.MinValue && a == -1)) {
            throw new ArithmeticException(
              "Multiplication overflows a long: " + a + " * " + b)
          } else
            total
        }
      }
    }
  }

  def safeAdd(a: Int, b: Int): Int = {
    val sum: Int = a + b
    if ((a ^ sum) < 0 && (a ^ b) >= 0) {
      throw new ArithmeticException(
        "Addition overflows an int: " + a + " + " + b)
    } else
      sum
  }

  def safeAdd(a: Long, b: Long): Long = {
    val sum: Long = a + b
    // check for a change of sign in the result when the inputs have the same sign
    if ((a ^ sum) < 0 && (a ^ b) >= 0) {
      throw new ArithmeticException(
        "Addition overflows a long: " + a + " + " + b)
    } else sum
  }

  def compareLongs(a: Long, b: Long): Int = {
    if (a < b) {
      -1
    } else {
      if (a > b) {
        1
      } else 0
    }
  }

}
