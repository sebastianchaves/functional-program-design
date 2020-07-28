import scala.annotation.tailrec

def factorial(n: BigDecimal): BigDecimal = {

  @tailrec
  def factorial(n: BigDecimal, acc: BigDecimal): BigDecimal =
    if(n == 0) acc else factorial(n-1, acc * n)

  factorial(n, 1)

}

factorial(1)

val pseudoResult = Map("a" -> 0.6,"b" -> 0.2, "c" -> 1.0)
val earlyAbort = 0.5

def expensiveFunc(s: String): Double = {
  println(s"Evaluating for $s")
  pseudoResult(s)
}

val inputsToTry = Seq("a","b","c")

val results = inputsToTry.toStream.map(input => input -> expensiveFunc(input))



val finalResult = results.find { case (k, res) => res < earlyAbort }.getOrElse(results.minBy(_._2))

def flip[A,B,C](f: (A, B) => C): (B, A) => C = {
  (b, a) => f(a, b)
}

PartialFunction

10 / 0
