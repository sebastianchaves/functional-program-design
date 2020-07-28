def expr = {
  val x = { print("x"); 1}
  lazy val y = { print("y"); 2}
  def z = { print("z"); 3}
  z + y + x + z + y + x
}

expr



def from(n: Int): Stream[Int] = n #:: from(n+1)

val nats = from(0)

nats.map(_ * 4).take(4).toList

def sieve(s: Stream[Int]): Stream[Int] = {
  s.head #:: sieve(s.tail.filter(_ % s.head != 0))
}

sieve(from(2)).take(100).toList


def sqrtStream(x: Double): Stream[Double] = {
  def improve(guess: Double): Double = (guess + x / guess) / 2
  lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
  guesses
}

sqrtStream(6).take(10).toList

def isGoodEnough(guess: Double, x: Double) = math.abs((guess * guess -x) / x) < 0.0001

sqrtStream(4).filter(isGoodEnough(_, 4)).take(10).toList

from(1).map(_ * 4)
from(1).filter(_ % 4 == 0)