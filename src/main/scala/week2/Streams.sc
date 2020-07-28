val stream = Stream("hola", "chao", "hello")

val list = List(1, 2, 3)

list.toStream

def streamRange(lo: Int, hi: Int): Stream[Int] = {
  if (lo >= hi) Stream.empty
  else Stream.cons(lo, streamRange(lo + 1, hi))
}

def listRange(lo: Int, hi: Int): List[Int] = {
  if (lo >= hi) Nil
  else lo :: listRange(lo + 1, hi)
}

(1000 to 10000).toStream.filter(_ > 1000)(5)

1 #:: 2 #:: Stream.Empty



def streamRangeV2(lo: Int, hi: Int): Stream[Int] = {
  print(lo + " ")
  if (lo >= hi) Stream.empty
  else Stream.cons(lo, streamRangeV2(lo + 1, hi))
}

streamRangeV2(1, 10).take(3).toList