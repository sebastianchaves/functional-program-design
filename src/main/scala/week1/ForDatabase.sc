case class Book(title: String, authors: List[String])

val books: List[Book] = List(
  Book(title = "A", authors = List("1", "2")),
  Book(title = "B", authors = List("1", "8")),
  Book(title = "C", authors = List("6", "2", "7")),
  Book(title = "D", authors = List("3", "5"))
)

for {
  b <- books
  a <- b.authors
  if a.startsWith("1")
} yield b.title

for {
  b <- books
  if b.title.contains("C")
} yield b.title

for {
  b1 <- books
  b2 <- books
  if b1.title < b2.title
  a1 <- b1.authors
  a2 <- b2.authors
  if a1 == a2
} yield a1



def mapFun[T, U](xs: List[T], f: T => U): List[U] = for (x <- xs) yield f(x)

def flatMapFun[T, U](xs: List[T], f: T => Iterable[U]): List[U] = for (x <- xs; y <- f(x)) yield y

def filterFun[T](xs: List[T], f: T => Boolean): List[T] = for (x <- xs; if f(x)) yield x

books.flatMap(b => b.authors.withFilter(a => a startsWith "1").map(a => b.title))

books.flatMap(b => b.authors.withFilter(a => a startsWith "1").map(a => b.title))