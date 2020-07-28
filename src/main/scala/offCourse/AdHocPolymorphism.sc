def aggregate[A: Monoid](xs: Iterable[A]): A = xs.fold(Monoid[A].zero)(Monoid[A].add)

trait Semigroup[A] {
  def add(x: A, y: A): A
}

object Semigroup {
  def apply[A: Semigroup]: Semigroup[A] = implicitly
}

trait Monoid[A] extends Semigroup[A] {
  def zero: A
}

object Monoid {
  def apply[A: Monoid]: Monoid[A] = implicitly
}

implicit object IntAdder extends Monoid[Int] {
  override def add(x: Int, y: Int): Int = x + y
  override def zero: Int = 0
}

implicit object StringAdder extends Monoid[String] {
  override def add(x: String, y: String): String = x + y
  override def zero: String = ""
}

aggregate(Seq(1, 2, 3))
aggregate(Seq("Hola ", "Carola!"))
