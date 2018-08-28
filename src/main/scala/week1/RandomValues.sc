trait RandomGenerator[+T] {
  self =>

  def generate: T

  def map[S](f: T => S): RandomGenerator[S] = new RandomGenerator[S] {
    override def generate: S = f(self.generate)
  }

  def flatMap[S](f: T => RandomGenerator[S]): RandomGenerator[S] = new RandomGenerator[S] {
    override def generate: S = f(self.generate).generate
  }

}

// Methods
def single[T](x: T): RandomGenerator[T] = new RandomGenerator[T] {
  override def generate: T = x
}

def choose(lo: Int, hi: Int): RandomGenerator[Int] = for (x <- integers) yield lo + (x % (hi - lo))

def oneOf[T](xs: T*): RandomGenerator[T] = for(idx <- choose(0, xs.length)) yield xs(idx)

// Integers
val integers: RandomGenerator[Int] = new RandomGenerator[Int] {
  val random = new java.util.Random
  override def generate: Int = random.nextInt
}

// Booleans
val booleans: RandomGenerator[Boolean] = integers.map(_ >= 0)

// Tuples
def pairs[T, U](t: RandomGenerator[T], u: RandomGenerator[U]): RandomGenerator[(T, U)] = for {
  x <- t
  y <- u
} yield (x, y)

// Lists
def lists: RandomGenerator[List[Int]] = for {
  isEmpty <- booleans
  list <- if (isEmpty) emptyLists else nonEmptyLists
} yield list

def emptyLists: RandomGenerator[List[Int]] = single(Nil)
def nonEmptyLists: RandomGenerator[List[Int]] = for {
  head <- integers
  tail <- lists
} yield head :: tail


// Tree
trait Tree
case class Inner(left: Tree, right: Tree) extends Tree
case class Leaf(x: Int) extends Tree

def trees: RandomGenerator[Tree] = for {
  isLeaf <- booleans
  tree <- if (isLeaf) leafs else inners
} yield tree

def leafs: RandomGenerator[Leaf] = for {
  value <- integers
} yield Leaf(value)

def inners: RandomGenerator[Inner] = for {
  left <- trees
  right <- trees
} yield Inner(left, right)

// Tests
def test[T](g: RandomGenerator[T], numTimes: Int = 100)(test: T => Boolean): Unit = {
  for(i <- 0 until numTimes) {
    val value = g.generate
    assert(test(value), s"test failed for $value")
  }
  println(s"passed $numTimes tests")
}

test(pairs(lists, lists)) {
  case (xs, ys) => (xs ++ ys).length > xs.length
}




