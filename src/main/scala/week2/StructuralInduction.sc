trait IntSet {
  def incl(x: Int): IntSet

  def contains(x: Int): Boolean

  def union(x: IntSet): IntSet
}

case class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  override def incl(x: Int): IntSet = {
    if (x < elem) NonEmpty(elem, left incl x, right)
    else if (x > elem) NonEmpty(elem, left, right incl x)
    else this
  }

  override def contains(x: Int): Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }

  override def union(other: IntSet): IntSet = (left union (right union other)) incl elem
}

object Empty extends IntSet {
  override def incl(x: Int): IntSet = NonEmpty(x, Empty, Empty)

  override def contains(x: Int): Boolean = false

  override def union(other: IntSet): IntSet = other
}

// Prove laws
// #1
val firstLaw: Int => Boolean = { x => Empty contains x }
val secondLaw1: Int => Boolean = { x => (Empty incl x) contains x }
val secondLaw2: Int => Boolean = {x => (NonEmpty(x, Empty, Empty) incl x) contains x}
// val secondLaw3: Int => Boolean = {x => NonEmpty(2, Empty, Empty) incl}

def thirdLaw(x: Int, y: Int): Boolean = ((Empty incl y) contains x) == (Empty contains x)

thirdLaw(1, 6)
