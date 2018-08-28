val intList: List[Int] = (1 to 100).toList


implicit class RichList(list: List[Int]) {
  def listasLocas(f: Int => Boolean): (List[Int], List[Int]) = list.foldLeft((Nil: List[Int], Nil: List[Int])){
    (accum, elem) => {
      val (l1, l2) = accum
      if(f(elem)) (l1 :+ elem, l2) else (l1, l2 :+ elem)
    }
  }
}


intList.listasLocas(_ % 2 == 0)