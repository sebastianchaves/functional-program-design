import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

val g: PartialFunction[Int, String] = {
  case 1 => "ok"
}

val sum1: PartialFunction[Int, Int] = {
  case x if x < 5 => x + 1
}

val sum0: PartialFunction[Int, Int] = {
  case x if x >= 5 => x + 0
}

(1 to 10) map sum1.orElse(sum0)

val future1 = Future {
  Thread.sleep(1)
  1 + 1
}

val future2 = Future {
  Thread.sleep(1)
  2 + 2
}

def f(x: Int): Future[Int] = future2

future1.flatMap(f)


















































