trait Publisher {

  private var subscribers: Set[Subscriber] = Set()

  def subscribe(subscriber: Subscriber): Unit = subscribers += subscriber

  def unsubscribe(subscriber: Subscriber): Unit = subscribers -= subscriber

  def publish(): Unit = subscribers.foreach(_.handler(this))

}

trait Subscriber {

  def handler(pub: Publisher)

}

class BankAccount extends Publisher {

  private var balance = 0

  def currentBalance: Int = balance

  def deposit(amount: Int): Unit = {
    if (amount > 0) {
      balance = balance + amount
      publish()
    }
  }

  def withdraw(amount: Int): Unit = {
    if (0 < amount && amount <= balance) {
      balance = balance - amount
      publish()
    } else throw new Exception("Not")
  }

}

class Consolidator(observed: List[BankAccount]) extends Subscriber {

  observed.foreach(_.subscribe(this))

  private var total: Int = _
  compute()

  private def compute(): Unit = total = observed.map(_.currentBalance).sum

  override def handler(pub: Publisher): Unit = compute()

  def totalBalance: Int = total

}


val bankA = new BankAccount
val bankB = new BankAccount
val consolidator = new Consolidator(List(bankA, bankB))

consolidator.totalBalance
bankA.deposit(20)
consolidator.totalBalance

