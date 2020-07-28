package typeclasses

object Main extends App {

  def toJson[T: Jsonable](t: T)(implicit jsonable: Jsonable[T]): String = jsonable.asJson(t).stringify

  println(toJson(List(1,2,3)))
  println(toJson(Person("Pepito", 23, List(Dog("scooby")), Some(100))))
  println(toJson(Dog("asd")))

}

// Model
case class Person(name: String, age: Int, dogs: List[Dog], eggs: Option[Int])

object Person {

  implicit def personAsJson: Jsonable[Person] = (p: Person) => p.toJValue

  implicit class RichPerson(person: Person) {
    def toJValue = JObject(Map("name" -> JString(person.name),
                               "age" -> JInt(person.age),
                               "dogs" -> JArray(person.dogs.map(_.toJValue))) ++ person.eggs.map(e => "eggs" -> JInt(e)))
  }

}

case class Dog(name: String)

object Dog {

  implicit def dogAsJson: Jsonable[Dog] = (d: Dog) => d.toJValue

  implicit class RichDog(dog: Dog) {
    def toJValue = JObject(Map("name" -> JString(dog.name)))
  }

}

case class Item(name: String, isNew: Boolean, quantity: Int)

trait Jsonable[T] {
  def asJson(t: T): JValue
}

object Jsonable {

  implicit val objectAsJson: Jsonable[Map[String, JValue]] = (t: Map[String, JValue]) => JObject(t)

  implicit val stringAsJString: Jsonable[String] = (t: String) => JString(t)

  implicit val intAsJInt: Jsonable[Int] = (t: Int) => JInt(t)

  implicit val booleanAsJBoolean: Jsonable[Boolean] = (t: Boolean) => JBoolean(t)

  implicit def arrayAsJArray[A](implicit jsonable: Jsonable[A]): Jsonable[List[A]] = (l: List[A]) => JArray(l.map(jsonable.asJson))

  implicit val nullAsJNull: Jsonable[Null] = (_: Null) => JNull()

}