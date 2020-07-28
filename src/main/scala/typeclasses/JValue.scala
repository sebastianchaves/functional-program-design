package typeclasses

sealed trait JValue {
  def stringify: String
}

case class JObject(fields: Map[String, JValue]) extends JValue {
  def stringify: String = "{" + fields.map { case(name, value) => s""""$name":${value.stringify}""" }.mkString(",") + "}"
}

case class JString(value: String) extends JValue {
  def stringify: String = s""""$value""""
}

case class JInt(value: Int) extends JValue {
  def stringify: String = value.toString
}

case class JBoolean(value: Boolean) extends JValue {
  def stringify: String = value.toString
}

case class JArray(elems: List[JValue]) extends JValue {
  def stringify: String = "[" + elems.map(_.stringify).mkString(",") + "]"
}

case class JNull() extends JValue {
  def stringify: String = "null"
}

case class JOption(value: Option[JValue]) extends JValue {
  def stringify: String = value match {
    case None => ""
    case Some(value) => value.stringify
  }
}
