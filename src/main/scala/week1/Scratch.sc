val f: String => String = { case "ping" => "pong"}

f("ping")

val g: PartialFunction[String, String] = {case "ping" => "pong"}

g.isDefinedAt("pong")
g.isDefinedAt("ping")

g("ping")

val n = 10
val i = 20

for {
  x <- 2 to n
  y <- 2 to i
  if true
} yield (x, y)


