package mustache

import org.specs._

class MustacheSpec extends Specification {
  "render with a plain string template" in {
    val output = Mustache.render("hello")
    output must_== "hello"
  }

  "render a simple string template with a map" in {
    val view = Map("planet" -> "World!")
    val output = Mustache.render("Hello {{planet}}", view)
    output must_== "Hello World!"
  }
}
