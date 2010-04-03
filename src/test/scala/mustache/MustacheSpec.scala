package mustache

import fixtures._

import org.specs._

class MustacheSpec extends Specification {
  "Mustache object" >> {
    "render with a plain string template" in {
      Mustache.render("hello") must equalIgnoreSpace ("hello")
    }

    "render a simple string template with a map" in {
      val view = Map("planet" -> "World!")
      Mustache.render("Hello {{planet}}", view) must equalIgnoreSpace ("Hello World!")
    }
  }

  "Mustache class" >> {
    "render a simple example" in {
      val expected = """
<VirtualHost *>
  ServerName example.com
  DocumentRoot /var/www/example.com
  RailsEnv production
</VirtualHost>
      """
      (new Passenger).render must equalIgnoreSpace (expected)
    }

    "render with the default extension of .mustache" in {
      (new DefaultExtension).render must equalIgnoreSpace ("Hello world!")
    }
  }
}
