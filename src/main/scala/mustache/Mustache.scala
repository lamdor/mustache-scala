package mustache

abstract class Binding {
  def stringValueFor(name: String): String
  def booleanValueFor(name: String): Boolean = stringValueFor(name).toBoolean
}

class MapBinding[T](view: Map[String, T]) extends Binding {
  def stringValueFor(name: String): String = view.getOrElse(name, "").toString
}

class ObjectBinding(view: Object) extends Binding {
  def stringValueFor(name: String): String = {
    val method = view.getClass.getMethod(name)
    method.invoke(view).toString
  }
}

object Mustache {
  private val BooleanExpressionRE = """(?s)\{\{\s*#(\w+)\s*\}\}(.*)\{\{\s*\/\1\s*\}\}""".r
  private val ValueExpressionRE = """(?s)\{\{\s*(\w+)\s*\}\}""".r

  def render(template: String) = template

  def render[T](template: String, view: Map[String, T]): String =
    render(template, new MapBinding(view))

  def render(template: String, backingObject: Object): String =
    render(template, new ObjectBinding(backingObject))

  def render(template: String, binding: Binding): String = {
    (BooleanExpressionRE findFirstIn template) match {
      case Some(expr @ BooleanExpressionRE(name, body)) => {
        if (binding.booleanValueFor(name)) {
          return render(template.replace(expr, render(body.trim, binding)), binding)
        } else {
          return render(template.replace(expr, ""), binding)
        }
      }
      case None =>
    }

    (ValueExpressionRE findFirstIn template) match {
      case Some(expr @ ValueExpressionRE(name)) => {
        return render(template.replace(expr, binding.stringValueFor(name)), binding)
      }
      case None => template
    }

  }
}

abstract class Mustache {
  import scala.io.Source

  protected var templateExtension: String = "mustache"

  def template: String = {
    val templateLocation = getClass.getName.replaceAll("\\.", "/") + "." + templateExtension
    val templateURL = getClass.getResource("/" + templateLocation)
    val templateSource = Source.fromURL(templateURL)
    templateSource.getLines.mkString
  }

  def render(): String = {
    Mustache.render(template, this)
  }
}
