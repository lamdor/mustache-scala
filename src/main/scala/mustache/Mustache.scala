package mustache

abstract class Binding {
  def stringValueFor(name: String): String
}

class MapBinding[T](view: Map[String, T]) extends Binding {
  def stringValueFor(name: String): String = view.getOrElse(name, "").toString
}

class ObjectBinding(obj: Object) extends Binding {
  def stringValueFor(name: String): String = {
    val method = obj.getClass.getMethod(name)
    method.invoke(obj).toString
  }
}

object Mustache {
  private val ValueExpressionRE = """(?s)\{\{\s*(\w+)\s*\}\}""".r

  def render(template: String) = template

  def render[T](template: String, view: Map[String, T]): String =
    render(template, new MapBinding(view))

  def render(template: String, backingObject: Object): String =
    render(template, new ObjectBinding(backingObject))

  def render(template: String, binding: Binding): String = {
    (ValueExpressionRE findFirstIn template) match {
      case Some(expr @ ValueExpressionRE(name)) => {
        render(template.replace(expr, binding.stringValueFor(name)), binding)
      }
      case None => template
    }
  }
}

abstract class Mustache {
  import scala.io.Source

  protected var templateExtension: String = _

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
