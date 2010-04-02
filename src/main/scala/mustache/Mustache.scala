package mustache

object Mustache {
  private val ValueExpressionRE = """(?s)\{\{(\w+)\}\}""".r

  def render(template: String) = template

  def render(template: String, view: Map[String, String]): String = {
    var replacedTemplate = template
    view.foreach { entry =>
      replacedTemplate = replacedTemplate.replaceAll("\\{\\{" + entry._1 + "\\}\\}", entry._2)
    }
    return replacedTemplate
  }

  def render(template: String, backingObject: Object): String = {
    (ValueExpressionRE findFirstIn template) match {
      case Some(expr @ ValueExpressionRE(name)) => {
        val value = backingObject.getClass.getMethod(name).invoke(backingObject)
        render(template.replace(expr, value.toString), backingObject)
      }
      case None => template
    }
  }
}

abstract class Mustache {
  import scala.io.Source

  protected var templateExtension: String = _

  def render(): String = {
    val templateLocation = getClass.getName.replaceAll("\\.", "/") + "." + templateExtension
    val templateURL = getClass.getResource("/" + templateLocation)
    val templateSource = Source.fromURL(templateURL)
    val template = templateSource.getLines.mkString
    Mustache.render(template, this)
  }
}
