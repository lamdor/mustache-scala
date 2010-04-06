package mustache

import binding._
import template._

object Mustache {
  def render(template: String) = template

  def render[T](template: String, view: Map[String, T]): String =
    render(template, new MapBinding(view))

  def render(template: String, backingObject: Object): String =
    render(template, new ObjectBinding(backingObject))

  def render(templateString: String, binding: Binding): String = {
    val template = TemplateParser.parse(templateString)
    template(binding)
  }
}

case class TemplateNotFoundException(message: String) extends Exception

abstract class Mustache {
  import scala.io.Source

  protected var templateExtension: String = "mustache"

  lazy val templateLocation = getClass.getName.replaceAll("\\.", "/") + "." + templateExtension

  lazy val template: Option[String] = {
    val templateURL = getClass.getResource("/" + templateLocation)
    try {
      val templateSource = Source.fromURL(templateURL)
      Some(templateSource.getLines.mkString)
    } catch {
      case e: Exception => None
    }
  }

  def render(): String = {
    template match {
      case Some(templateString) => Mustache.render(templateString, this)
      case None => throw TemplateNotFoundException("unable to find template " + templateLocation)
    }
  }
}
