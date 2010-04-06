package mustache.template

import binding._
import elements._
import scala.util.parsing.combinator._

case class Template(elements: List[Element]) {
  def apply(binding: Binding) =
    elements.foldLeft("") { (str, ele) => str + ele(binding) }
}

case class TemplateParsingException(message: String) extends Exception

object TemplateParser extends RegexParsers {
  val BooleanRegex = """(?s)\{\{#(.+)\}\}(.*)\{\{/\1\}\}""".r
  val EnumerateRegex = """(?s)\{\{(.+)\}\}(.+)\{\{/\1\}\}""".r

  override def skipWhitespace = false

  def name = "\\w+".r

  def textExpr: Parser[Element] = "[^({{)(}})]+".r ^^ { text => TextElement(text) }
  def valueExpr: Parser[Element] = """\{\{\s*""".r ~> name <~ """\s*\}\}""".r ^^ { name => ValueElement(name)}
  def booleanExpr: Parser[Element] = regex(BooleanRegex) ^^ {
    case BooleanRegex(name, body) => BooleanElement(name, parseAll(elements, body.trim).get)
  }
  def enumerateExpr: Parser[Element] = regex(EnumerateRegex) ^^ {
    case EnumerateRegex(name, body) => EnumerateElement(name, parseAll(elements, body).get)
  }

  def elements = rep(booleanExpr | valueExpr | textExpr)

  def parse(text: String) = parseAll(elements, text) match {
    case Success(elements, _) => Template(elements)
    case Failure(msg, _) => throw TemplateParsingException(msg)
    case Error(msg, _) => throw TemplateParsingException(msg)
    case x => throw TemplateParsingException("unknown parsing exception " + x)
  }
}

