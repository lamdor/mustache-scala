package mustache.elements

import binding._

abstract class Element {
  def apply(binding: Binding): String
}

case class TextElement(body: String) extends Element {
  def apply(binding: Binding): String = body
}

case class ValueElement(name: String) extends Element {
  def apply(binding: Binding): String = binding.stringValueFor(name)
}

case class EnumerateElement(name: String, innerElements: List[Element]) extends Element {
  def apply(binding: Binding): String = ""
}

case class BooleanElement(name: String, innerElements: List[Element]) extends Element {
  def apply(binding: Binding): String = if (binding.booleanValueFor(name)) {
    innerElements.map(ele => ele(binding)).mkString
  } else {
    ""
  }
}

