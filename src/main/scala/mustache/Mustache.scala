package mustache

object Mustache {
  def render(template: String) = template
  def render(template: String, view: Map[String, String]): String = {
    var replacedTemplate = template
    view.foreach { entry =>
      replacedTemplate = replacedTemplate.replaceAll("\\{\\{" + entry._1 + "\\}\\}", entry._2)
    }
    return replacedTemplate
  }
}
