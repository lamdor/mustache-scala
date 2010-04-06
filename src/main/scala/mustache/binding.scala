package mustache.binding

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
