package mustache.fixtures

class Simple extends Mustache {
  def name = "Luke"
  def value = 10000
  def taxedValue = value - (value * 0.4)
  def inCa = true
}
