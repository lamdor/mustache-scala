package mustache.fixtures

import java.util.Date

class Passenger extends Mustache {
  templateExtension = "conf"

  def server = "example.com"
  def deployTo = "/var/www/example.com"
  def stage = "production"
  def timestamp = new Date().toString
}
