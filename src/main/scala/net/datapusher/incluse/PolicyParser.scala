package net.datapusher.incluse

import scala.io.Source

/**
 * Something that is able to parse some input and turn it into
 * a Policy.
 */
trait PolicyParser {
  
  def parse(in: Source): Policy
  
}
