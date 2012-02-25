package net.datapusher.incluse
import java.io.InputStream
import scala.io.Source

trait PolicyParser {
  
  def parse(in: Source): Policy
  
}