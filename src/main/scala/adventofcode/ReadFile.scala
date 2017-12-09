package adventofcode

import java.nio.file.{Files, Paths}

import scala.collection.JavaConversions._


object ReadFile {
  def get_input_lines(clazz: Class[_], filename: String): List[String] =
    Files.readAllLines(Paths.get(clazz.getResource(filename).toURI)).toList

}
