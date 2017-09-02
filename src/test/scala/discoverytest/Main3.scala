package discoverytest

import scala.reflect.io.File

object Main3 {

  def main(args: Array[String]): Unit = {
    val o: String = getClass().getClassLoader.getResource(".").toURI.getPath
    val f: File = File(o)
    f.toDirectory.list.foreach(println)
    println(o)
  }
}
