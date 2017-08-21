package discoverytest

import scala.io.Source

object Main1 {
  def main(args: Array[String]): Unit = {

    println("Hello")

    val o :Iterator[String] = Source.fromResource("map/tiles").getLines()
    o.foreach(println)
    println(o)
  }

}
