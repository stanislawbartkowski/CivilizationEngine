package civilization.io.readdir

import scala.io.Source
import java.io.File

object ReadDir {

  // TODO : not always work as expected, failing for Heroku
  private def prev1getDirectory(resourcedir : String) : Seq[String] = Source.fromResource(resourcedir).getLines().toSeq

  private def prevgetDirectory(resourcedir: String): Seq[String] = {
    val fullDirName: String = getClass.getClassLoader.getResource(resourcedir).getPath
    val d = new File(fullDirName)
    d.listFiles().map(_.getName)
  }

  def getDirectory(resourcedir : String) : Seq[String] = {
    ReadDirJava.getResourceListing(getClass,resourcedir)
  }

}
