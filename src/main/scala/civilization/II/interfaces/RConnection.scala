package civilization.II.interfaces

trait RConnection {

  def setConnection(host: String, port: Int, database: Int = 0)

  def setConnection(url: String)

}
