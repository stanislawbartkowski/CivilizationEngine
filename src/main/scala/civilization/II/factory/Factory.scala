package civilization.II.factory

import com.google.inject.{Guice, PrivateModule}
import net.codingwell.scalaguice.ScalaPrivateModule
import net.codingwell.scalaguice.InjectorExtensions._
import civilization.II.interfaces.{IC, RAccess}
import civilization.I.II
import civilization.R

object Factory {

  private val injector = Guice.createInjector(new MyPrivateModule)

  def getI : IC = injector.instance[IC]

  def getR : RAccess = injector.instance[RAccess]

  private class MyPrivateModule extends PrivateModule with ScalaPrivateModule {
    def configure(): Unit = {
      bind[IC].to[II]
      expose[IC]
      bind[RAccess].to[R.R]
      expose[RAccess]
    }
  }

}
