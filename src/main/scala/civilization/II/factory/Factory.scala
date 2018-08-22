package civilization.II.factory

import com.google.inject.{Guice, PrivateModule}
import net.codingwell.scalaguice.ScalaPrivateModule
import net.codingwell.scalaguice.InjectorExtensions._
import civilization.II.interfaces.{IC, RAccess, ICache}
import civilization.I.II
import civilization.R
import civilization.helper.util.{Cache}

object Factory {

  private val injector = Guice.createInjector(new MyPrivateModule)

  def getI : IC = injector.instance[IC]

  def getR : RAccess = injector.instance[RAccess]

  def getIC : ICache = injector.instance[ICache]

  private class MyPrivateModule extends PrivateModule with ScalaPrivateModule {
    def configure(): Unit = {
      bind[IC].to[II]
      expose[IC]
      bind[RAccess].to[R.R]
      expose[RAccess]
      // cache class as Singleton
      bind[ICache].to[Cache].asEagerSingleton()
      expose[ICache]
    }
  }

}
