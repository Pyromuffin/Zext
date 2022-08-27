///////////////////////////////////////////

package Game


import Zext.Macros
import Zext.exports.*

import java.io.{BufferedWriter, File, FileWriter}
import scala.annotation.experimental
import scala.quoted.*


// touched
object CodeGen {

  def main(args: Array[String]): Unit =  {

    val code = GenerateForObjectsPackage(Game.JunimoGame)
    println(code)
    val path = java.io.File(args(0))
    //val path = java.io.File("E:\\Users\\pyrom\\Documents\\GitHub\\Zext\\src\\main\\scala\\Game\\Generated.scala")
    println(path)

    val file = BufferedWriter(new FileWriter(path))
    file.write(code)
    file.close()
  }


  inline def GenerateForObjectsPackage[T](thing : T): String = {
    val names = Macros.ObjectNames(thing)
    var s =
      """package Game
        |
        |object allObjects {
        |
        |""".stripMargin

    names.foreach { name =>
      s += "\t" + name + "\n"
    }

    s += "}"

    s
  }
}// death// death// death// death// death// death// death// death// death// death// death