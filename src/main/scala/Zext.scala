package Zext

import World.*
import Query.Property
import Rule.*
import Direction.*
import Interpreter.*

import scala.collection.mutable.ArrayBuffer
import scala.language.{implicitConversions, postfixOps}


trait Property


object holdable extends Property
object fixed extends Property
object scenery extends Property
object wet extends Property
case class initialDescription(desc : StringExpression) extends Property

trait Container {
    var contents : ArrayBuffer[ZextObject] = ArrayBuffer[ZextObject]()
    var open = true
    var transparent = false
}



object ZextObject {
    val nouns = ArrayBuffer[ZextObject]()
}

class ZextObject {
    var definiteArticle : String = "the"
    var indefiniteArticle : String = "a"
    var name : String = ""
    var aliases = ArrayBuffer[String]()
    var description : StringExpression = ""
    var properties : ArrayBuffer[Property] = ArrayBuffer[Property]()
    var plural = false
    var proper = false

    var parentContainer : Container = null
    ZextObject.nouns += this

    def definite : String = {
        if(proper)
            return name.toString

        definiteArticle + " " + name
    }

    def indefinite : String = {
        if(proper)
            return name.toString

        indefiniteArticle + " " + name
    }

    def be : String = {
        if(plural)
            return "are"
        "is"
    }

    def ?(property: Property) : Boolean =  {
        properties.contains(property)
    }

    def get[P <: Property] = {

    }

    override def toString: String = definite
}


object thing {
    extension(d: StringExpression)  {
        inline def unary_~ : thing = {
            thing() a Macros.variableName desc d
        }
    }
}


class thing extends ZextObject{

    parentContainer = World.currentRoom
    parentContainer.contents += this

    def FixName(s : String): String  ={
        s.replace('_', ' ')
    }

    def SetName(s : String): Unit = {
        val fixed = FixName(s)
        name = fixed
        aliases.addAll(fixed.split(' '))
    }


    def desc(desc : StringExpression)  ={
        description = desc
        this
    }

    def is(prop: Property) = {
        properties += prop
        this
    }

    def are(prop: Property) = {
        properties += prop
        this
    }

    def and(prop: Property) = {
        properties += prop
        this
    }

    def a(name : String) = {
        SetName(name)
        this
    }

    def the(name : String) = {
        SetName(name)
        this
    }

    def some(name : String) = {
        SetName(name)
        plural = true
        indefiniteArticle = "some"
        this
    }


    def mass = {
        plural = true
        indefiniteArticle = "some"
        this
    }


    def named(name : String) = {
        SetName(name)
        proper = true
        this
    }

    def has(prop: Property) = {
        properties += prop
        this
    }

    def aka(s : String) = {
        aliases.addOne(s)
        this
    }
}

class Supporter extends thing with Container


