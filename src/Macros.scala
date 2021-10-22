package Zext

import scala.quoted.*


inline def assert(inline expr: Boolean): Unit =
    ${ assertImpl('expr) }

def assertImpl(expr: Expr[Boolean])(using Quotes) = '{
if !$expr then
    throw AssertionError(s"failed assertion: ${${ showExpr(expr) }}")
}

def showExpr[T](expr: Expr[T])(using Quotes): Expr[String] =
    val code: String = expr.show
    Expr(code)


inline def GetClass(inline expr: Boolean): Unit =
        ${ assertImpl('expr) }

object Macros{
    inline def Zebra[T](): String = ${
        ZebraCode[T]()(using Type.of[T])
    }

    inline def fullClassName[T]: String =
        ${ fullClassNameImpl[T] }


    import scala.quoted.*

    inline def variableName : String = {
        ${ variableNameImpl }
    }

    inline def varvarName : String = {
        ${ varvarNameImpl }
    }

    def varvarNameImpl(using Quotes): Expr[String] = {
        import quotes.reflect.*
        val callee = Symbol.spliceOwner.owner.owner
        Expr(callee.name)
    }


    def variableNameImpl(using Quotes): Expr[String] = {
        import quotes.reflect.*
        val callee = Symbol.spliceOwner.owner
        Expr(callee.name)
    }
}


def ZebraCode[T]()(using Type[T], Quotes): Expr[String] = {
    Expr(Type.show[T])
}



def fullClassNameImpl[T](using quotes: Quotes, tpe: Type[T]): Expr[String] =
    import quotes.reflect.*
    Expr(TypeTree.of[T].symbol.fullName)



