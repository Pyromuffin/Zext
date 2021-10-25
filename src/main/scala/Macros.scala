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



    inline def varvarName : String = {
        ${ varvarNameImpl }
    }


    inline def variableName : String = {
        ${ variableNameImpl }
    }

    def variableNameImpl(using Quotes): Expr[String] = {
        import quotes.reflect.*
        val callee = Symbol.spliceOwner.owner
        Expr(callee.name)
    }

    def varvarNameImpl(using Quotes): Expr[String] = {
        import quotes.reflect.*
        val callee = Symbol.spliceOwner.owner.owner
        Expr(callee.name)
    }


    inline def TouchEveryone[T](something : T) : List[Any] = {
        ${ TouchEveryoneImpl( '{something} ) }
    }


    def TouchEveryoneImpl[T](something : Expr[T])(using Quotes)(using t : Type[T]): Expr[List[Any]] = {
        import quotes.reflect.*

        val exprTree: Term = something.asTerm
        val tpr = TypeRepr.of[T]
        val symbol = tpr.typeSymbol
        val fields = symbol.declaredFields
        val listExprs : List[Expr[Any]] = fields.map(exprTree.select(_).asExpr)
        Expr.ofList(listExprs)
    }

}

def ZebraCode[T]()(using Type[T], Quotes): Expr[String] = {
    Expr(Type.show[T])
}



def fullClassNameImpl[T](using quotes: Quotes, tpe: Type[T]): Expr[String] =
    import quotes.reflect.*
    Expr(TypeTree.of[T].symbol.fullName)



