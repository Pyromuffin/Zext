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

  inline def depth[T]: Int =
    ${depthImpl[T]}

  import scala.quoted.*

  /*
      inline def packageToucher[T](something : T) : List[Any] = {
          ${ packageToucherImpl('something) }
      }

      def packageToucherImpl[T](something : Expr[T])(using Quotes, Type[T]): Expr[List[Any]] = {
          import quotes.reflect.*

          val _package = TypeRepr.of[T].termSymbol.owner
          val expr = _package.tree.asExpr
          val objects = _package.declaredFields
          val objects  = actions(0).tree match{
              case ValDef(x, y, z) => z
          }
          val companion = actionType.get.asExpr
          //val expr = companion.tree.asExpr

          //val exprs = actions.map(a => a.termSymbol.tree.asExpr)
          //val fields : List[Expr[Any]] = zext.declaredFields.map( f => zextTree.select(f).asExpr )
          val names = actions.map( a => Expr(a.name))
          Expr.ofList(List(companion))
      }
  */


  inline def packageToucher[T](something : T) : List[Any] = {
    ${ packageToucherImpl('something) }
  }

  def packageToucherImpl[T](something : Expr[T])(using Quotes, Type[T]): Expr[List[Any]] = {
    import quotes.reflect.*
    // assume something is a top level object
    val _package = TypeRepr.of[T].typeSymbol.owner
    val fields = _package.declaredFields
    val packageExpr = _package.tree.asExpr //  assertion failed: Cannot get tree of package symbol
    val packageTree = packageExpr.asTerm
    val fieldExprs : List[Expr[Any]] = fields.map( f => packageTree.select(f).asExpr)

    Expr.ofList(fieldExprs)
  }



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


def depthImpl[T](using quotes: Quotes, tpe: Type[T]): Expr[Int] = {
  import quotes.reflect.*

  val tpe = TypeRepr.of[T]

  val zextSym = TypeRepr.of[ZextObject].typeSymbol
  val bases = tpe.baseClasses.filterNot( c => c.flags.is(Flags.Trait) )
  var depth = 0

  val str = bases.map(_.fullName).reduce(_ + " " + _)
  println(str)


  for(i <- 0 until bases.size){
    val base = bases(i)
    if(base == zextSym)
      return Expr(depth)
    depth += 1

  }

  Expr(depth)

}

