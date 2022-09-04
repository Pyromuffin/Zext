package Zext

import scala.annotation.experimental
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

  inline def fullClassName[T]: String =
    ${ fullClassNameImpl[T] }

  inline def depth[T]: Int =
    ${depthImpl[T]}


  inline def ObjectNames[T](something : T) : List[String] = {
    ${ ObjectNamesImpl('something) }
  }

  inline def packageToucher[T](something : T) : List[Any] = {
    ${ packageToucherImpl('something) }
  }

  @experimental
  def packageToucherImpl[T](something : Expr[T])(using Quotes, Type[T]): Expr[List[Any]] = {
    import quotes.reflect.*
    // assume something is a top level object
    val _package = TypeRepr.of[T].typeSymbol.owner
    //val packageExpr = _package.tree.asExpr //  assertion failed: Cannot get tree of package symbol
    //val packageTree = packageExpr.asTerm
    //val fieldExprs : List[Expr[Any]] = fields.map( f => packageTree.select(f).asExpr)

    // select is "."

    //Expr(fields.map(f => `{f.fullName)}.asExprOf[Any])

    /*
    //val fooDef = DefDef(_package, argss => Some('{println(s"Calling foo")}.asTerm))
    val thing = Some('{println("Calling foo")}.asTerm)
    //_package.tree.show

    println("Package:" + _package)
    println("companion class:" +  _package.companionClass)
    println("companion module:" + _package.companionModule)
    println("module class :" +  _package.moduleClass)

    val valSymbol = Symbol.newVal(_package, "potato", TypeRepr.of[Int], Flags.Module | Flags.Final | Flags.StableRealizable, Symbol.noSymbol)
    val className = "Vegetable"
    val vd = ValDef(valSymbol, Some('{Game.Path}.asTerm) )
    vd.symbol.tree.changeOwner(_package)
    */

    val fields = _package.fieldMembers


    val valdefs = fields.filter(f => f.isValDef && !f.flags.is(Flags.Synthetic) && f.flags.is(Flags.Module) && !f.fullName.contains('$')) // weird incremental $ things added for pain purposes
    //valdefs.foreach(vd => println(vd))
    //valdefs.foreach(vd => println(vd.tree))
    //val valdefsTerms = valdefs.map(_.tree.asInstanceOf[ValDef])
    //valdefsTerms.foreach(vd => println(vd.rhs.map(_.show)))

    val packageTerm = Ident(_package.companionModule.termRef)
    //println(packageTerm)

    //Apply(Select(  New(Ident(Forest)),     <init>),List())
    //ValDef(FarmHouse,Ident(FarmHouse$),Apply(Select(New(Ident(FarmHouse$)),<init>),List()))

     def GetObjectsRecursive(valdef: Symbol): Seq[Expr[Any]] = {

       val correctFlags = Flags.Final | Flags.Lazy | Flags.Module | Flags.StableRealizable
       val childrenObjects = valdef.fieldMembers.filter(f => f.isValDef && (f.flags & correctFlags).is(correctFlags) )// && f.fullName.contains("watering_can"))

       val objectTerm = Ident(valdef.termRef)
       val exprs = childrenObjects.map(objectTerm.select(_).asExpr)

       exprs.concat(childrenObjects.flatMap(GetObjectsRecursive))
    }


    val exprs = valdefs.map(packageTerm.select(_).asExpr).concat( valdefs.flatMap(GetObjectsRecursive) )
    //println(exprs)
    Expr.ofList(exprs)
  }


  def ObjectNamesImpl[T](something : Expr[T])(using Quotes, Type[T]): Expr[List[String]] = {
    import quotes.reflect.*
    // assume something is a top level object
    val _type = TypeRepr.of[T].typeSymbol
    val _package = _type.owner
    val fields = _package.fieldMembers

    //println(_package)

    val valdefs = fields.filter(f => f.isValDef && !f.flags.is(Flags.Synthetic) && !f.fullName.contains('$')) // weird incremental $ things added for pain purposes
    val names = valdefs.map(_.fullName).map(Expr(_))

    Expr.ofList(names)
  }


  inline def GetPackageObject[T](something: T): Any = {
    ${GetPackageObjectImpl('something)}
  }

  @experimental
  def GetPackageObjectImpl[T](something : Expr[T])(using Quotes, Type[T]): Expr[Any] = {
    import quotes.reflect.*
    val _type = TypeRepr.of[T].typeSymbol
    val _package = _type.owner
    val companion = _package.moduleClass

    val classof = Ident(defn.Predef_classOf.termRef)
    val expr = TypeApply(classof, TypeTree.ref(_package) :: Nil)
    println(expr.show)

    expr.asExpr
  }

  def CodePosition() : String = {
    "here"
  //   ${ CodePositionImpl }
  }

  def CodePositionImpl(using Quotes): Expr[String] = {
    import quotes.reflect.*
    //val pos = Symbol.spliceOwner.owner.pos
    val pos = Option(Position.ofMacroExpansion)
    if(pos.isDefined){
      val p = pos.get
      Expr(p.sourceFile.name + ":" + (p.startLine + 1))
    } else {
      Expr("unknown source position")
    }

  }




  inline def variableName : String = {
    ${ variableNameImpl }
  }

  def variableNameImpl(using Quotes): Expr[String] = {
    import quotes.reflect.*
    val callee = Symbol.spliceOwner.owner
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
    val fields = symbol.declaredFields.filter(s => !s.flags.is(Flags.Synthetic) )
    val listExprs : List[Expr[Any]] = fields.map(exprTree.select(_).asExpr)
    Expr.ofList(listExprs)
  }

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


  // val str = bases.map(_.fullName).reduce(_ + " " + _)

  for(i <- 0 until bases.size){
    val base = bases(i)
    if(base == zextSym)
      return Expr(depth)
    depth += 1

  }

  depth = -1

  Expr(depth)

}

