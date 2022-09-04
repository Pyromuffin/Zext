package test

import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.printing.*
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}
import dotty.tools.dotc.transform.{Erasure, Inlining, Pickler, YCheckPositions}
import dotty.tools.dotc.typer.TyperPhase
import dotty.tools.dotc.util.Spans.NoCoord

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


class Plugin extends StandardPlugin{
  val name: String = "Objectifier"
  override val description: String = "objects are now less lazy"

  def init(options: List[String]): List[PluginPhase] =
    List(new CollectObjects)
}


class CollectObjects extends PluginPhase {

  import tpd.*

  val phaseName = "CollectObjects"
  override val runsAfter = Set(TyperPhase.name)


  override def transformPackageDef(tree: PackageDef)(using ctx : Context): Tree = {


   // println("object count :" + all.size)
   // all.foreach(println)
    /*
    var idx = 0
    for(s <- tree.stats){
      println(idx)
      println(s.show)
      println(s.getClass)
      println(s.symbol.owner)
      println(s.symbol.flags.flagsString)
      idx += 1
    }
*/
    //println("tree symbol " + tree.symbol)

    val objects = ArrayBuffer[Tree]()

    tree.foreachSubTree{
      case vd : ValDef =>{
        if(vd.symbol.flags.isAllOf(Module | StableRealizable | Lazy | Final) && !vd.symbol.flags.isOneOf(Synthetic | Deferred | Private | Accessor)){
          objects.addOne(ref(vd.symbol))
        }
      }
      case _ =>
    }


    val prefix = ctx.compilationUnit.source.name.stripSuffix(".scala")
    val secretClass = newNormalizedClassSymbol(tree.symbol.moduleClass, typeName(prefix + "$SecretHolder"), FlagSet(0), defn.ObjectType :: Nil).entered
    val constructorSymbol = newConstructor(secretClass, Synthetic, Nil, Nil).entered
    val constructorDef = DefDef(constructorSymbol.asTerm)

    val secretMethodSymbol = newSymbol(secretClass, termName("Reveal"), JavaStatic | Method, defn.UnitType).entered
    val body : Tree = Block(objects.toList, Literal(Constant(())))
    val secretMethodDef = DefDef(secretMethodSymbol.asTerm, body)

    val secretClassDef = ClassDef(secretClass, constructorDef, secretMethodDef :: Nil)
    val newPackageDef = cpy.PackageDef(tree)(pid = tree.pid, stats = tree.stats ::: secretClassDef :: Nil)


    //println(secretMethodDef.show)

    //println("package " + newPackageDef.show)

    newPackageDef
  }

  /*

override def transformTypeDef(tree: TypeDef)(using ctx: Context): Tree = {

  if (!tree.symbol.isPackageObject)
    return tree

 // println(tree.symbol)
  //println(tree.show)

  val newsym = newSymbol(tree.symbol, termName("Secret"), FlagSet(0), defn.IntType).entered
  val secret = ValDef(newsym.asTerm, Literal(Constant(100)))

  val template = tree.rhs.asInstanceOf[Template]
  val newBody = template.body.appended(secret)
  val newTemplate = cpy.Template(template)(body = newBody)


  val newTree =  cpy.TypeDef(tree)(rhs = newTemplate)


  newTree
}


  override def transformDefDef(tree: DefDef)(using ctx: Context): Tree = {


    tree.setDefTree
    val sym = tree.symbol

    if tree.name != termName("Touch") then
      return tree

 //   println(tree.show)
  //  println(AllObjects.objects)


    // ignore abstract and synthetic methods
    if tree.rhs.isEmpty || sym.isOneOf(Synthetic | Deferred | Private | Accessor | Inline | ConstructorProxyModule)
    then return tree

    try {
     // println("\n\n\n\n")
     // println("========================== tree ==========================")
     // println(tree.show)

      // val body = {tree.rhs}

     // val newsym = newSymbol(tree.symbol, termName("body"), Local, tree.rhs.tpe).asTerm

     // val body = ValDef(newsym, Block(Nil, tree.rhs))

//      val bodyRef = ref(body.symbol)
      val all = AllObjects.packages.flatMap( kv =>  kv._2).filterNot(t => t.symbol.fullName.toString.contains("scala"))
      //printRes
      // shove it all together in a block
      val rhs1 = tpd.Block(all.toList, Literal(Constant(null)) )

      //replace RHS with new
      val newDefDef = cpy.DefDef(tree)(rhs = tree)
      println("====================== transformed ======================")
      println(newDefDef.show)
      newDefDef
    } catch {
      case e =>
   //     println("====================== error ===========================")
    //    println(e)
   //     println(e.printStackTrace)
        tree

    }
  }
  */
}







