package foo

import scala.reflect.macros.Context
import language.experimental.macros

object Checker {
  def apply[A]: Unit = macro applyImpl[A]

  def applyImpl[A: c.WeakTypeTag](c: Context): c.Expr[Unit] = {
    val tpe = c.weakTypeOf[A].typeSymbol.asClass
    require (tpe.isSealed, s"Type $tpe is not sealed")
    tpe.typeSignature // SI-7046
    require (tpe.knownDirectSubclasses.nonEmpty, s"Did not find sub classes for type $tpe")

    import c.universe._
    c.Expr[Unit](reify {} .tree)
  }
}