/*
 * AutoFormat.scala
 * (play-json-sealed)
 *
 * Copyright (c) 2013-2014 Hanns Holger Rutz. All rights reserved.
 *
 * This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 * For further information, please contact Hanns Holger Rutz at
 * contact@sciss.de
 */

package de.sciss.play.json

import play.api.libs.json.{Format, JsError, JsObject, JsResult, JsString, JsSuccess, JsValue}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.experimental.macros
import scala.reflect.macros.Context

object FlatFormat {
  private val DEBUG   = false
  private val PACKAGE = false

  def apply[A]: Format[A] = macro applyImpl[A]

  private def log(what: => String): Unit = println(s"<sealed> $what")

  def applyImpl[A: c.WeakTypeTag](c: Context): c.Expr[Format[A]] = {
    import c.universe._
    val aTpeW   = c.weakTypeOf[A]
    val aClazz  = aTpeW.typeSymbol.asClass

    def id(s: String) = Ident(newTermName(s))

    val aIdent  = Ident(aClazz)

    require(aClazz.isSealed, s"Type $aTpeW is not sealed")
    aClazz.typeSignature  // SI-7046 !
    val subs    = aClazz.knownDirectSubclasses
    require(subs.nonEmpty  , s"Type $aTpeW does not have known direct subclasses")

    if (DEBUG) {
      log(s"Known direct subclasses of $aTpeW")
      subs.foreach(s => log(s.toString))
      log("")
    }

    val subsAll = subs.toList.sortBy(_.name.toString)
    val knownValues = subsAll.toList.map { _.name.toString }.mkString(",")
    // for each sub type a tuple of writer-case-body, (bool, reader-case-body) where `bool` is true
    // if the sub type is a singleton object and false if it is a case class
    val cases   = subsAll.map { sub =>
      val subC      = sub.asClass
      val isObject  = subC.isModuleClass
      val subIdent  = Ident(if (isObject) {
        val companion = subC.companionSymbol
        require(companion.isModule, s"Sub type $subC of $aTpeW does not have a companion object")
        companion.asModule
      } else subC)
      val patWrite  = Bind(newTermName("x"), Typed(id("_"), subIdent))
      val subName0  = if (PACKAGE) sub.fullName else sub.name.toString
      val subName   = Literal(Constant(subName0))
      val patRead   = subName
      val (bodyWrite, bodyRead) = if (isObject) {
        val jsSuccessTree = Ident(typeOf[JsSuccess.type].typeSymbol.asClass.companionSymbol.asModule) // frickin' hell
        Apply(id("writeObject"), subName :: Nil) ->
        Apply(TypeApply(jsSuccessTree, aIdent :: Nil), subIdent :: Nil) // JsSuccess[A](MyCaseObject)

      } else {
        // val jsonTree  = Ident(typeOf[Json.type].typeSymbol.asClass)
        // val subFmt    = TypeApply(Select(jsonTree, "format"), subIdent :: Nil)
        val jsonTree  = Ident(typeOf[AutoFormat.type].typeSymbol.asClass)
        val subFmt    = TypeApply(Select(jsonTree, newTermName("apply")), subIdent :: Nil)
        Apply(TypeApply(id("writeClass"), subIdent:: Nil), subName :: id("x") :: subFmt :: Nil) ->
        Apply(Select(subFmt, newTermName("reads")), id("data") :: Nil)
      }
      CaseDef(pat = patWrite, guard = EmptyTree, body = bodyWrite) ->
        (isObject, CaseDef(pat = patRead, guard = EmptyTree, body = bodyRead))
    }
    val (casesWrite, casesRead) = cases.unzip
    val matchWrite      = Match(id("value"), casesWrite)
    val matchWriteExpr  = c.Expr[JsValue](matchWrite)
    val casesReadO      = casesRead collect { case (true, tree) => tree }
    val matchReadO      = Match(id("name"), casesReadO) // XXX TODO add catch all
    val matchReadExprO  = c.Expr[JsResult[A]](matchReadO)
    val validSubs       = c.Expr[JsError](Literal(Constant(knownValues)))
    val r               = reify {
      new Format[A] {
        private def writeObject(name: String): JsValue = JsString(name)

        def writes(value: A): JsValue = matchWriteExpr.splice

        def reads(json: JsValue): JsResult[A] = try {
          (json: @unchecked) match {
            // this crashes upon macro expansion (probably a bug)
            // case JsObject(Seq(("class", JsString(name)), rest @ _*)) => ...
            case JsString(name) =>
              matchReadExprO.splice
          }
        } catch {
          case e: MatchError => JsError(
            "Unable to parse " + json.toString
            + " known values are " + validSubs.splice
            + " " + e.toString
          )
        }
      }
    }
    val t = r.tree

    if (DEBUG) {
      log("Tree:")
      println(t)
    }
    c.Expr[Format[A]](t)
  }
}
