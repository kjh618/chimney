package io.scalaland.chimney.internal.utils

import scala.quoted.*

object MacroUtils {

  // TODO
  def extractNameFromSelector[To: Type, T: Type](selector: Expr[To => T])(using Quotes): String = {
    import quotes.reflect.*
    selector.asTerm match
      case InlinedLambda(List(ValDef(identVal, _, _)), t @ Select(Ident(identExtract), name))
          if identVal == identExtract =>
        name
      case t => report.throwError(s"Illegal selector: ${normalizeLambdaMessage(selector.show)}")
  }

  object InlinedLambda {
    def unapply(using Quotes)(arg: quotes.reflect.Term): Option[(List[quotes.reflect.ValDef], quotes.reflect.Term)] =
      import quotes.reflect.*
      arg match
        case Inlined(_, _, Lambda(vals, term)) => Some((vals, term))
        case Inlined(_, _, nested)             => InlinedLambda.unapply(nested)
        case t                                 => None
  }

  private def normalizeLambdaMessage(lambdaShow: String): String =
    lambdaShow.replaceAll("""_\$\d+""", "x")

  extension [T](inline x: T) inline def inspect(): T = ${ inspectImpl('x) }

  private def inspectImpl[T](x: Expr[T])(using Quotes): Expr[T] = {
    println(x.show)
    x
  }
}
