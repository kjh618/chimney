package io.scalaland.chimney.internal.utils

import scala.quoted._

object MacroUtils {

  def extractNameFromSelectorImpl[To: Type, T: Type](code: Expr[To => T])(using Quotes): Expr[String] =
    import quotes.reflect.*
    code.asTerm match
      case InlinedLambda(List(ValDef(identVal, _, _)), t @ Select(Ident(identExtract), name))
          if identVal == identExtract =>
        Expr(name)
      case t => report.throwError(s"Illegal selector: ${normalizeLambdaMessage(code.show)}")

  object InlinedLambda:
    def unapply(using Quotes)(arg: quotes.reflect.Term): Option[(List[quotes.reflect.ValDef], quotes.reflect.Term)] =
      import quotes.reflect.*
      arg match
        case Inlined(_, _, Lambda(vals, term)) => Some((vals, term))
        case Inlined(_, _, nested)             => InlinedLambda.unapply(nested)
        case t                                 => None
  end InlinedLambda

  private def normalizeLambdaMessage(lambdaShow: String): String =
    lambdaShow.replaceAll("""_\$\d+""", "x")
  end normalizeLambdaMessage

  extension [T](inline x: T) inline def inspect(): T = ${ inspectImpl('x) }

  private def inspectImpl[T](x: Expr[T])(using Quotes): Expr[T] = {
    println(x.show)
    x
  }
}
