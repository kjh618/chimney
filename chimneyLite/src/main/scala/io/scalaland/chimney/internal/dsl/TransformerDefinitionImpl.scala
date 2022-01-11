package io.scalaland.chimney.internal.dsl

import io.scalaland.chimney.Transformer
import io.scalaland.chimney.dsl.TransformerDefinition
import io.scalaland.chimney.internal.utils.MacroUtils

import scala.deriving.Mirror
import scala.quoted.*
import scala.compiletime.{summonAll, summonInline, erasedValue, constValue}

object TransformerDefinitionImpl {

  def withFieldConstImpl[From, To, T, U](
      transformerDefinition: Expr[TransformerDefinition[From, To]]
  )(
      selector: Expr[To => T],
      value: Expr[U]
  )(using Type[From], Type[To], Type[T], Type[U], Quotes): Expr[TransformerDefinition[From, To]] = {
    val name = MacroUtils.extractNameFromSelectorImpl(selector)
    '{ TransformerDefinition($transformerDefinition.newFieldValues + ($name -> $value)) }
  }

  def buildTransformerImpl[From, To](
      transformerDefinition: Expr[TransformerDefinition[From, To]]
  )(using Quotes, Type[From], Type[To]): Expr[Transformer[From, To]] = {
    (Expr.summon[Mirror.ProductOf[From]], Expr.summon[Mirror.ProductOf[To]]) match {
      case (Some(srcMirror), Some(destMirror)) => ???
      case _                                   => ???
    }
  }

  inline def getValueForDestField[From, T](destFieldName: String, inline src: From): T =
    ${ getValueForDestFieldImpl[From, T]('destFieldName, 'src) }

  def getValueForDestFieldImpl[From, T](destFieldName: Expr[String], src: Expr[From])(using
      Quotes,
      Type[From],
      Type[T]
  ): Expr[T] = {
    import quotes.reflect.*
    val srcFields = TypeTree.of[From].symbol.caseFields
    println(srcFields)
    val srcField = srcFields.find(_.name == destFieldName.valueOrError).get
    Select(src.asTerm, srcField).asExprOf[T]
  }
}
