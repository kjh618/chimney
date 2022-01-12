package io.scalaland.chimney.internal.dsl

import io.scalaland.chimney.Transformer
import io.scalaland.chimney.dsl.TransformerDefinition
import io.scalaland.chimney.internal.utils.MacroUtils

import scala.deriving.Mirror
import scala.quoted.*
import scala.compiletime.{summonAll, summonInline, erasedValue, constValue}

object TransformerDefinitionImpl {

  def withFieldConstImpl[From: Type, To: Type, T: Type, U: Type](
      transformerDefinition: Expr[TransformerDefinition[From, To]]
  )(
      selector: Expr[To => T],
      value: Expr[U]
  )(using Quotes): Expr[TransformerDefinition[From, To]] = {
    val name = MacroUtils.extractNameFromSelectorImpl(selector)
    '{ TransformerDefinition($transformerDefinition.newFieldValues + ($name -> $value)) }
  }

  def buildTransformerImpl[From: Type, To: Type](
      transformerDefinition: Expr[TransformerDefinition[From, To]]
  )(using Quotes): Expr[Transformer[From, To]] = {
    (Expr.summon[Mirror.ProductOf[From]], Expr.summon[Mirror.ProductOf[To]]) match {
      case (Some(srcMirror), Some(destMirror)) =>
        '{
          new Transformer[From, To] {
            def transform(src: From): To =
              getDest(src, $destMirror)
          }
        }
      case _ => ???
    }
  }

  inline def getDest[From, To](inline src: From, destMirror: Mirror.ProductOf[To]): To = {
    val destFieldValues = getDestFieldValues[From, destMirror.MirroredElemLabels, destMirror.MirroredElemTypes](src)
    destMirror.fromProduct(destFieldValues)
  }

  inline def getDestFieldValues[From, DestFieldNames <: Tuple, DestFieldTypes <: Tuple](
      inline src: From
  ): DestFieldTypes =
    inline (erasedValue[DestFieldNames], erasedValue[DestFieldTypes]) match {
      case _: ((nameHead *: nameTail), (typeHead *: typeTail)) =>
        getDestFieldValue[From, typeHead](constValue[nameHead], src) *:
          getDestFieldValues[From, nameTail, typeTail](src)
      case _: (EmptyTuple, EmptyTuple) => EmptyTuple
    }

  inline def getDestFieldValue[From, T](inline destFieldName: Any, inline src: From): T =
    ${ getDestFieldValueImpl[From, T]('destFieldName, 'src) }

  def getDestFieldValueImpl[From: Type, T: Type](destFieldName: Expr[Any], src: Expr[From])(using Quotes): Expr[T] = {
    import quotes.reflect.*
    // TODO: https://dotty.epfl.ch/api/scala/quoted/Quotes$reflectModule$SymbolMethods.html
    val srcFields = TypeTree.of[From].symbol.caseFields
    val srcField = srcFields.find(_.name == destFieldName.asExprOf[String].valueOrError).get
    Select(src.asTerm, srcField).asExprOf[T]
  }
}
