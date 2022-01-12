package io.scalaland.chimney.internal.dsl

import io.scalaland.chimney.Transformer
import io.scalaland.chimney.dsl._
import io.scalaland.chimney.internal.utils.MacroUtils

import scala.deriving.Mirror
import scala.quoted.*
import scala.compiletime.{erasedValue, constValue, summonInline}

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
      transformerDefinition: Expr[TransformerDefinition[From, To]],
      destMirror: Expr[Mirror.ProductOf[To]]
  )(using Quotes): Expr[Transformer[From, To]] =
    '{
      new Transformer[From, To] {
        def transform(src: From): To =
          getDest(src, $destMirror)
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
        getDestFieldValue[From, typeHead](src, constValue[nameHead]) *:
          getDestFieldValues[From, nameTail, typeTail](src)
      case _: (EmptyTuple, EmptyTuple) => EmptyTuple
    }

  inline def getDestFieldValue[From, DestFieldType](inline src: From, inline destFieldName: Any): DestFieldType =
    ${ getDestFieldValueImpl[From, DestFieldType]('src, 'destFieldName) }

  def getDestFieldValueImpl[From: Type, DestFieldType: Type](
      src: Expr[From],
      destFieldName: Expr[Any]
  )(using Quotes): Expr[DestFieldType] = {
    import quotes.reflect.*
    // TODO: https://dotty.epfl.ch/api/scala/quoted/Quotes$reflectModule$SymbolMethods.html
    val srcFields = TypeTree.of[From].symbol.caseFields
    val srcFieldTypes = srcFields.map(TypeRepr.of[From].memberType)

    val (srcField, srcFieldType) = (srcFields zip srcFieldTypes)
      .find((srcField, srcFieldType) => srcField.name == destFieldName.asExprOf[String].valueOrError)
      .get // TODO

    val selectSrcField = Select(src.asTerm, srcField)
    if (srcFieldType <:< TypeRepr.of[DestFieldType]) {
      selectSrcField.asExprOf[DestFieldType]
    } else {
      val destFieldMirror = Expr.summon[Mirror.ProductOf[DestFieldType]].get // TODO
      srcFieldType.asType match {
        case '[t] =>
          '{
            val transformer = Transformer.define[t, DestFieldType].buildTransformer(using $destFieldMirror)
            transformer.transform(${ selectSrcField.asExprOf[t] })
//            ${ selectSrcField.asExprOf[t] }.transformInto[DestFieldType]
          }
      }
    }
  }
}
