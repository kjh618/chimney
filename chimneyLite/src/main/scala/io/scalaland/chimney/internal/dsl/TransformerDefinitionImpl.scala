package io.scalaland.chimney.internal.dsl

import io.scalaland.chimney.Transformer
import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.inlineTransform
import io.scalaland.chimney.internal.utils.MacroUtils

import scala.compiletime.{constValue, erasedValue, summonInline}
import scala.quoted.*

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
  )(using Quotes): Expr[Transformer[From, To]] =
    '{
      new Transformer[From, To] {
        def transform(src: From): To =
          ${ genTransformBody[From, To]('src) }
      }
    }

  def genTransformBody[From: Type, To: Type](src: Expr[From])(using Quotes): Expr[To] = {
    import quotes.reflect.*

    val destTypeRepr = TypeRepr.of[To]
    val destTypeSymbol = destTypeRepr.typeSymbol
    val destFieldSymbols = destTypeSymbol.caseFields
    val destFieldNames = destFieldSymbols.map(_.name)
    val destFieldTypeReprs = destFieldSymbols.map(destTypeRepr.memberType)

    val destFieldValues = (destFieldNames zip destFieldTypeReprs).map { (destFieldName, destFieldTypeRepr) =>
      destFieldTypeRepr.asType match {
        case '[destFieldType] => genDestFieldValue[From, destFieldType](src, destFieldName)
      }
    }

    val destConstructorSymbol = destTypeSymbol.primaryConstructor // TODO: Handle noSymbol
    val destFieldValueTerms = destFieldValues.map(_.asTerm)

    New(Inferred(destTypeRepr)).select(destConstructorSymbol).appliedToArgs(destFieldValueTerms).asExprOf[To]
  }

  def genDestFieldValue[From: Type, DestFieldType: Type](
      src: Expr[From],
      destFieldName: String
  )(using Quotes): Expr[DestFieldType] = {
    import quotes.reflect.*

    val srcTypeRepr = TypeRepr.of[From]
    val srcFieldSymbols = srcTypeRepr.typeSymbol.caseFields
    val srcFieldTypeReprs = srcFieldSymbols.map(srcTypeRepr.memberType)

    val (srcFieldSymbol, srcFieldTypeRepr) = (srcFieldSymbols zip srcFieldTypeReprs)
      .find((symbol, _) => symbol.name == destFieldName)
      .get // TODO

    val selectSrcFieldTerm = src.asTerm.select(srcFieldSymbol)

    if (srcFieldTypeRepr <:< TypeRepr.of[DestFieldType]) {
      selectSrcFieldTerm.asExprOf[DestFieldType]
    } else { // TODO: Check if a given transformer exists
      srcFieldTypeRepr.asType match {
        case '[srcFieldType] =>
          '{ inlineTransform[srcFieldType, DestFieldType](${ selectSrcFieldTerm.asExprOf[srcFieldType] }) }
      }
    }
  }
}
