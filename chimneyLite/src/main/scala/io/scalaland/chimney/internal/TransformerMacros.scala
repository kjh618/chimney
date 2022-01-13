package io.scalaland.chimney.internal

import io.scalaland.chimney.internal.InlineTransformer

import scala.quoted.*

object TransformerMacros {

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
          val selectSrcField = selectSrcFieldTerm.asExprOf[srcFieldType]
          '{ InlineTransformer.inlineTransform[srcFieldType, DestFieldType]($selectSrcField) }
      }
    }
  }
}
