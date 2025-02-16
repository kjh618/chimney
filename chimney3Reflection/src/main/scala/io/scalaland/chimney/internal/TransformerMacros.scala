package io.scalaland.chimney.internal

import io.scalaland.chimney.Transformer
import io.scalaland.chimney.dsl.TransformerDefinition
import io.scalaland.chimney.internal.InlineTransformer

import scala.deriving.Mirror
import scala.quoted.*

object TransformerMacros {

  def genTransformBody[From: Type, To: Type, Operations <: Tuple: Type](
      definition: Expr[TransformerDefinition[From, To, Operations]],
      src: Expr[From]
  )(using Quotes): Expr[To] = {
    import quotes.reflect.*

    if (Expr.summon[Mirror.ProductOf[To]].isEmpty) {
      ??? // TODO
    }

    val destTypeRepr = TypeRepr.of[To]
    val destTypeSymbol = destTypeRepr.typeSymbol
    val destFieldSymbols = destTypeSymbol.caseFields
    val destFieldNames = destFieldSymbols.map(_.name)
    val destFieldTypeReprs = destFieldSymbols.map(destTypeRepr.memberType)

    val destFieldValues = (destFieldNames zip destFieldTypeReprs).map { (destFieldName, destFieldTypeRepr) =>
      destFieldTypeRepr.asType match {
        case '[destFieldType] =>
          genDestFieldValue[From, To, Operations, destFieldType](definition, src, destFieldName)
      }
    }

    val destConstructorSymbol = destTypeSymbol.primaryConstructor // TODO: Handle noSymbol
    val destFieldValueTerms = destFieldValues.map(_.asTerm)

    // "new $To(${ destFieldValues(0) }, ...)"
    New(Inferred(destTypeRepr)).select(destConstructorSymbol).appliedToArgs(destFieldValueTerms).asExprOf[To]
  }

  def genDestFieldValue[From: Type, To: Type, Operations <: Tuple: Type, DestFieldType: Type](
      definition: Expr[TransformerDefinition[From, To, Operations]],
      src: Expr[From],
      destFieldName: String
  )(using Quotes): Expr[DestFieldType] = {
    import quotes.reflect.*

    genDestFieldValueFromOverrides[From, To, Operations, DestFieldType](
      definition,
      destFieldName
    ) match {
      case Some(destFieldValueFromOverrides) => destFieldValueFromOverrides
      case None =>
        val srcTypeRepr = TypeRepr.of[From]
        val srcFieldSymbols = srcTypeRepr.typeSymbol.caseFields
        val srcFieldTypeReprs = srcFieldSymbols.map(srcTypeRepr.memberType)

        val (srcFieldSymbol, srcFieldTypeRepr) = (srcFieldSymbols zip srcFieldTypeReprs)
          .find((symbol, _) => symbol.name == destFieldName)
          .get // TODO

        // "$src.$srcFieldSymbol"
        val selectSrcFieldTerm = src.asTerm.select(srcFieldSymbol)

        if (srcFieldTypeRepr <:< TypeRepr.of[DestFieldType]) {
          selectSrcFieldTerm.asExprOf[DestFieldType]
        } else {
          srcFieldTypeRepr.asType match {
            case '[srcFieldType] =>
              val selectSrcField = selectSrcFieldTerm.asExprOf[srcFieldType]
              Expr.summon[Transformer[srcFieldType, DestFieldType]] match {
                case Some(t) => '{ $t.transform($selectSrcField) }
                case None    => '{ InlineTransformer.inlineTransform[srcFieldType, DestFieldType]($selectSrcField) }
              }
          }
        }
    }
  }

  def genDestFieldValueFromOverrides[
      From: Type,
      To: Type,
      Operations <: Tuple: Type,
      DestFieldType: Type
  ](
      definition: Expr[TransformerDefinition[From, To, ? <: Tuple]],
      destFieldName: String
  )(using Quotes): Option[Expr[DestFieldType]] =
    Type.of[Operations] match {
      case '[TransformerOperation.FieldConst[name] *: _] if Type.valueOfConstant[name].get == destFieldName => // TODO
        Some('{ $definition.overrides(${ Expr(destFieldName) }).asInstanceOf[DestFieldType] })
      case '[_ *: tail] =>
        genDestFieldValueFromOverrides[From, To, tail, DestFieldType](definition, destFieldName)
      case '[EmptyTuple] => None
    }
}
