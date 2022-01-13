package io.scalaland.chimney.internal.dsl

import io.scalaland.chimney.Transformer
import io.scalaland.chimney.dsl.TransformerDefinition
import io.scalaland.chimney.internal.TransformerMacros
import io.scalaland.chimney.internal.utils.MacroUtils

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
          ${ TransformerMacros.genTransformBody[From, To]('src) }
      }
    }
}
