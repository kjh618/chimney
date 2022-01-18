package io.scalaland.chimney.internal.dsl

import io.scalaland.chimney.Transformer
import io.scalaland.chimney.dsl.TransformerDefinition
import io.scalaland.chimney.internal.{TransformerMacros, TransformerOperation}
import io.scalaland.chimney.internal.utils.MacroUtils

import scala.quoted.*

object TransformerDefinitionImpl {

  def withFieldConstImpl[From: Type, To: Type, Operations <: Tuple: Type, T: Type, U: Type](
      definition: Expr[TransformerDefinition[From, To, Operations]],
      selector: Expr[To => T],
      value: Expr[U]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: Tuple]] = {
    import quotes.reflect.*

    val name = MacroUtils.extractNameFromSelector(selector)

    val fieldConstTypeRepr = TypeRepr.of[TransformerOperation.FieldConst]
    val nameConstantTypeRepr = ConstantType(StringConstant(name))

    // "TransformerOperation.FieldConst[$name]"
    fieldConstTypeRepr.appliedTo(nameConstantTypeRepr).asType match {
      case '[fieldConstName] =>
        '{
          TransformerDefinition[From, To, fieldConstName *: Operations](
            $definition.overrides + (${ Expr(name) } -> $value)
          )
        }
    }
  }

  def buildTransformerImpl[From: Type, To: Type, Operations <: Tuple: Type](
      definition: Expr[TransformerDefinition[From, To, Operations]]
  )(using Quotes): Expr[Transformer[From, To]] =
    '{
      new Transformer[From, To] {
        def transform(src: From): To =
          ${ TransformerMacros.genTransformBody[From, To, Operations](definition, 'src) }
      }
    }
}
