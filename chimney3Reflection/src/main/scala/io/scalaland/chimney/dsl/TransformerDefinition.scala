package io.scalaland.chimney.dsl

import io.scalaland.chimney.Transformer
import io.scalaland.chimney.internal.dsl.TransformerDefinitionImpl

case class TransformerDefinition[From, To, TransformerOperations <: Tuple](
    overrides: Map[String, Any]
) {

  transparent inline def withFieldConst[T, U](
      inline selector: To => T,
      value: U
  )(using U <:< T): TransformerDefinition[From, To, ? <: Tuple] =
    ${ TransformerDefinitionImpl.withFieldConstImpl('this, 'selector, 'value) }

  inline def buildTransformer: Transformer[From, To] =
    ${ TransformerDefinitionImpl.buildTransformerImpl('this) }
}
