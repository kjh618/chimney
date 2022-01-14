package io.scalaland.chimney.internal

import io.scalaland.chimney.dsl.TransformerDefinition

object InlineTransformer {
  inline def inlineTransform[From, To](inline src: From): To = {
    ${ TransformerMacros.genTransformBody[From, To, EmptyTuple]('{ TransformerDefinition(Map.empty) }, 'src) }
  }
}
