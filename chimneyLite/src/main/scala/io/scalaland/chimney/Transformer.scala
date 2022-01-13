package io.scalaland.chimney

import io.scalaland.chimney.dsl.TransformerDefinition

trait Transformer[From, To] {
  def transform(src: From): To
}

object Transformer {

  def define[From, To]: TransformerDefinition[From, To] =
    TransformerDefinition(Map.empty)

  inline given [From, To]: Transformer[From, To] =
    Transformer.define[From, To].buildTransformer
}
