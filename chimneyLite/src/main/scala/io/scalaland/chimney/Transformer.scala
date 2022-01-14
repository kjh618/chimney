package io.scalaland.chimney

import io.scalaland.chimney.dsl.TransformerDefinition

trait Transformer[From, To] {
  def transform(src: From): To
}

object Transformer {

  def define[From, To]: TransformerDefinition[From, To, EmptyTuple] =
    TransformerDefinition(Map.empty)

  inline given transformer[From, To]: Transformer[From, To] =
    Transformer.define[From, To].buildTransformer
}
