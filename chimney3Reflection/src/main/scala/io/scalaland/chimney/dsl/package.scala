package io.scalaland.chimney.dsl

import io.scalaland.chimney.Transformer

import scala.compiletime.summonFrom

extension [From](src: From) {

  def into[To]: TransformerInto[From, To, EmptyTuple] =
    TransformerInto(src, TransformerDefinition(Map.empty))

  inline def transformInto[To]: To =
    summonFrom {
      case t: Transformer[From, To] => t.transform(src)
      case _                        => Transformer.derive[From, To].transform(src)
    }
}
