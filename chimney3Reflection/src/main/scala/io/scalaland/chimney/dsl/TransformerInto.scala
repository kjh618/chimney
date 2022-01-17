package io.scalaland.chimney.dsl

import io.scalaland.chimney.Transformer

import scala.compiletime.summonFrom

case class TransformerInto[From, To, TransformerOperations <: Tuple](
    src: From,
    definition: TransformerDefinition[From, To, TransformerOperations]
) {

  transparent inline def withFieldConst[T, U](
      inline selector: To => T,
      value: U
  )(using U <:< T): TransformerInto[From, To, ? <: Tuple] =
    withDefinition(definition.withFieldConst(selector, value))

  inline def transform: To =
    summonFrom {
      case t: Transformer[From, To] => t.transform(src)
      case _                        => definition.buildTransformer.transform(src)
    }

  transparent inline def withDefinition(
      inline newDefinition: TransformerDefinition[From, To, ? <: Tuple]
  ): TransformerInto[From, To, ? <: Tuple] =
    inline newDefinition match {
      case d: TransformerDefinition[From, To, newTransformerOperations] =>
        TransformerInto[From, To, newTransformerOperations](src, d)
    }
}
