package io.scalaland.chimney.dsl

import io.scalaland.chimney.Transformer
import io.scalaland.chimney.internal.dsl.TransformerDefinitionImpl

import scala.deriving.Mirror

case class TransformerDefinition[From, To](newFieldValues: Map[String, Any]) {

  inline def withFieldConst[T, U](inline selector: To => T, value: U)(using U <:< T): TransformerDefinition[From, To] =
    ${ TransformerDefinitionImpl.withFieldConstImpl('this)('selector, 'value) }

  inline def buildTransformer(using destMirror: Mirror.ProductOf[To]): Transformer[From, To] =
    ${ TransformerDefinitionImpl.buildTransformerImpl('this, 'destMirror) }
}
