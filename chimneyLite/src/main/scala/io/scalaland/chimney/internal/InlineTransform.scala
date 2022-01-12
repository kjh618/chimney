package io.scalaland.chimney.internal

import io.scalaland.chimney.internal.dsl.TransformerDefinitionImpl

import scala.deriving.Mirror

inline def inlineTransform[From, To](inline src: From)(using destMirror: Mirror.ProductOf[To]): To =
  TransformerDefinitionImpl.getDest(src, destMirror)
