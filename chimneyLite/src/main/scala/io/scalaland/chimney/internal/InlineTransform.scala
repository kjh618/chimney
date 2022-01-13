package io.scalaland.chimney.internal

import io.scalaland.chimney.internal.dsl.TransformerDefinitionImpl

inline def inlineTransform[From, To](inline src: From): To =
  ${ TransformerDefinitionImpl.genTransformBody[From, To]('src) }
