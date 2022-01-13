package io.scalaland.chimney.internal

object InlineTransformer {
  inline def inlineTransform[From, To](inline src: From): To =
    ${ TransformerMacros.genTransformBody[From, To]('src) }
}
