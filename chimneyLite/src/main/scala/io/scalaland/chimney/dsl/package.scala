package io.scalaland.chimney.dsl

import io.scalaland.chimney.Transformer

extension [From](src: From) {

  def into[To](using transformer: Transformer[From, To]): Transformer[From, To] =
    transformer

  def transformInto[To](using transformer: Transformer[From, To]): To =
    transformer.transform(src)
}
