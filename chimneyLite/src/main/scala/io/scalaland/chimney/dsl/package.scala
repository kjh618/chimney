package io.scalaland.chimney.dsl

import io.scalaland.chimney.Transformer

extension [From](source: From) {
  def transformInto[To](using transformer: Transformer[From, To]): To =
    transformer.transform(source)
}
