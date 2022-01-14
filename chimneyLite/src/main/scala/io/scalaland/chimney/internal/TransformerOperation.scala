package io.scalaland.chimney.internal

sealed trait TransformerOperation

object TransformerOperation {
  final class FieldConst[Name <: String] extends TransformerOperation
}
