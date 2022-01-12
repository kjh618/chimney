package io.scalaland.chimney

import utest.*
import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.dsl.TransformerDefinitionImpl
import io.scalaland.chimney.internal.utils.MacroUtils.inspect

object DslSpec extends TestSuite {

  val tests = Tests {

    "1" - {
      case class Value(a: Int)
      case class Values1(a: Int, b: Int, c: Int)
      case class Values2(a: Int, b: Int, c: Int)

      val transformer = Transformer
        .define[Values1, Values2]
        .withFieldConst(_.b, 2)
        .withFieldConst(_.c, 3)
        .buildTransformer
        .inspect()

      transformer.transform(Values1(1, 1, 1)) ==> Values2(1, 2, 3)
    }
  }
}

object Domain1 {

  case class UserName(value: String)

  val userNameToStringTransformer: Transformer[UserName, String] =
    (userName: UserName) => userName.value + "T"

  case class UserDTO(id: String, name: String)

  case class User(id: String, name: UserName)

}

object VCDomain1 {

  case class UserName(value: String) extends AnyVal

  case class UserDTO(id: String, name: String)

  case class User(id: String, name: UserName)

}

object Poly {

  case class MonoSource(poly: String, other: String)

  case class PolySource[T](poly: T, other: String)

  case class MonoTarget(poly: String, other: String)

  case class PolyTarget[T](poly: T, other: String)

  val monoSource = MonoSource("test", "test")
  val polySource = PolySource("test", "test")
  val monoTarget = MonoTarget("test", "test")
  val polyTarget = PolyTarget("test", "test")
}

object NonCaseDomain {

  class ClassSource(val id: String, val name: String)

  trait TraitSource {
    val id: String
    val name: String
  }

  class TraitSourceImpl(val id: String, val name: String) extends TraitSource
}
