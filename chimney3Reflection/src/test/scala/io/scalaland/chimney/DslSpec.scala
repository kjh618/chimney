package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.utils.MacroUtils.inspect
import utest.*

object DslSpec extends TestSuite {

  val tests = Tests {

    "use implicit transformer directly" - {

      import Domain1._

      implicit def trans: Transformer[UserName, String] = userNameToStringTransformer

      UserName("Batman").into[String].transform ==> "BatmanT"
      UserName("Batman").transformInto[String] ==> "BatmanT"
    }

    "use implicit transformer for nested field" - {

      import Domain1._

      implicit def trans: Transformer[UserName, String] = userNameToStringTransformer

      val batman = User("123", UserName("Batman"))
      val batmanDTO = batman.transformInto[UserDTO]

      batmanDTO.id ==> "123"
      batmanDTO.name ==> "BatmanT"
    }

    "support different set of fields of source and target" - {

      case class Foo(x: Int, y: String, z: (Double, Double))
      case class Bar(x: Int, z: (Double, Double))
      case class HaveY(y: String)

      "field is dropped - the target" - {
        Foo(3, "pi", (3.14, 3.14)).transformInto[Bar] ==> Bar(3, (3.14, 3.14))
      }

      "field is added to the target" - {

        "not compile if source for the target fields is not provided" - {

          compileError("Bar(3, (3.14, 3.14)).transformInto[Foo]")
            .check("", "no accessor named y in source type io.scalaland.chimney.DslSpec.Bar")
        }

        "fill the field with provided default value" - {

          "pass when selector is valid" - {

            Bar(3, (3.14, 3.14))
              .into[Foo]
              .withFieldConst(_.y, "pi")
              .transform ==>
              Foo(3, "pi", (3.14, 3.14))

            Bar(3, (3.14, 3.14))
              .into[Foo]
              .withFieldConst(cc => cc.y, "pi")
              .transform ==>
              Foo(3, "pi", (3.14, 3.14))
          }

          "not compile when selector is invalid" - {

            compileError("""Bar(3, (3.14, 3.14))
                  .into[Foo]
                  .withFieldConst(_.y, "pi")
                  .withFieldConst(_.z._1, 0.0)
                  .transform
                """)
              .check("", "Invalid selector!")

            compileError("""Bar(3, (3.14, 3.14))
                  .into[Foo]
                  .withFieldConst(_.y + "abc", "pi")
                  .transform
                """)
              .check("", "Invalid selector!")

            compileError("""
                val haveY = HaveY("")
                Bar(3, (3.14, 3.14))
                  .into[Foo]
                  .withFieldConst(cc => haveY.y, "pi")
                  .transform
                """)
              .check("", "Invalid selector!")
          }
        }

//        "support default values for Options" - {
//          case class SomeFoo(x: String)
//          case class Foobar(x: String, y: Option[Int])
//          case class Foobar2(x: String, y: Option[Int] = Some(42))
//
//          "use None when .enableOptionDefaultsToNone" - {
//            SomeFoo("foo").into[Foobar].enableOptionDefaultsToNone.transform ==> Foobar("foo", None)
//          }
//
//          "not compile if .enableOptionDefaultsToNone is missing" - {
//            compileError("""SomeFoo("foo").into[Foobar].transform ==> Foobar("foo", None)""")
//              .check("", "Chimney can't derive transformation from SomeFoo to Foobar")
//          }
//
//          "target has default value, but default values are disabled and .enableOptionDefaultsToNone" - {
//            SomeFoo("foo").into[Foobar2].disableDefaultValues.enableOptionDefaultsToNone.transform ==>
//              Foobar2("foo", None)
//          }
//
//          "not use None as default when other default value is set" - {
//            SomeFoo("foo").into[Foobar2].transform ==> Foobar2("foo", Some(42))
//            SomeFoo("foo").into[Foobar2].enableOptionDefaultsToNone.transform ==> Foobar2("foo", Some(42))
//          }
//
//          "not compile if default value is missing and no .enableOptionDefaultsToNone" - {
//            compileError("""SomeFoo("foo").into[Foobar].transform""")
//              .check("", "Chimney can't derive transformation from SomeFoo to Foobar")
//          }
//
//          "not compile if default values are disabled and no .enableOptionDefaultsToNone" - {
//            compileError("""SomeFoo("foo").into[Foobar2].disableDefaultValues.transform""")
//              .check("", "Chimney can't derive transformation from SomeFoo to Foobar2")
//          }
//        }
//
//        "use implicit transformer for option when .enableUnsafeOption" - {
//          case class Foobar(x: Option[Int])
//          case class Foobar2(x: String)
//
//          implicit val stringToIntTransformer: Transformer[Int, String] = _.toString
//
//          "use transformer when .enableUnsafeOption" - {
//            Foobar(Some(1)).into[Foobar2].enableUnsafeOption.transform ==> Foobar2("1")
//          }
//
//          "use transformer when .disableUnsafeOption adn then .enableUnsafeOption" - {
//            Foobar(Some(1)).into[Foobar2].disableUnsafeOption.enableUnsafeOption.transform ==> Foobar2("1")
//          }
//        }
//
//        "fill the field with provided generator function" - {
//
//          "pass when selector is valid" - {
//
//            Bar(3, (3.14, 3.14))
//              .into[Foo]
//              .withFieldComputed(_.y, _.x.toString)
//              .transform ==>
//              Foo(3, "3", (3.14, 3.14))
//
//            Bar(3, (3.14, 3.14))
//              .into[Foo]
//              .withFieldComputed(cc => cc.y, _.x.toString)
//              .transform ==>
//              Foo(3, "3", (3.14, 3.14))
//          }
//
//          "not compile when selector is invalid" - {
//
//            compileError("""Bar(3, (3.14, 3.14))
//                  .into[Foo]
//                  .withFieldComputed(_.y, _.x.toString)
//                  .withFieldComputed(_.z._1, _.z._1 * 10.0)
//                  .transform
//                """)
//              .check("", "Invalid selector!")
//
//            compileError("""Bar(3, (3.14, 3.14))
//                  .into[Foo]
//                  .withFieldComputed(_.y + "abc", _.x.toString)
//                  .transform
//                """)
//              .check("", "Invalid selector!")
//
//            compileError("""
//                val haveY = HaveY("")
//                Bar(3, (3.14, 3.14))
//                  .into[Foo]
//                  .withFieldComputed(cc => haveY.y, _.x.toString)
//                  .transform
//                """)
//              .check("", "Invalid selector!")
//          }
//        }
      }
    }

    // TODO
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
