package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import utest.*

case class Value1(a: Int)
case class Value2(a: Int)
case class Values1(a: Value1, b: Int, c: Int)
case class Values2(a: Value2, b: Int, c: Int)

object TempTests extends TestSuite {

  val tests = Tests {

    test {
      Values1(Value1(1), 2, 3).transformInto[Values2] ==> Values2(Value2(1), 2, 3)
    }

    test {
      val transformer = Transformer
        .define[Values1, Values2]
        .withFieldConst(_.b, 2)
        .withFieldConst(_.c, 3)
        .buildTransformer

      transformer.transform(Values1(Value1(1), 1, 1)) ==> Values2(Value2(1), 2, 3)
    }

    test {
      Value1(1).into[Value2].withFieldConst(_.a, 2).transform ==> Value2(2)
    }
  }
}
