// SPDX-License-Identifier: Apache-2.0

package firrtl.stage.transforms

import firrtl.{CircuitState, CustomTransformException, Transform}

class CatchCustomTransformExceptions(val underlying: Transform) extends Transform with WrappedTransform {

  override def execute(c: CircuitState): CircuitState = try {
    underlying.transform(c)
  } catch {
    case e: Exception => throw CustomTransformException(e)
  }

}

object CatchCustomTransformExceptions {
  def apply(a: Transform): CatchCustomTransformExceptions = new CatchCustomTransformExceptions(a)
}
