import org.scalatest.{Matchers,PropSpec}
// import Matchers._
import org.scalatest.prop.{PropertyChecks,TableDrivenPropertyChecks}

import org.scalatest.concurrent.{Signaler, TimeLimitedTests}
import org.scalatest.time.SpanSugar._


abstract class UnitSpec
         extends PropSpec
         with Matchers
         with PropertyChecks
         with TimeLimitedTests {
  def timeLimit = 1 second
  override val defaultTestSignaler = new Signaler {
    @deprecated("","")
    override def apply (testThread: Thread) : Unit = {
      println ("Too slow")
      testThread.stop () // deprecated. unsafe. do not use
    }
  }

  // override val defaultTestInterruptor: Interruptor = new Interruptor {
  //   override def apply(testThread: Thread): Unit = {
  //     println("Too slow")
  //     testThread.stop() // deprecated. unsafe. do not use
  //   }
  // }
}


         // with TableDriven
         // with GeneratorDrivenPropertyChecks

// with Matchers
// with ShouldMatchers
// with OptionValues
// with Inside
// with Inspectors
