package lecture2

import org.scalatest.concurrent.Eventually
import org.scalatest.exceptions.TestFailedException
import org.scalatest.{FunSuite, Matchers, OptionValues, ParallelTestExecution}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Success}

/**
  * Created by a.a.perfilyev on 22.03.2018.
  */
class ScalaTestSugarSpec extends FunSuite with Matchers with Eventually with OptionValues with ParallelTestExecution {

  implicit val config: PatienceConfig = PatienceConfig(timeout = 5 seconds, interval = 1 second)

  var changeInTime = 0

  val changeIt = Future {
    Thread.sleep(5)
    10
  }

  changeIt.onComplete {
    case Success(x) => changeInTime = x
    case Failure(_) => changeInTime = -1
  }

  test("Variable with side effect should change in 10 seconds") {
    eventually {
      changeInTime should be(10)
    }
  }

  test("Wait before") {
    Thread.sleep(5)
    changeInTime should be(10)
  }

  test("Test Some value") {
    val some = Some("String")
    some.value should fullyMatch regex """^S.*ng$"""
  }

  test("Test None value") {
    val some: Option[String] = None
    intercept[TestFailedException] {
      some.value should fullyMatch regex """^S.*ng$"""
    }
  }
}
