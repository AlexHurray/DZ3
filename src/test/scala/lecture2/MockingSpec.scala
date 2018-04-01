package lecture2

import lecture2.mock.HttpMethod.HttpMethod
import lecture2.mock.{MethodCounter, HttpClient, HttpMethod}
import org.scalamock.scalatest.MockFactory
import org.scalatest._

/**
  * This test tests nothing and is intended only to show the power of Mockito mocks
  *
  *
  */
class MockingSpec extends FlatSpec with MockFactory {


  "MockExample" should "verify MethodCounter" in {
    val m = mockFunction[Int, String]
    val mockClient = mock[HttpClient]

    m expects (42) returning "Forty two" once()
    (mockClient.send _).stubs(*) onCall { arg: HttpMethod => {
        MethodCounter.incMethod(arg)
        s"This is ${arg}"
      }
    }


    assert(m(42) == "Forty two")
    assert(mockClient.send(HttpMethod.POST) == "This is POST")
    assert(mockClient.send(HttpMethod.POST) == "This is POST")
    assert(mockClient.send(HttpMethod.GET) == "This is GET")
    assert(MethodCounter.counter ==  Map(HttpMethod.POST -> 2, HttpMethod.GET -> 1, HttpMethod.DELETE -> 0))
  }
}