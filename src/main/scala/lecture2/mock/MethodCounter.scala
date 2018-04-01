package lecture2.mock

import lecture2.mock.HttpMethod.HttpMethod

/**
  * Created by a.a.perfilyev on 21.03.2018.
  */
object MethodCounter {
  var counter = Map(HttpMethod.POST -> 0, HttpMethod.GET -> 0, HttpMethod.DELETE -> 0)
  def incMethod(method: HttpMethod) = {
    val before = counter(method)
    counter += (method -> (before+1))
  }
}
