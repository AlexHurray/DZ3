package lecture2.mock

import lecture2.mock.HttpMethod.HttpMethod

/**
  * Created by a.a.perfilyev on 21.03.2018.
  */
trait HttpClient {
  def send(method: HttpMethod): String
}

object HttpMethod extends Enumeration {
  type HttpMethod = Value
  val POST, GET, DELETE = Value
}
