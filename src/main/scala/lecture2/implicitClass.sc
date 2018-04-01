implicit class IntWithTimes(n: Int){
  def times(f: => Unit) = {
    def loop(current: Int): Unit = {
      if(current > 0) {
        f
        loop(current - 1)
      }
    }
    loop(n)
  }
}

10 times(println("Hi"))