implicit class OptionWithDefault(option: Option[Int]){
  def default() : Int = {
    option.fold(0)(x => x)
  }
}

Some(10).default
Option.empty[Int].default