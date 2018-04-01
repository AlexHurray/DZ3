trait Logging {
  def info(message: String) = println(s"${Console.GREEN}INFO: $message${Console.RESET}")
  def error(message: String) = println(s"${Console.RED}ERROR: $message${Console.RESET}")
}