package mycorrhiza

object Exceptions {
    case class InvalidCharacter(message: String) extends Exception(message)
}
