package com.jgeof.mycorrhiza

object Exceptions {
    case class InvalidCharacter(message: String) extends Exception(message)
    case class SequenceLengthError(message: String) extends Exception(message)
}
