package com.jgeof.mycorrhiza.util

object Exceptions {
    case class InvalidCharacter(message: String) extends Exception(message)
    case class SequenceLengthError(message: String) extends Exception(message)
    case class KeyAlreadyPresent(message: String) extends Exception(message)
}
