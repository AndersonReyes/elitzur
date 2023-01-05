package com.spotify.elitzur.cats

object Common {
  case class Field[T](name: String, value: T)

  type ValidationResult = Either[Field[_], Field[_]]

  /**
   * Wraps data with invalid fields context if invalid
   */
  case class RecordWrapper[T](data: T, invalid: Option[List[Field[_]]])

}
