package com.spotify.elitzur.cats

import com.spotify.elitzur.validators.FieldValidator

trait Implicits {
  implicit class ValidatorWithCatsImplicits[T](v: T) {
    def validateWithCats(fieldName: String)(
      implicit coreValidator: FieldValidator[T]
    ): List[Common.ValidationResult] =
      FunctionalValidator.wrappedValidator(Some(fieldName))(coreValidator)(v)
  }

}
