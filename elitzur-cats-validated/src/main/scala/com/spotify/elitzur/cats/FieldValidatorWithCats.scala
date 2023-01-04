package com.spotify.elitzur.cats

import com.spotify.elitzur.cats.ValidatorWithCats._
import com.spotify.elitzur.validators._

/** field validator is just a function that takes [A] and returns validation result.
  * Cats Validated heavily relies on functional validation. */
sealed trait FieldValidatorWithCats[A] extends (A => ValidationResult) {
  def validationType: String
  def shouldValidate: Boolean
}

object FieldValidatorWithCats {

  /** Validate as along as there exist a core elitzur field validator */
  def wrapCoreValidator[T](fieldName: String)(
    implicit coreValidator: FieldValidator[T]
  ): FieldValidatorWithCats[T] = new FieldValidatorWithCats[T] {

    override def shouldValidate: Boolean = coreValidator.shouldValidate

    override def validationType: String = coreValidator.validationType

    override def apply(v1: T): ValidationResult =
      Either.cond(
        coreValidator.validate(Unvalidated(v1)).isValid,
        Field(fieldName, v1),
        Field(fieldName, v1)
      )
  }

  object Implicits {
    implicit class ToFieldValidatorImplicits[T](v: T) {

      /** Convert to cats field validator provided there exists a core elitzur validator */
      def validateWithCats(
        fieldName: String
      )(implicit coreValidator: FieldValidator[T]): ValidationResult =
        wrapCoreValidator(fieldName)(coreValidator).apply(v)
    }
  }
}
