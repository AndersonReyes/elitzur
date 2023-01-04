package com.spotify.elitzur.cats

import com.spotify.elitzur.MetricsReporter
import com.spotify.elitzur.cats.ValidatorWithCats._
import com.spotify.elitzur.validators.{
  Invalid,
  PostValidation,
  PreValidation,
  Valid,
  ValidationRecordConfig,
  Validator
}

import scala.reflect.ClassTag

class ValidatorWithCats[A](fn: A => List[ValidationResult])(
  implicit reporter: MetricsReporter,
  classTag: ClassTag[A]
) extends Validator[RecordWrapper[A]] {

//  override def shouldValidate: Boolean = true

  override def validateRecord(
    a: PreValidation[RecordWrapper[A]],
    path: String,
    outermostClassName: Option[String],
    config: ValidationRecordConfig
  ): PostValidation[RecordWrapper[A]] = {
    val data = a.forceGet.data
    val results = fn(data)
    val valid: List[Field[_]] =
      results.filter(_.isRight).map { case Right(n) => n }

    val invalid: List[Field[_]] = results.filter(_.isLeft).map {
      case Left(e) => e
    }

    valid.foreach(
      f =>
        reporter.reportValid(
          classTag.runtimeClass.getSimpleName,
          f.name,
          f.value.getClass.getSimpleName
      )
    )

    invalid.foreach(
      f =>
        reporter.reportInvalid(
          classTag.runtimeClass.getSimpleName,
          f.name,
          f.value.getClass.getSimpleName
      )
    )

    if (invalid.nonEmpty) {
      Invalid(RecordWrapper(data, Some(invalid)))
    } else {
      Valid(RecordWrapper(data, None))
    }
  }

  override def shouldValidate: Boolean = true
}

object ValidatorWithCats {
  case class Field[T](name: String, value: T)
  type ValidationResult = Either[Field[_], Field[_]]

  /**
    * Wraps data with invalid fields context if invalid
    */
  case class RecordWrapper[T](data: T, invalid: Option[List[Field[_]]])
}
