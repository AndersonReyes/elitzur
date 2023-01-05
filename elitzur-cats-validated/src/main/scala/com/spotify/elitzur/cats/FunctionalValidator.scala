package com.spotify.elitzur.cats

import com.spotify.elitzur.cats.Common._
import com.spotify.elitzur.validators._
import shapeless._

import scala.language.implicitConversions

trait FunctionalValidator[T] extends (T => List[ValidationResult]) {
  def validationType: String
  def shouldValidate: Boolean = true
}

//
object FunctionalValidator {
//    extends LabelledTypeClassCompanion[FunctionalValidator] {
  /** wrap core validators with cats validated */
  def wrappedValidator[T](name: Option[String] = None)(
    implicit coreValidator: FieldValidator[T]
  ): FunctionalValidator[T] = new FunctionalValidator[T] {
    override def shouldValidate: Boolean = coreValidator.shouldValidate
    override def validationType: String =
      name.getOrElse(coreValidator.validationType)

    override def apply(v1: T): List[ValidationResult] =
      List(
        Either.cond(
          coreValidator.validate(Unvalidated(v1)).isValid,
          Field(validationType, v1),
          Field(validationType, v1)
        )
      )
  }
//
//  def wrappedValidator[T](
//    name: String,
//    func: FunctionalValidator[T]
//  ): FunctionalValidator[T] = new FunctionalValidator[T] {
//    override def shouldValidate: Boolean = func.shouldValidate
//
//    override def validationType: String = name
//
//    override def apply(v1: T): List[ValidationResult] =
//      List(
//        Either.cond(
//          func(v1).exists(_.isRight),
//          Field(validationType, v1),
//          Field(validationType, v1)
//        )
//      )
//  }
//
//  implicit def instance[T](
//    implicit coreValidator: FieldValidator[T]
//  ): FunctionalValidator[T] =
//    wrappedValidator(Some(coreValidator.validationType))
//
//  object typeClass extends LabelledTypeClass[FunctionalValidator] {
//    override def coproduct[L, R <: Coproduct](
//      name: String,
//      cl: => FunctionalValidator[L],
//      cr: => FunctionalValidator[R]
//    ): FunctionalValidator[L :+: R] = ???
//
//    override def emptyCoproduct: FunctionalValidator[CNil] =
//      new FunctionalValidator[CNil] {
//        override def shouldValidate: Boolean = false
//        override def validationType: String = "CNil"
//
//        override def apply(v1: CNil): List[ValidationResult] = List.empty
//      }
//
//    def instanceFunc[T](
//      implicit f: Option[String] => FunctionalValidator[T]
//    ): Option[String] => FunctionalValidator[T] =
//      f
//
//    override def product[H, T <: HList](
//      name: String,
//      ch: FunctionalValidator[H],
//      ct: FunctionalValidator[T]
//    ): FunctionalValidator[H :: T] = new FunctionalValidator[H :: T] {
//      override def shouldValidate: Boolean =
//        ct.shouldValidate || ch.shouldValidate
//      override def validationType: String = name
//
//      private val f = wrappedValidator(s"$name.${ch.validationType}", ch)
//
//      override def apply(v1: H :: T): List[ValidationResult] = {
//        val head =
//          if (ch.shouldValidate) f(v1.head) else List.empty
//        val tail = ct.apply(v1.tail)
//        head ++ tail
//      }
//    }
//
//    override def emptyProduct: FunctionalValidator[HNil] =
//      new FunctionalValidator[HNil] {
//        override def shouldValidate: Boolean = false
//        override def validationType: String = "HNil"
//
//        override def apply(v1: HNil): List[ValidationResult] = List.empty
//      }
//
//    override def project[F, G](instance: => FunctionalValidator[G],
//                               to: F => G,
//                               from: G => F): FunctionalValidator[F] =
//      new FunctionalValidator[F] {
//        override def shouldValidate: Boolean = instance.shouldValidate
//        override def validationType: String = instance.validationType
//
//        override def apply(v1: F): List[ValidationResult] =
//          instance.apply(to(v1))
//      }
//  }

}
