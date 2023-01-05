package com.spotify.elitzur.cats

import com.spotify.elitzur.cats.Common._
import com.spotify.elitzur.validators.{Unvalidated, Valid}
import com.spotify.elitzur.{AgeTesting, CountryCodeTesting, MetricsReporter}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.collection.mutable

class ValidatorWithCatsTest extends AnyFlatSpec with should.Matchers {

  class TestMetricsReporter extends MetricsReporter {
    val metrics: mutable.Map[String, Long] =
      scala.collection.mutable.Map[String, Long]()

    override def reportValid(className: String,
                             fieldName: String,
                             validationTypeName: String): Unit = {
      val key = s"$fieldName/$validationTypeName/ElitzurValid"
      this.metrics.put(key, this.metrics.getOrElse(key, 0L) + 1L)
    }

    override def reportInvalid(className: String,
                               fieldName: String,
                               validationTypeName: String): Unit = {
      val key = s"$fieldName/$validationTypeName/ElitzurInvalid"
      this.metrics.put(key, this.metrics.getOrElse(key, 0L) + 1L)
    }

  }
  case class NestedTest(country: String)
  case class DataTest(age: Long, nested: NestedTest, nativeLong: Long = 7L)

  object DataTestValidator extends FunctionalValidator[DataTest] {
    override def shouldValidate: Boolean = true
    override def validationType: String = "DataTest"
    override def apply(v1: DataTest): List[ValidationResult] =
      List(
        AgeTesting(v1.age).validateWithCats("age"),
        CountryCodeTesting(v1.nested.country).validateWithCats("nested.country")
      ).flatten
  }

  it should "validate Correct results" in {
    implicit val reporter: TestMetricsReporter = new TestMetricsReporter
    val validator = new ValidatorWithCats(DataTestValidator)
    val d = DataTest(5, NestedTest("CA"))

    validator.validateRecord(Unvalidated(RecordWrapper(d, None))) shouldBe Valid(
      RecordWrapper(d, None)
    )

    reporter.metrics shouldBe scala.collection.mutable
      .Map(
        "nested.country/CountryCodeTesting/ElitzurValid" -> 1L,
        "age/AgeTesting/ElitzurValid" -> 1L
      )
  }

  it should "validate Incorrect results" in {
    implicit val reporter: TestMetricsReporter = new TestMetricsReporter
    val validator = new ValidatorWithCats[DataTest](DataTestValidator)
    val d = DataTest(500, NestedTest("sdfsdfdfd"))

    val actual =
      validator
        .validateRecord(Unvalidated(RecordWrapper(d, None)))

    actual.isValid shouldBe false
    actual.isInvalid shouldBe true

    actual.forceGet.invalid.map(_.map(_.name)) shouldBe Some(
      List("age", "nested.country")
    )

    reporter.metrics shouldBe scala.collection.mutable
      .Map(
        "nested.country/CountryCodeTesting/ElitzurInvalid" -> 1L,
        "age/AgeTesting/ElitzurInvalid" -> 1L
      )
  }
}
