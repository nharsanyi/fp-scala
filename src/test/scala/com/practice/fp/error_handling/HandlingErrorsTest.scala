package com.practice.fp.error_handling

import org.scalatest.FunSuite

class HandlingErrorsTest extends FunSuite {

  case class Person(firstName: String, lastName: String)

  test("should map") {
    val p = Person("John", "Doe")
    val f: Person => String = x => s"${x.firstName} ${x.lastName}"

    assertResult(Some("John Doe"))(Some(p).map(f))
    assertResult(None)(None.map(f))
  }

  test("should flatMap") {
    val p = Person("John", "Doe")
    val f: Person => Option[String] = x => Some(s"${x.firstName} ${x.lastName}")

    assertResult(Some("John Doe"))(Some(p).flatMap(f))
    assertResult(None)(None.flatMap(f))
  }

  test("should getOrElse") {
    val p = Person("John", "Doe")
    val defP = Person("Jane", "Doe")

    assertResult(p)(Some(p).getOrElse(defP))
    assertResult(defP)(None.getOrElse(defP))
  }

  test("should orElse") {
    val p = Person("John", "Doe")
    val defP = Some(Person("Jane", "Doe"))

    assertResult(Some(p))(Some(p).orElse(defP))
    assertResult(defP)(None.orElse(defP))
  }

  test("should filter") {
    val p = Person("John", "Doe")
    val f = (x: Person) => x.lastName == "Doe"

    assertResult(Some(p))(Some(p).filter(f))
    assertResult(None)(None.filter(f))
    assertResult(None)(Some(Person("John", "Smith")).filter(f))
  }

  test("should sequence") {
    val l1: List[HandlingErrors.Option[Int]] = List(HandlingErrors.Some(1), HandlingErrors.Some(2))
    assertResult(HandlingErrors.Some(List[Int](1, 2)))(HandlingErrors.sequence(l1))

    val l2: List[HandlingErrors.Option[Int]] = List(HandlingErrors.Some(1), HandlingErrors.None, HandlingErrors.Some(2))
    assertResult(HandlingErrors.None)(HandlingErrors.sequence(l2))

    val l3: List[HandlingErrors.Option[Int]] = List(HandlingErrors.Some(1))
    assertResult(HandlingErrors.Some(List[Int](1)))(HandlingErrors.sequence(l3))

  }
}
