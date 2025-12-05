package aoc._5

import munit.CatsEffectSuite
import InclusiveRangeLong.{Merged, NotMerged}

class InclusiveRangeLongSuite extends CatsEffectSuite {

  val _1to5 = InclusiveRangeLong(1, 5)
  val _1to8 = InclusiveRangeLong(1, 8)
  val _1to10 = InclusiveRangeLong(1, 10)
  val _3to5 = InclusiveRangeLong(3, 5)
  val _5to10 = InclusiveRangeLong(5, 10)
  val _8to16 = InclusiveRangeLong(8, 16)
  val _7to_20 = InclusiveRangeLong(7, 20)
  val _10to14 = InclusiveRangeLong(10, 14)

  test("merge same range should return same range") {
    // when
    val merged = _1to5.merge(_1to5)

    // then
    assertEquals(merged, Merged(_1to5))
  }

  test("merge right subset contact should merge") {
    // when
    val merged = _1to5.merge(_3to5)

    // then
    assertEquals(merged, Merged(_1to5))
  }

  test("merge left subset contact should merge") {
    // when
    val merged = _1to8.merge(_1to5)

    // then
    assertEquals(merged, Merged(_1to8))
  }

  test("merge subset should merge") {
    // given
    val merged = _1to8.merge(_3to5)

    // then
    assertEquals(merged, Merged(_1to8))
  }

  test("merge append contact should merge") {
    // when
    val merged = _5to10.merge(_10to14)

    // then
    assertEquals(merged, Merged(InclusiveRangeLong(5, 14)))
  }

  test("merge append intersection should merge") {
    // when
    val merged = _5to10.merge(_8to16)

    // then
    assertEquals(merged, Merged(InclusiveRangeLong(5, 16)))
  }

  test("merge append no intersection or contact should not merge") {
    // when
    val merged = _1to5.merge(_8to16)

    // then
    assertEquals(merged, NotMerged)
  }

  test("merge prepend contact should merge") {
    // when
    val merged = _1to5.merge(_5to10)

    // then
    assertEquals(merged, Merged(InclusiveRangeLong(1, 10)))
  }

  test("merge prepend intersection should merge") {
    // when
    val merged = _8to16.merge(_5to10)

    // then
    assertEquals(merged, Merged(InclusiveRangeLong(5, 16)))
  }

  test("merge prepend no intersection or contact should not merge") {
    // when
    val merged = _10to14.merge(_3to5)

    // then
    assertEquals(merged, NotMerged)
  }

  test("merge superset right contact should merge") {
    // when
    val merged = _5to10.merge(_1to10)

    // then
    assertEquals(merged, Merged(InclusiveRangeLong(1, 10)))
  }

  test("merge superset left contact should merge") {
    // when
    val merged = _1to5.merge(_1to8)

    // then
    assertEquals(merged, Merged(InclusiveRangeLong(1, 8)))
  }

  test("merge superset no contact should merge") {
    // when
    val merged = _10to14.merge(_7to_20)

    // then
    assertEquals(merged, Merged(InclusiveRangeLong(7, 20)))
  }
}
