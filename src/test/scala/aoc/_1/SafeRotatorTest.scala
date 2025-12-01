package aoc._1

import munit.CatsEffectSuite

class SafeRotatorTest extends CatsEffectSuite {

  test("rotate right with single command") {
    // given
    val safeRotator = SafeRotator(0, 100, 50)

    // when
    val safeAfterRotation = safeRotator.rotate((RightRotate, 5L))

    // then
    assertEquals(safeAfterRotation.currentRotation, 55L)
  }

  test("rotate left with single command") {
    // given
    val safeRotator = SafeRotator(0, 100, 50)

    // when
    val safeAfterRotation = safeRotator.rotate((LeftRotate, 5L))

    // then
    assertEquals(safeAfterRotation.currentRotation, 45L)
  }

  test("rotate right with single command exceeding") {
    // given
    val safeRotator = SafeRotator(0, 100, 50)

    // when
    val safeAfterRotation = safeRotator.rotate((RightRotate, 55L))

    // then
    assertEquals(safeAfterRotation.currentRotation, 5L)
  }

  test("rotate left with single command exceeding") {
    // given
    val safeRotator = SafeRotator(0, 100, 50)

    // when
    val safeAfterRotation = safeRotator.rotate((LeftRotate, 55L))

    // then
    assertEquals(safeAfterRotation.currentRotation, 95L)
  }

  test("rotate right with single command few times exceeding") {
    // given
    val safeRotator = SafeRotator(0, 100, 50)

    // when
    val safeAfterRotation = safeRotator.rotate((RightRotate, 965L))

    // then
    assertEquals(safeAfterRotation.currentRotation, 15L)
  }

  test("rotate left with single command few times exceeding") {
    // given
    val safeRotator = SafeRotator(0, 100, 50)

    // when
    val safeAfterRotation = safeRotator.rotate((LeftRotate, 965L))

    // then
    assertEquals(safeAfterRotation.currentRotation, 85L)
  }

  test("rotate left with single command exceeding") {
    // given
    val safeRotator = SafeRotator(0, 100, 14)

    // when
    val safeAfterRotation = safeRotator.rotate((LeftRotate, 82L))

    // then
    assertEquals(safeAfterRotation.currentRotation, 32L)
  }

  test("rotate right with single command should set currentRotation to 0") {
    // given
    val safeRotator = SafeRotator(0, 100, 50)

    // when
    val safeAfterRotation = safeRotator.rotate((RightRotate, 50L))

    // then
    assertEquals(safeAfterRotation.currentRotation, 0L)
  }

  test("rotate left with single command should set currentRotation to 0") {
    // given
    val safeRotator = SafeRotator(0, 100, 50)

    // when
    val safeAfterRotation = safeRotator.rotate((LeftRotate, 50L))

    // then
    assertEquals(safeAfterRotation.currentRotation, 0L)
  }

  test(
    "rotate left with single command exceeding should set currentRotation to 0"
  ) {
    // given
    val safeRotator = SafeRotator(0, 100, 50)

    // when
    val safeAfterRotation = safeRotator.rotate((LeftRotate, 150L))

    // then
    assertEquals(safeAfterRotation.currentRotation, 0L)
  }

}
