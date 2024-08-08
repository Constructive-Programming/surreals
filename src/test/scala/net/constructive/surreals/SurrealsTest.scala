package net.constructive.surreals

//import algebra.laws.RingLaws
import munit.DisciplineSuite
import cats.PartialOrder
import org.scalacheck.*
import org.scalacheck.Prop.*

class SurrealsTest extends DisciplineSuite:
  def surrealGen: Gen[Surreal] =
    Gen.oneOf(Zero, Omega, Epsilon)

  given surrealArb[S]: Arbitrary[Surreal] = Arbitrary(surrealGen)

  val field = SurrealField()

  //checkAll("Surreals", RingLaws[Surreal].field(field))

  test("simple string representation") {
    import field.*
    assertEquals(Surreal.stringRep(Zero), Strings.Zero)
    assertEquals(Surreal.stringRep(Omega), Strings.Omega)
    assertEquals(Surreal.stringRep(Epsilon), Strings.Epsilon)
    assertEquals(Surreal.stringRep(One), s"{ ${Strings.Zero} | }")
    assertEquals(Surreal.stringRep(inc(One)), s"{ { ${Strings.Zero} | } | }")
  }

  property("int representation") {
    forAll(Gen.chooseNum(Int.MinValue, Int.MaxValue, 0, 1, -1)) { (i) =>
      import SurrealStructure.*
      val s = StringRep(fromInt(i => s"<$i>")(i))
      i match
        case 0 => assertEquals(s, Strings.Zero)
        case i  if i > 0 => assertEquals(s, s"{ <${i - 1}> | }")
        case i  if i < 0 => assertEquals(s, s"{ | <${i + 1}> }")
    }
  }


  val smallInts = Gen.choose(-10, 10)
  val pairOfSmallInts = smallInts.flatMap(i => smallInts.map(_ -> i))

  property("intComparisons") {
    forAll(pairOfSmallInts) { (i, j) =>
      import SurrealStructure.*
      val si = intStructure(i)
      val sj = intStructure(j)
      assertEquals(
        SurrealStructure.partialOrder.partialCompare(si, sj),
        PartialOrder[Int].partialCompare(i, j)
      )
    }
  }

  property("int addition") {
    forAll(pairOfSmallInts) { (i, j) =>
      import SurrealStructure.*
      val si = intStructure(i)
      val sj = intStructure(j)
      assertEquals(
        SurrealStructure.partialOrder.partialCompare(plus(si, sj), intStructure(i + j)),
        0.0
      )
    }
  }
