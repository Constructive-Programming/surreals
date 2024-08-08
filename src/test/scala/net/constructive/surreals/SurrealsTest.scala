package net.constructive.surreals

//import algebra.laws.RingLaws
import munit.DisciplineSuite
import cats.PartialOrder
import higherkindness.droste.data.Fix
import org.scalacheck.*
import org.scalacheck.Prop.*

class SurrealsTest extends DisciplineSuite:

  val field = SurrealField()

  //checkAll("Surreals", RingLaws[Surreal].field(field))

  test("simple string representations") {
    import field.*
    assertEquals(Surreal.stringRep(Zero), Strings.Zero)
    assertEquals(Surreal.stringRep(Omega), Strings.Omega)
    assertEquals(Surreal.stringRep(Epsilon), Strings.Epsilon)
    assertEquals(Surreal.stringRep(One), s"{ ${Strings.Zero} | }")
    assertEquals(Surreal.stringRep(inc(One)), s"{ { ${Strings.Zero} | } | }")
    assertEquals(Surreal.stringRep(inc(dec(Epsilon))), Strings.Epsilon)
    assertEquals(Surreal.stringRep(inc(inc(Omega))), s"{ { ${Strings.Omega} | } | }")
    assertEquals(Surreal.stringRep(dec(dec(Tau))), s"{ | { | ${Strings.Tau} } }")
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


  val tinyInts = Gen.choose(-3, 3)
  val smallInts = Gen.choose(-100, 100)

  val specialSurreals = Gen.oneOf(OmegaS, EpsilonS, TauS)

  property("int comparisons") {
    forAll(smallInts, smallInts) { (i, j) =>
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
    forAll(smallInts, smallInts) { (i, j) =>
      import SurrealStructure.*
      val si = intStructure(i)
      val sj = intStructure(j)
      assertEquals(
        SurrealStructure.partialOrder.partialCompare(plus(si, sj), intStructure(i + j)),
        0.0
      )
    }
  }

  test("special addition with int") {
      import SurrealStructure.partialOrder
      val si = Fix(OmegaS)
      assert(partialOrder.gt(Fix(LeftS(si)), si))
      assert(partialOrder.gt(Fix(LeftS(Fix(LeftS(si)))), si))
      assert(partialOrder.gt(Fix(LeftS(Fix(LeftS(Fix(LeftS(si)))))), si))
      assert(partialOrder.gt(si, Fix(RightS(si))))
      assert(partialOrder.gt(si, Fix(RightS(Fix(RightS(Fix(RightS(si))))))))
      assert(partialOrder.gt(si, Fix(RightS(Fix(RightS(Fix(RightS(Fix(RightS(si))))))))))
      assert(partialOrder.gt(si, Fix(RightS(Fix(RightS(si))))))
  }

  // property("special addition with int") {
  //   forAll(specialSurreals, tinyInts) { (i, j) =>
  //     import SurrealStructure.*
  //     val si = Fix(i)
  //     val sj = intStructure(j)
  //     val sjInv = intStructure(-j)
  //     assertEquals(
  //       SurrealStructure.partialOrder.partialCompare(plus(plus(si, sj), sjInv), si),
  //       0.0
  //     )
  //   }
  // }
