package net.constructive.surreals

import cats.Eq
import algebra.ring.Field
import algebra.ring.AdditiveMonoid
import higherkindness.droste.data.Fix

trait Surreal

object Zero extends Surreal
object Omega extends Surreal
object Epsilon extends Surreal
object Tau extends Surreal
case class LeftNumber(left: Surreal) extends Surreal
case class RightNumber(right: Surreal) extends Surreal
case class DyadicNumber(left: Surreal, right: Surreal) extends Surreal

object Surreal:

  given equality: Eq[Surreal] =
    case (Zero, Zero) => true

  given additiveMonoid: AdditiveMonoid[Surreal] with {
    def zero = Zero
    def plus(a: Surreal, b: Surreal) = SurrealField().plus(a, b)
  }

  def recurively(n: Surreal): Fix[SurrealStructure] =
    Fix(SurrealStructure.fromSurreal(recurively)(n))

  def stringRep(n: Surreal): String =
    SurrealStructure.StringRep(SurrealStructure.fromSurreal(stringRep)(n))

case class SurrealField() extends Field[Surreal]:
  val One = LeftNumber(Zero)
  def zero = Zero
  def one = One

  def inc: Surreal => Surreal =
    case n@(Zero | Omega | Epsilon | LeftNumber(_)) => LeftNumber(n)
    case RightNumber(s) => s

  def negate(n: Surreal) = n match
    case Zero => Zero
    case LeftNumber(s) => RightNumber(s)
    case RightNumber(s) => LeftNumber(s)
    case DyadicNumber(l, r) => DyadicNumber(r, l)

  def plus(a: Surreal, b: Surreal) = (a, b) match
    case (a, Zero) => a
    case (Zero, b) => b

  def times(a: Surreal, b: Surreal) = (a, b) match
    case (Zero, _) => Zero
    case (_, Zero) => Zero
    case (One, b) => b
    case (b, One) => b
  def div(n: Surreal, d: Surreal) = (n, d) match
    case (Zero, _) => Zero
    case (b, Zero) => Zero
