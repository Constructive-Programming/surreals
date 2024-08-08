package net.constructive.surreals

import cats.{Functor, PartialOrder}
import higherkindness.droste.*
import higherkindness.droste.data.Fix

trait SurrealStructure[+S]

case object ZeroS extends SurrealStructure[Nothing]
case object EpsilonS extends SurrealStructure[Nothing]
case object OmegaS extends SurrealStructure[Nothing]
case object TauS extends SurrealStructure[Nothing]
case class LeftS[S](smaller: S) extends SurrealStructure[S]
case class RightS[S](larger: S) extends SurrealStructure[S]
case class GeneralS[S](smaller: S, larger: S) extends SurrealStructure[S]

object Strings:
  val EmptySet = "∅"
  val Zero = "ø"
  val Omega = "ω"
  val Epsilon = "ε"
  val Tau = "τ"
  def setRep(s: Set[String]) = s.mkString("{", ",", "}")

object SurrealStructure:
  type Structure = Fix[SurrealStructure]

  private case object EmptySet extends SurrealStructure[Nothing]

  private def smaller(n: Structure): Structure = n match
    case LeftS(s) => s.asInstanceOf[Structure]
    case RightS(_) => Fix(EmptySet)
    case GeneralS(s, _) => s.asInstanceOf[Structure]
    case ZeroS => Fix(EmptySet)
    case n => Fix(LeftS(n))

  private def larger(n: Structure): Structure = n match
    case LeftS(_) => Fix(EmptySet)
    case RightS(l) => l.asInstanceOf[Structure]
    case GeneralS(_, l) => l.asInstanceOf[Structure]
    case ZeroS => Fix(EmptySet)
    case n => Fix(RightS(n))

  def apply(l: Structure, r: Structure): Structure = Fix {
    (l, r) match
      case (EmptySet, EmptySet) => ZeroS
      case (RightS(s), EmptySet) => s.asInstanceOf[SurrealStructure[Structure]]
      case (s, EmptySet) => LeftS(s)
      case (EmptySet, LeftS(s)) => s.asInstanceOf[SurrealStructure[Structure]]
      case (EmptySet, s) => RightS(s)
      case (l, r) => GeneralS(l, r)
  }

  def unapply(s: Structure): Option[(Structure, Structure)] =
    Some(smaller(s) -> larger(s))

  given functor: Functor[SurrealStructure] with {
    def map[A, B](n: SurrealStructure[A])(f: A => B): SurrealStructure[B] = n match
      case ZeroS => ZeroS
      case EpsilonS => EpsilonS
      case OmegaS => OmegaS
      case TauS => TauS
      case LeftS(l) => LeftS(f(l))
      case RightS(r) => RightS(f(r))
      case GeneralS(l, r) => GeneralS(f(l), f(r))
  }

  given partialOrder: PartialOrder[Structure] with {
    def partialCompare(x: Structure, y: Structure): Double = (lteqv(x, y), lteqv(y, x)) match
      case (true, true) => 0.0
      case (true, false) => -1.0
      case (false, true) => 1.0
      case (false, false) => Double.NaN

    override def lteqv(x: Structure, y: Structure): Boolean = (x, y) match
      case (x, y) if (x == y) => true
      case (EmptySet, _) => true
      case (_, EmptySet) => false
      case (ZeroS, s) => lteqZero(s)
      case (EpsilonS, s) =>  lteqEpsilon(s)
      case (OmegaS, s) =>  lteqOmega(s)
      case (TauS, s) =>  lteqTau(s)
      case (SurrealStructure(xl:Structure, _),
            SurrealStructure(_, yr:Structure)) =>
        gt(y, xl) && gt(yr, x)

    def lteqZero(s: Structure): Boolean = s match {
      case EmptySet => true
      case ZeroS => true
      case EpsilonS => false
      case OmegaS => false
      case TauS => false
      case SurrealStructure(_, r: Structure) => gt(r, Fix(ZeroS))
    }

    def lteqEpsilon(s: Structure): Boolean = s match {
      case EmptySet => true
      case ZeroS => true
      case EpsilonS => true
      case OmegaS => false
      case TauS => false
      case SurrealStructure(_, r: Structure) => gt(r, Fix(EpsilonS))
    }

    def lteqOmega(s: Structure): Boolean = s match {
      case EmptySet => true
      case ZeroS => true
      case EpsilonS => true
      case OmegaS => true
      case TauS => false
      case SurrealStructure(_, r: Structure) => gt(r, Fix(OmegaS))
    }

    def lteqTau(s: Structure): Boolean = s match {
      case EmptySet => true
      case ZeroS => true
      case OmegaS => true
      case EpsilonS => true
      case TauS => true
      case SurrealStructure(_, r: Structure) => gt(r, Fix(TauS))
    }

    override def gt(x: Structure, y: Structure): Boolean =
      if (x == EmptySet || y == EmptySet) true
      else !(lteqv(x, y))

  }

  val toSurreal: Algebra[SurrealStructure, Surreal] = Algebra {
    case ZeroS => Zero
    case OmegaS => Omega
    case EpsilonS => Epsilon
    case TauS => Tau
    case LeftS(l) => LeftNumber(l)
    case RightS(r) => RightNumber(r)
    case GeneralS(l,  r) => DyadicNumber(l, r)
  }

  def plus(x: Structure, y: Structure): Structure =
    (x, y) match
      case (EmptySet, _) => Fix(EmptySet)
      case (_, EmptySet) => Fix(EmptySet)
      case (x, ZeroS) => x
      case (ZeroS, y) => y
      case (SurrealStructure(xl, xr), SurrealStructure(yl, yr)) =>
        SurrealStructure(
          partialOrder.pmax(plus(xl, y), plus(x, yl)).getOrElse(Fix(EmptySet)),
          partialOrder.pmin(plus(xr, y), plus(x, yr)).getOrElse(Fix(EmptySet))
        )

  def fromSurreal[S](trans: Surreal => S): GCoalgebra[SurrealStructure, Surreal, S] = GCoalgebra {
    case Zero => ZeroS
    case Omega => OmegaS
    case Epsilon => EpsilonS
    case Tau => TauS
    case LeftNumber(l) => LeftS(trans(l))
    case RightNumber(r) => RightS(trans(r))
    case DyadicNumber(l, r) => GeneralS(trans(l), trans(r))
  }

  def fromInt[S](trans: Int => S): GCoalgebra[SurrealStructure, Int, S] = GCoalgebra {
    case 0 => ZeroS
    case Succ(n) => LeftS(trans(n))
    case Prev(n) => RightS(trans(n))
  }

  def intStructure(i: Int): Structure =
    Fix(fromInt(intStructure)(i))

  val StringRep: Algebra[SurrealStructure, String] = Algebra {
    case ZeroS => Strings.Zero
    case EpsilonS => Strings.Epsilon
    case OmegaS => Strings.Omega
    case TauS => Strings.Tau
    case LeftS(l) => s"{ $l | }"
    case RightS(r) => s"{ | $r }"
    case GeneralS(l, r) => s"{ $l | $r }"
  }

private[surreals] object Succ:
  def unapply(n: Int) = if (n > 0) Some(n - 1) else None

private[surreals] object Prev:
  def unapply(n: Int) = if (n < 0) Some(n + 1) else None
