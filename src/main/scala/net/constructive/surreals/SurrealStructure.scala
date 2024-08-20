package net.constructive.surreals

import cats.{Functor, PartialOrder}
import higherkindness.droste.*
import higherkindness.droste.data.Fix
import com.github.blemale.scaffeine.{ Cache, Scaffeine }

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
      case (LeftS(xl), _) if xl == y => false
      case (_, LeftS(yl)) if x == yl => true
      case (RightS(xr), _) if xr == y => true
      case (_, RightS(yr)) if x == yr => false
      case (EmptySet, _) | (_, EmptySet) => true
      case (EpsilonS, s) =>  lteqEpsilon(s)
      case (OmegaS, s) =>  lteqOmega(s)
      case (TauS, s) =>  lteqTau(s)
      case (SurrealStructure(xl:Structure, _),
            SurrealStructure(_, yr:Structure)) =>
        gt(y, xl) && gt(yr, x)

    inline def lteqEpsilon(s: Structure): Boolean = s match {
      case EmptySet => true
      case ZeroS => true
      case EpsilonS => true
      case OmegaS => false
      case TauS => false
      case SurrealStructure(_, r: Structure) => gt(r, Fix(EpsilonS))
    }

    inline def lteqOmega(s: Structure): Boolean = s match {
      case EmptySet => true
      case ZeroS => true
      case EpsilonS => true
      case OmegaS => true
      case TauS => false
      case SurrealStructure(_, r: Structure) => gt(r, Fix(OmegaS))
    }

    inline def lteqTau(s: Structure): Boolean = s match {
      case EmptySet => true
      case ZeroS => true
      case OmegaS => true
      case EpsilonS => true
      case TauS => true
      case SurrealStructure(_, r: Structure) => gt(r, Fix(TauS))
    }

    val gtCache: Cache[(Structure, Structure), Boolean] =
      Scaffeine().recordStats().maximumSize(100000).build()

    override def gt(x: Structure, y: Structure): Boolean =
      gtCache.getIfPresent(x -> y).getOrElse(setGT(x, y))

    def setGT(x: Structure, y: Structure): Boolean = {
      val gt = calcGT(x, y)
      gtCache.put(x -> y, gt)
      gt
    }

    def calcGT(x: Structure, y: Structure): Boolean = (x, y) match
      case (EmptySet, SurrealStructure(_, yr:Structure)) => lteqv(yr, x)
      case (SurrealStructure(xl:Structure, _), EmptySet) => lteqv(y, xl)
      case _ => !lteqv(x, y)

    def pickMax(x: Structure, y: Structure): Structure = (x, y) match
      case (EmptySet, p) => p
      case (p, EmptySet) => p
      case _ if lteqv(x, y) => y
      case _ => x

    def pickMin(x: Structure, y: Structure): Structure = (x, y) match
      case (EmptySet, p) => p
      case (p, EmptySet) => p
      case _ if lteqv(x, y) => x
      case _ => y
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

  val negateCache: Cache[Structure, Structure] =
    Scaffeine().recordStats().maximumSize(100000).build()

  def negate(x: Structure): Structure =
    negateCache.getIfPresent(x).getOrElse(setNegate(x))

  def setNegate(x: Structure): Structure = {
    val neg = calcNegate(x)
    negateCache.put(x, neg)
    neg
  }

  def calcNegate(x: Structure): Structure =
    x match
      case EmptySet => Fix(EmptySet)
      case ZeroS => Fix(ZeroS)
      case SurrealStructure(xl, xr) =>
        SurrealStructure(negate(xr), negate(xl))

  val plusCache: Cache[(Structure, Structure), Structure] =
    Scaffeine().recordStats().maximumSize(100000).build()

  def plus(x: Structure, y: Structure): Structure =
    plusCache.getIfPresent(x -> y).getOrElse(setPlus(x, y))

  def setPlus(x: Structure, y: Structure): Structure = {
    val plus = calcPlus(x, y)
    plusCache.put(x -> y, plus)
    plus
  }

  def calcPlus(x: Structure, y: Structure): Structure =
    (x, y) match
      case (EmptySet, _) => Fix(EmptySet)
      case (_, EmptySet) => Fix(EmptySet)
      case (x, ZeroS) => x
      case (ZeroS, y) => y
      case (SurrealStructure(xl, xr), SurrealStructure(yl, yr)) =>
        val xl_y = plus(xl, y)
        val x_yl = plus(x, yl)
        val xr_y = plus(xr, y)
        val x_yr = plus(x, yr)

        val rl = partialOrder.pmax(xl_y, x_yl)
        val rr = partialOrder.pmin(xr_y, x_yr)

        SurrealStructure(rl.getOrElse(Fix(EmptySet)), rr.getOrElse(Fix(EmptySet)))

  val timesCache: Cache[(Structure, Structure), Structure] =
    Scaffeine().recordStats().maximumSize(100000).build()

  def times(x: Structure, y: Structure): Structure =
    timesCache.getIfPresent(x -> y).getOrElse(setTimes(x, y))

  def setTimes(x: Structure, y: Structure): Structure = {
    val times = calcTimes(x, y)
    timesCache.put(x -> y, times)
    times
  }

  def calcTimes(x: Structure, y: Structure): Structure =
    (x, y) match
      case (EmptySet, _) => Fix(EmptySet)
      case (_, EmptySet) => Fix(EmptySet)
      case (x, ZeroS) => Fix(ZeroS)
      case (ZeroS, y) => Fix(ZeroS)
      case (x, LeftS(ZeroS)) => x
      case (LeftS(ZeroS), y) => y
      case (x, RightS(ZeroS)) => negate(x)
      case (RightS(ZeroS), y) => negate(y)
      case (SurrealStructure(xl, xr), SurrealStructure(yl, yr)) =>
        val xl_y = times(xl, y)
        val x_yl = times(x, yl)
        val xl_yl = times(xl, yl)

        val xr_y = times(xr, y)
        val x_yr = times(x, yr)
        val xr_yr = times(xr, yr)

        val xl_yr = times(xl, yr)
        val xr_yl = times(xr, yl)

        val rla = plus(plus(xl_y, x_yl), negate(xl_yl))
        val rlb = plus(plus(xr_y, x_yr), negate(xr_yr))
        val rl = partialOrder.pickMax(rla, rlb)

        val rra = plus(plus(xl_y, x_yr), negate(xl_yr))
        val rrb = plus(plus(xr_y, x_yl), negate(xr_yl))
        val rr = partialOrder.pickMin(rra, rrb)

        SurrealStructure(rl, rr)

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
