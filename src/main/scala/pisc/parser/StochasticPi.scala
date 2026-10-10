/*
 * Copyright (c) 2023-2026 Sebastian I. Gliţa-Catina <gseba@users.sourceforge.net>
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * [Except as contained in this notice, the name of Sebastian I. Gliţa-Catina
 * shall not be used in advertising or otherwise to promote the sale, use
 * or other dealings in this Software without prior written authorization
 * from Sebastian I. Gliţa-Catina.]
 */

package pisc
package parser

import scala.io.Source

import scala.collection.mutable.{
  LinkedHashMap => Map,
  LinkedHashSet => Set
}

import scala.meta.{ Lit, Term }
import scala.meta.dialects.Scala3

import io.circe.{ Codec, Encoder, Json }

import StochasticPi.*
import Calculus.*
import Directive.Settings
import Encoding.*
import scala.util.parsing.combinator.pisc.parser.Expansion
import Expansion.Duplications


abstract class StochasticPi extends Expression:

  def μ: Parser[μ] = μʹ ^^ (_.asInstanceOf[μ])

  private def μʹ: Parser[Pre] =
    "τ"~opt("@"~>rate) ~ opt( expression ) ^^ { // silent prefix
      case _ ~ r ~ code =>
        val free = code.map(_._2).getOrElse(Names())
        val rʹ = Some(r.getOrElse(None))
        τ(rʹ, code.map(_._1))(sπ_id).free = free
    } |
    name ~ opt("@"~>rate) ~ ("<"~>opt("ν")~name<~">") ~ opt( expression ) ^^ { // negative prefix i.e. output
      case (ch, _) ~ _ ~ _ ~ _  if !ch.isSymbol =>
        throw PrefixChannelParsingException(ch)
      case _ ~ _ ~ (Some(_) ~ (par, _)) ~ _ if !par.isSymbol =>
        throw PrefixChannelParsingException(par)
      case (ch, name) ~ r ~  (ν ~ (arg, free)) ~ Some((it, freeʹ)) =>
        val rʹ = Some(r.getOrElse(None))
        val bound = ν.fold(Names())(_=>free)
        (π(ch, arg, polarity = ν, rʹ, Some(it))(sπ_id).bound = bound).free = name ++ (free ++ freeʹ &~ bound)
      case (ch, name) ~ r ~ (ν ~ (arg, free)) ~ _ =>
        val rʹ = Some(r.getOrElse(None))
        val bound = ν.fold(Names())(_=>free)
        (π(ch, arg, polarity = ν, rʹ, None)(sπ_id).bound = bound).free = name ++ (free &~ bound)
    } |
    name ~ opt("@"~>rate) ~ ("("~>nameʹ<~")") ~ opt( expression ) ^^ { // positive prefix i.e. input
      case (ch, _) ~ _ ~ _ ~ _ if !ch.isSymbol =>
        throw PrefixChannelParsingException(ch)
      case _ ~ _ ~ (par, _) ~ _ if !par.isSymbol =>
        throw PrefixChannelParsingException(par)
      case _ ~ _ ~ _ ~ Some(((Left(enums), _), _)) =>
        throw TermParsingException(enums)
      case (ch, name) ~ r ~ (par, bound) ~ code =>
        val free = code.map(_._2).getOrElse(Names())
        val rʹ = Some(r.getOrElse(None))
        (π(ch, par, polarity = Some(""), rʹ, code.map(_._1))(sπ_id).bound = bound).free = name ++ free
    } |
    name ~ cons_r ~ ("("~>namesʹ<~")") ~ opt( expression ) ^^ { // polyadic unconsing
      case (ch, _) ~ _ ~ _ ~ _ if !ch.isSymbol =>
        throw PrefixChannelParsingException(ch)
      case _ ~ _ ~ params ~ _ if !params.forall(_._1.isSymbol) =>
        throw PrefixChannelsParsingException(params.filterNot(_._1.isSymbol).map(_._1)*)
      case (ch, _) ~ _ ~ params ~ _
          if {
            val paramsʹ = params.filterNot { case (λ(Symbol("")), _) => true case _ => false }
            paramsʹ.size > paramsʹ.distinctBy { case (λ(Symbol(it)), _) => it }.size
          } =>
        throw PrefixChannelsParsingExceptionʹ(ch.asSymbol,
                                              params
                                                .filterNot { case (λ(Symbol("")), _) => true case _ => false }
                                                .map(_._1.asSymbol.name)*)
      case _ ~ _ ~ _ ~ Some(((Left(enums), _), _)) =>
        throw TermParsingException(enums)
      case (λ(ch: Symbol), _) ~ cons ~ params ~ _
          if params.exists { case (λ(`ch`), _) => true case _ => false } =>
        throw ConsItselfParsingException(ch, cons)
      case (ch, name) ~ cons ~ params ~ code =>
        val args = λ(params.map(_._1))
        val bound = params.map(_._2).reduce(_ ++ _)
        val free = code.map(_._2).getOrElse(Names())
        (π(ch, args, polarity = Some(cons), None, code.map(_._1))("").bound = bound).free = name ++ (free &~ bound)
    }

  def name: Parser[(λ, Names)] = ident ^^ (Symbol(_)) ^^ { it => λ(it) -> Set(it) } |
                                 floatingPointNumber ^^ (BigDecimal(_)) ^^ { λ(_) -> Names() } |
                                 stringLiteral ^^ (_.stripPrefix("\"").stripSuffix("\"")) ^^ { λ(_) -> Names() } |
                                 ( "True" | "False" ) ^^ (_ == "True") ^^ { λ(_) -> Names() } |
                                 expression ^^ {
                                   case ((Right(_), term), free) => λ(term) -> free
                                   case ((Left(enums), _), _) => throw TermParsingException(enums)
                                 }

  def rate: Parser[Any] = "("~> floatingPointNumber ~ floatingPointNumber <~")" ^^ { case rate ~ whatIf => (BigDecimal(rate), BigDecimal(whatIf)) } |
                          "∞" ^^ { _ => -1L } |
                          naturalNumber<~"∞" ^^ { -_.toLong } |
                          "⊤" ^^ { _ => 1L } |
                          naturalNumber<~"⊤" ^^ { _.toLong } |
                          floatingPointNumber ^^ { BigDecimal(_) } |
                          super.ident ^^ { Symbol(_) } |
                          expression ^^ {
                            case ((Right(term), _), free) =>
                              if free.nonEmpty
                              then
                                val werr = _settings.werr
                                _settings.werr = false
                                warn(throw RateFreeNamesException(free.map(_.name).toSeq*))
                                _settings.werr = werr
                              term
                            case ((Left(enums), _), _) =>
                              throw TermParsingException(enums)
                          }

  def nameʹ: Parser[(λ, Names)] =
    name ~ opt(`type`) ^^ {
      case (λ, free) ~ tpe =>
        (λ.copy()(using tpe), free)
    }

  def names: Parser[List[(λ, Names)]] =
    rep1sep(name, ",")

  def namesʹ: Parser[List[(λ, Names)]] =
    rep1sep(opt(nameʹ) ^^ { _.getOrElse(λ(Symbol("")) -> Names()) }, ",")

  def scale: Parser[Int] =
    opt( ("0+".r | naturalNumber) <~ "*" ) ^^ {
      case Some(_: String)  => Some(BigInt(0))
      case Some(it: BigInt) => Some(it)
      case _                => None
    } ^^ { _.fold(-1)(_.toInt) }

  def pace: Parser[(Long, String)] =
    naturalNumber ~ opt( ","~> ident ) ^^ {
      case amount ~ unit =>
        amount.toLong -> unit.getOrElse(_settings.paceunit)
    }

  /**
   * Channel names start with lower case.
   * @return
   */
  override def ident: Parser[String] =
      "" ~> // handle whitespace
      rep1(acceptIf(Character.isLowerCase)("channel name expected but '" + _ + "' found"),
          elem("channel name part", { (ch: Char) => Character.isJavaIdentifierPart(ch) || ch == '\'' || ch == '"' })) ^^ (_.mkString)

  def naturalNumber: Parser[BigInt] =
    """\d+""".r ^^ { BigInt(_) } ^? { case it if it > 0 => it }

  protected val emitter: Emitter

  private[parser] var eqtn: List[Bindʹ] = null
  private[parser] var defn: Map[Int, List[Define]] = null
  private[parser] var self: Set[Int] = null
  protected var _nest = -1
  private[parser] var _nth: Map[Int, Long] = null
  protected final def nest(b: Boolean) =
    _nth(_nest) += 1
    _nest += (if b then 1 else -1)
    if b
    then
      _nth(_nest) = 0L
      _cntr(_nest) = 0L
    else
      _nth -= _nest+1
      _cntr -= _nest+1
  private[parser] var _cntr: Map[Int, Long] = null

  private[parser] def pos(bound: Boolean = false) = { _cntr(_nest) += 1; Position(_cntr(_nest), bound) }
  private[parser] def pos_(bound: Boolean = false) = { _cntr(_nest) += 1; Position(-_cntr(_nest), bound) }

  protected final def path = (0 until _nest).map(_nth(_))

  private[parser] var _id: helper.υidυ = null

  private[parser] var _sπ_id: helper.υidυ = null

  private[parser] var _χ_id: helper.υidυ = null

  protected final def id = _id()

  protected final def sπ_id = _sπ_id()

  protected final def χ_id = _χ_id()

  protected final def copy: ((Any, Any), Any) =
    _id.copy -> _sπ_id.copy -> _χ_id.copy

  protected final def paste(it: ((Any, Any), Any)) =
    _id.paste(it._1._1)
    _sπ_id.paste(it._1._2)
    _χ_id.paste(it._2)

  protected final def save[T](r: => ParseResult[T]): Option[(T, Input)] =
    val nest = _nest
    val cntr = Map.from(_cntr)
    _id.save {
      _sπ_id.save {
        _χ_id.save {
          var ok = false
          try
            r match
              case Success(it, in) =>
                ok = true
                Some(it -> in)
              case _ =>
                None
        finally
          if !ok
          then
            _cntr = cntr
            _nest = nest
        }
      }
    }

  protected object BindingOccurrence:
    def apply(names: Names)
             (using Bindings, Int): Unit =
      if _code < 0
      then
        names.foreach(this(_, None, true))
      else
        (names zip names.map(Some(_))).foreach(this(_, _, true))
    def apply(name: Symbol, shadow: Option[Symbol], hardcoded: Boolean = false)
             (using bindings: Bindings, scaling: Int): Unit =
      bindings.get(name) match
        case Some(Occurrence(_, _, true)) =>
          throw UniquenessBindingParsingException(_code, _nest, name, hardcoded, "clobbered")
        case Some(Occurrence(Some(_), Position(counter, true, _), _)) if counter < 0 =>
          throw UniquenessBindingParsingException(_code, _nest, name, hardcoded, "duplicated")
        case Some(Occurrence(_, it @ Position(counter, false, _), _)) if counter < 0 =>
          if scaling != 1 then throw UniquenessBindingParsingException(_code, _nest, name, hardcoded, "scaled")
          bindings += name -> Occurrence(shadow, it.copy(binds = true, path = path))
        case _ =>
          bindings += name -> Occurrence(shadow, pos(true))

  protected object PendingOccurrence:
    def apply(names: Names)
             (using Bindings): Unit =
      names.foreach(this(_))
    def apply(name: Symbol)
             (using bindings: Bindings): Unit =
      bindings.get(name) match
        case Some(Occurrence(Some(_), Position(counter, true, it), false)) if counter < 0 && !path.startsWith(it) =>
          throw ScopeBindingParsingException(_code, _nest, name)
        case Some(it @ Occurrence(None, Position(counter, false, _), false)) if counter < 0 =>
          bindings += name -> it.copy(pending = true)
        case _ =>


object StochasticPi:

  private val cons_r = """[^/*{\[(<.,"'\p{Alnum}@\p{Space}'",.>)\]}*/]+""".r

  enum Emitter(val canScale: Boolean = false,
               val featuresLinearReplication: Boolean = false,
               val hasReplicationInputGuardFlaw: Int => Boolean = { _ => true }):
    def this(featuresLinearReplication: Boolean) =
      this(featuresLinearReplication = featuresLinearReplication,
           hasReplicationInputGuardFlaw = if featuresLinearReplication
                                          then _ >= -1
                                          else _ != 1)
    case ce extends Emitter(true)
    case cef extends Emitter(true)
    case zio extends Emitter(true)
    case ziof extends Emitter(true)
    case fs2 extends Emitter(true)
    case zs extends Emitter(true)
    case kk extends Emitter(canScale = true, hasReplicationInputGuardFlaw = { _ => false })
    private[parser] case test extends Emitter()

  type Actions = Set[String]

  object Actions:
    def apply(ps: Pre*): Actions = Set.from(
      ps
        .filter { case Act(it) => it }
        .headOption
        .map(_.asInstanceOf[Act].υidυ)
    )

  def nil = Actions()


  trait Act(fun: () => String):
    val rate: Option[Any]
    final def id: String = fun()
    final lazy val υidυ: String = id

  object Act:
    def unapply(self: Pre): Some[Boolean] =
      self match
        case it: Act => Some(it.rate.isDefined)
        case _ => Some(false)

  trait Sum { this: + =>
    private var _enabled: Actions = null
    inline def enabled: Actions = _enabled
    inline def enabled_=(enabled: Actions): + =
      _enabled = enabled
      this
  }

  type Names = Set[Symbol]

  object Names:
    def apply(os: λ*): Names = Set.from(os
      .filter(_.isSymbol)
      .map(_.asSymbol)
    )
    def apply(names: Names): Names = Set.from(names)


  trait Bound:
    private var _bound = Names()
    inline final def bound = _bound
    inline final def bound_=(bound: Names): this.type =
      _bound = bound
      this

  object Bound:

    given Encoder[Bound] = it => Json.obj("bound" -> Json.fromValues(it.bound.map(_.name).map(Json.fromString)))

  trait Free:
    private var _free = Names()
    inline final def free = _free
    inline final def free_=(free: Names): this.type =
      _free = free
      this

  object Free:

    given Encoder[Free] = it => Json.obj("free" -> Json.fromValues(it.free.map(_.name).map(Json.fromString)))


  // exceptions

  import scala.meta.Enumerator

  import Expression.ParsingException

  abstract class PrefixParsingException(msg: String, cause: Throwable = null)
      extends ParsingException(msg, cause)

  case class RateFreeNamesException(names: String*)
      extends PrefixParsingException(s"""A rate that is a Scalameta Term may have free names '${names.mkString(", ")}' which are ignored""")

  case object TracesRateException
      extends PrefixParsingException("When traces are active, used rates must not be Scala code")

  case class PrefixChannelParsingException(name: λ)
      extends PrefixParsingException(s"${name.`val`} is not a channel name but a ${name.kind}")

  case class PrefixChannelsParsingExceptionʹ(name: Symbol, ps: String*)
      extends PrefixParsingException(s"""For a polyadic name ${name.name}, the parameters names '${ps.mkString(", ")}' must be distinct""")

  case class TermParsingException(enums: List[Enumerator])
      extends PrefixParsingException(s"The embedded Scalameta should be a Term, not Enumerator `$enums'")

  case class ConsItselfParsingException(name: Symbol, cons: String)
      extends PrefixParsingException(s"A name ${name.name} that knows how to CONS (`$cons') is itself the result")


  // functions

  extension [T <: AST](ast: T)

    def shallow: T =

      ast.map(_.shallow) {

        case it @ `{}`(identifier, pointers, true, params*) =>
          val paramsʹ = pointers.map(λ(_))
          `(*)`(identifier, (params ++ paramsʹ)*)(using it.lc).setPos(it.pos).free = it.free ++ Names(paramsʹ*)

        case it => it

      }


  class Main(override protected val emitter: Emitter,
             override protected val in: String,
             address: String = "localhost",
             parallelism: Int = Int.MaxValue,
             threshold: Int = 0,
             timeout: Int = 123456,
             exit: Boolean = true,
             plugins: List[Settings.Plugin] = Nil
  ) extends Expansion:

    def line(using Duplications): Parser[Either[Bindʹ, Option[Define]]] =
      equation ^^ { Left(_) } | definition ^^ { Right(_) }

    private def ensure(implicit prog: List[Bindʹ]): Unit =
      import helper.Ensure.*

      val i = main

      if i < 0 then throw MainParsingException

      given rec: Map[(String, Int), Int] = Map()
      given rep: Map[Int, Int] = Map()

      prog(i)._3.recursive(using "Main" -> 0 :: Nil)

      if rec.contains("Main" -> 0) then throw MainParsingExceptionʹ

      for
        (i, n) <- rep
      do
        prog(i)._1 match
          case `(*)`(identifier, params*) =>
            warn(throw RecRepParsingException(identifier, params.size, n))

    private def `(*) => +`(prog: List[Bind]): `(*)` => + = {
      case `(*)`(identifier, args*) =>
        prog
          .find {
            case (`(*)`(`identifier`, params*), _) if params.size == args.size => true
            case _ => false
          }
          .get
          ._2
    }

    extension [T <: AST](ast: T)

      inline def τ: Calculus.Pre.τ =
        import Cast.given
        Calculus.Pre.τ(Some(-1L), None)(sπ_id)(using ast.lc).setPos(ast.pos)

      def parse(using excluded: Map[String, Actions]): (T, Actions) =

        inline given Conversion[AST, T] = _.asInstanceOf[T]

        extension [S](self: `.` & S)
          def append_τ(end: + | -, ps: Pre*): (S, Actions) =
            val psʹ = ps :+ end.τ
            self.cc(end = end, prefixes = psʹ).asInstanceOf[S] -> Actions(psʹ*)

        extension (self: +)
          def prepend_τ: + =
            val (pos, free) = self.pos -> self.free
            val (seq, enabled) = (`.`(self).setPos(pos).free = free).append_τ(self)
            val par = ∥(-1, seq).setPos(pos).free = free
            val sum = `+`(-1, par).setPos(pos).free = free
            sum.enabled = enabled

        ast match

          case ∅() =>

            ast.runtimeChecked match

               case it: + =>
                 (it.enabled = nil, it.enabled)

          case it: + =>
            val sum = it.choices.foldLeft(it.cc(choices = Nil).enabled = nil) {
              case (sum: +, par) =>
                val (parʹ, enabledʹ) = par.parse
                assert(enabledʹ.nonEmpty)
                assert((sum.enabled & enabledʹ).isEmpty)
                sum.cc(choices = sum.choices :+ parʹ).enabled = sum.enabled ++ enabledʹ
            }
            (sum, sum.enabled)

          case it: ∥ =>
            val (par, enabled) = it.components.foldLeft((it.cc(components = Nil), nil)) {
              case ((par: ∥, enabled), seq) =>
                val (seqʹ, enabledʹ) = seq.parse
                assert(enabledʹ.nonEmpty)
                assert((enabled & enabledʹ).isEmpty)
                (par.cc(components = par.components :+ seqʹ), enabled ++ enabledʹ)
            }
            (par, enabled)

          case it @ `.`(end, ps*) =>
            val (endʹ, enabled) = end.parse

            if Actions(ps*).nonEmpty
            then
              (it.cc(end = endʹ), Actions(ps*))
            else if enabled.nonEmpty
            then
              (it.cc(end = endʹ), enabled)
            else
              assert(endʹ.isInstanceOf[+] && endʹ.asInstanceOf[+].isVoid || endʹ.isInstanceOf[`(*)`])
              it.append_τ(endʹ, ps*)

          case it @ ?:(_, _t, _f) =>
            def _t_f(_sum: +): + =
              val (sum, _) = _sum.parse

              if sum.enabled.isEmpty
              || sum.enabled.exists(excluded.contains(_))
              then
                sum.prepend_τ
              else
                sum

            val (t, f) = _t_f(_t) -> _f.map(_t_f(_))

            f.foreach { sum => t.enabled.foreach(excluded(_) = sum.enabled) }
            f.foreach(_.enabled.foreach(excluded(_) = t.enabled))

            assert(t.enabled.nonEmpty)
            assert(f.map(_.enabled.nonEmpty).getOrElse(true))

            assert((t.enabled & f.map(_.enabled).getOrElse(nil)).isEmpty)
            (it.cc(t = t, f = f), t.enabled ++ f.map(_.enabled).getOrElse(nil))

          case it @ !(_, _, Some(μ), sum) =>
            val (sumʹ, _) = sum.parse
            (it.cc(sum = sumʹ), Actions(μ))

          case it: ! =>
            val τʹ: τ = it.τ
            it.cc(guard = Some(τʹ.cc()('!' + τʹ.υidυ))).parse

          case it @ `⟦⟧`(Definition(_, _, _, variables, _), _sum, _, pointers) =>
            val n = pointers.size

            val sum =
              if variables.size == n
              then
                _sum
              else
                val names = variables.drop(n)
                val (pos, free) = (_sum.pos, _sum.free -- names)
                val νʹ: ν = ν(names.map(_.name).toSeq*).setPos(pos).bound = names
                val seq = `.`(_sum, νʹ).setPos(pos).free = free
                val par = ∥(-1, seq).setPos(pos).free = free
                `+`(-1, par).setPos(pos).free = free

            var (sumʹ, _) = sum.parse

            if sumʹ.enabled.isEmpty
            then
              sumʹ = sumʹ.prepend_τ

            (it.cc(sum = sumʹ), sumʹ.enabled)

          case _: `{}` => ???

          case it: `(*)` => (it, nil)

      def split(using discarded_excluded: (Map[String, Actions], Map[String, Actions])): Actions =

        ast match

          case ∅() => nil

          case it @ +(_, par) =>
            par.split
            it.enabled

          case it @ +(_, ps*) =>
            val ls = ps.map(_.split)
            var ts = ls.take(0)
            var ds = ls.drop(1)

            val (discarded, _) = discarded_excluded

            ls.foreach { it =>
              assert(it.nonEmpty)
              val ks = (ts ++ ds).reduce(_ ++ _)
              assert((it & ks).isEmpty)
              it.foreach { k =>
                if !discarded.contains(k)
                then
                  discarded(k) = nil
                discarded(k) ++= ks
              }
              ts :+= it
              ds = ds.drop(1)
            }

            it.enabled

          case ∥(_, ss*) =>
            ss.map(_.split).reduce(_ ++ _)

          case `.`(_: `(*)`, ps*) =>
            assert(Actions(ps*).nonEmpty)
            Actions(ps*)

          case `.`(end, ps*) =>
            var enabled = end.split

            if Actions(ps*).nonEmpty then
              enabled = Actions(ps*)

            enabled

          case ?:(_, t, f) =>
            t.split
            f.foreach(_.split)

            val (_, excluded) = discarded_excluded

            t.enabled.foreach { k => assert(!excluded.contains(k)) }
            f.foreach(_.enabled.foreach { k => assert(!excluded.contains(k)) })

            f.foreach { sum => t.enabled.headOption.foreach(excluded(_) = sum.enabled) }
            f.foreach(_.enabled.headOption.foreach(excluded(_) = t.enabled))

            assert((t.enabled & f.map(_.enabled).getOrElse(nil)).isEmpty)
            t.enabled ++ f.map(_.enabled).getOrElse(nil)

          case !(_, _, Some(μ), sum) =>
            sum.split
            Actions(μ)

          case _: ! => ??? // impossible by 'parse'

          case `⟦⟧`(_, sum, _, _) =>
            sum.split
            sum.enabled

          case _: `{}` => ???

          case _: `(*)` => ??? // always "guarded"

      def graph(using prog: List[Bind]): Seq[(Act, Act | Sum)] =

        ast.mapreduce[Seq[(Act, Act | Sum)]] {

          case `.`(end, ps*) =>

            inline given Conversion[(Pre, Act | Sum), (Act, Act | Sum)] = _.asInstanceOf[Act] -> _

            ( for
                i <- 0 until ps.size
                if ps(i) match { case Act(it) => it }
                j = ps.drop(i+1).indexWhere { case Act(it) => it }
                if j >= 0
              yield
                ps(i).asInstanceOf[Act] -> ps(i+1+j).asInstanceOf[Act]
            ) ++
            {
              val k = ps.lastIndexWhere { case Act(it) => it }
              if k >= 0
              then end match
                case sum: + =>
                  Seq(ps(k) -> sum)
                case ?:(_, t, Some(f)) =>
                  Seq(ps(k) -> t, ps(k) -> f)
                case ?:(_, t, _) =>
                  Seq(ps(k) -> t)
                case !(_, _, Some(μ), _) =>
                  Seq(ps(k) -> μ)
                case _: ! => ??? // impossible by 'parse'
                case `⟦⟧`(_, sum, _, _) =>
                  Seq(ps(k) -> sum)
                case _: `{}` => ???
                case it: `(*)` =>
                  val sum = `(*) => +`(prog)(it)
                  Seq(ps(k) -> sum)
              else Nil
            }

          case !(parallelism, _, Some(μ), sum) if parallelism < -1 =>
            Seq(μ -> sum)

          case !(_, _, Some(μ), sum) =>
            Seq(μ -> μ, μ -> sum)

          case _: ! => ??? // impossible by 'parse'

          case _: `{}` => ???

          case _ => Nil

        }(_ ++ _)


    def apply(prog: List[Bindʹ]): (List[Bind], (Map[String, Actions], Map[String, Actions], Map[String, Actions])) =

      given excluded: Map[String, Actions] = Map()

      given List[Bindʹ] = prog.map {
        case it @ (`(*)`("Main"), _, _sum @ ∅()) =>
          val pos = _sum.pos
          val sum = `+`(-1, ∥(-1, `.`(_sum, _sum.τ).setPos(pos)).setPos(pos)).setPos(pos)
          it.copy(_3 = sum.parse._1)
        case it =>
          it.copy(_3 = it._3.shallow.parse._1)
      }

      ensure

      given List[Bind] =
        given_List_Bindʹ
          .map {
            case (bind, _, it @ ∅()) =>
              bind -> it
            case (bind, true, sum) =>
              bind -> sum.labelʹ(using bind.identifier + '#' + bind.params.size)
            case (bind, _, sum) =>
              bind -> sum
          }

      val relabeling = Map[String, String]()

      given_List_Bind
        .foreach {
          case (_, ∅()) =>
          case (_, sum) =>
            def index(end: + | -): Unit =
              end.foreachʹ() {
                case `.`(end, ps*) =>
                  ps.foreach {
                    case it: Act =>
                      val key = it.υidυ
                      val i = key.indexOf(',')
                      if i >= 0
                      then
                        relabeling(key.substring(0, i)) = key
                    case _ =>
                  }
                  index(end)
                case !(_, _, Some(it), sum) =>
                  val key = it.υidυ
                  val i = key.indexOf(',')
                  if i >= 0
                  then
                    relabeling(key.substring(0, i)) = key
                  index(sum)
              }
            index(sum)
        }

      given_List_Bind
        .tapEach {
          _._2.foreach {
            case sum: + =>
              sum.enabled = sum.enabled.map { key => relabeling.getOrElse(key, key) }
            case _ =>
          }
        }

      excluded.clear()

      val discarded = Map[String, Actions]()

      val enabled = Map[String, Actions]()

      given_List_Bind
        .tapEach {
          case (_, sum) =>
            sum.split(using discarded -> excluded)

            sum.graph.foreach { (s, t) =>
              if !enabled.contains(s.υidυ)
              then
                enabled(s.υidυ) = nil
              t match
                case it: Act =>
                  enabled(s.υidυ) += it.υidυ
                case it: Sum =>
                  enabled(s.υidυ) ++= it.enabled
            }
        } -> (discarded, excluded, enabled)


    private var i: Int = -1
    private var l: (Int, Int) = (-1, -1)
    override def ln: String = if l._1 == l._2 then s"line #${l._2}" else s"lines #${l._1}-#${l._2}"

    protected def _init: Unit =
      _settings = Settings(elastic4s = Settings.Elastic4s("http", address),
                           parameters = Settings.Parameters(address, parallelism, threshold, timeout, exit, plugins))
      Directive("push" -> "1", emitter, _settings)()
      eqtn = List()
      defn = Map()
      self = Set()
      _nest = 0
      _id = new helper.υidυ
      _sπ_id = new helper.υidυ
      _χ_id = new helper.υidυ
      i = 0
      l = (0, 0)
      _last = None

    def apply(source: Source, errors: Boolean = false): List[Either[String, Bindʹ]] =
      _init
      _settings.werr = errors

      given Duplications()

      val r =
        (source.getLines() ++ Some(""))
          .zipWithIndex
          .foldLeft(List[((Int, Int), List[String])]() -> false) {
            case ((r, false), (t, n)) => (r :+ ((n, n) -> List(t.stripSuffix("\\")))) -> t.endsWith("\\")
            case ((r, true), (t, n))  => (r.init :+ ((r.last._1._1, n) -> (r.last._2 :+ t.stripSuffix("\\")))) -> t.endsWith("\\")
          }._1
          .flatMap { case ((m, n), itʹ) =>
            lc = { col =>
              var i = 0
              var l = 0
              while itʹ(i).length + l < col
              do
                l += itʹ(i).length
                i += 1
              (m+i+1, (if l == 0 then col else col%l)+1)
            }
            val it = itʹ.mkString
            l = (m+1, n+1)
            if it.matches("^[ ]*#.*") // commented lines
            || it.isBlank // empty lines
            then
              None
            else if it.matches("^[ ]*@.*")
            then // Scala
              if !_settings.exclude
              then
                Some(Left(it.replaceFirst("^([ ]*)@(.*)$", "$1$2")))
              else
                None
            else // StochasticPi
              _cntr = Map(0 -> 0L)
              _nth = Map(0 -> 0L)
              parseAll(line, it) match
                case Success(Left(equation), _) =>
                  if !_settings.exclude
                  then
                    eqtn :+= equation
                    val equations = eqtn.slice(i, eqtn.size)
                    i = eqtn.size
                    equations.map(Right(_))
                  else
                    Nil
                case Success(Right(Some(definition)), _) =>
                  if !defn.contains(_code) then defn(_code) = Nil
                  defn(_code) ::= definition
                  Nil
                case Success(Right(_), _) => // directive | excluded
                  Nil
                case failure: NoSuccess =>
                  scala.sys.error(failure.msg)
          }
          .filter {
            case Right((`(*)`(s"Self_$n", _*), _, _))
                if { try { n.toInt; true } catch _ => false } =>
              self.contains(n.toInt)
            case _ => true
          }

      val prog =
        ( if i < eqtn.size && !_settings.exclude
          then
            r ::: eqtn.slice(i, eqtn.size).map(Right(_))
          else
            r
        ).filter {
          case Right((`(*)`(s"Self_$n", _*), _, _))
              if { try { n.toInt; true } catch _ => false } =>
            self.contains(n.toInt)
          case _ => true
        }

      _last = None

      Right((`(*)`(null, λ(if _settings.typeclasses.isEmpty then Lit.Null() else Term.Tuple(_settings.typeclasses.map(Term.Name(_))))), false, ∅()): Bindʹ) ::
      Right((`(*)`(null, λ(_settings.elastic4s)), false, ∅()): Bindʹ) ::
      Right((`(*)`(null, λ(_settings.parameters.reify)), false, ∅()): Bindʹ) ::
      Right((`(*)`(null, λ(_settings.traces.fold(Lit.Null())(_.reify))), false, ∅()): Bindʹ) ::
      prog
