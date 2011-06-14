/*
	Listok is a dialect of LISP 
	Copyright (C) 2011 Konstantin Boukreev
	ru.kolyvan@gmail.com

	This file is part of Listok.

	Listok is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public
	License as published by the Free Software Foundation; either
	version 3 of the License, or (at your option) any later version.

	Listok is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
	Lesser General Public License for more details.

	You should have received a copy of the GNU Lesser General Public License
	along with this library. If not, see <http://www.gnu.org/licenses/>.
*/

package ru.listok


abstract sealed class MatchResult
object MatchNotFound extends MatchResult
case class MatchValueResult(value: Lcommon) extends  MatchResult
case class MatchSymbolResult(symbol: Symbol, value: Lcommon) extends  MatchResult
case class MatchSeqResult(pairs: List[(Symbol, Lcommon)]) extends  MatchResult


object PatternMatching {

  def runMatches(env: Env, patterns : List[Lcommon], value: Lcommon): Lcommon = {

    var p = patterns
    while (p != Nil) {

      p.head match {
        case Llist(xs) =>
          xs match {
            case Nil => // skip empty
            case x::xs =>
              runMatch(env, x, value) match {
                case MatchNotFound => // continue

                case MatchValueResult(_) =>
                  return Listok.eval(env, xs)

                case MatchSymbolResult(s, r) =>
                  return LambdaCalculus.makeCall('match, env, List(s), xs, List(r))

                case MatchSeqResult(l) =>
                  val (ll,args) = l.unzip
                  return LambdaCalculus.makeCall('match, env, ll, xs, args)
              }
          }

        case any =>
          return any
      }

      p = p.tail
    }

    Lnil
  }

  def runMatch (env: Env, pattern : Lcommon, value: Lcommon): MatchResult =
    pattern match {
      case Llist(xs) => formMatch(env, xs, value)
      case s: Lsymbol => trivialMatch(env, s.eval(env), value)
      case r: Lregex => regexMatch(env, r, Nil, value)
      case bq: Lmacrobackquote => runMatch(env, bq.eval(env), value)
      case lmb: Llambda => trivialMatch(env, lmb.eval(env), value)
      case _ => trivialMatch(env, dequote(env, pattern), value)
    }

  def trivialMatch(env: Env, pattern : Lcommon, value: Lcommon): MatchResult =
    pattern match {
      case fn: Lfunction =>
           call(env, fn, value) match {
             case Lnil => MatchNotFound
             case x => MatchValueResult(x)
           }
      case r: Lregex => regexMatch(env, r, Nil, value)
      case _ =>
        if (pattern == value)
          MatchValueResult(pattern)
        else
          MatchNotFound
    }


  def formMatch(env: Env, pattern : List[Lcommon], value: Lcommon): MatchResult =
    if (pattern == Nil)
      MatchNotFound
    else {
      pattern.head match {
        case lmb: Llambda => formMatch(env, lmb.eval(env) :: pattern.tail, value)
        case Lsymbol('list) => seqMatch(env, pattern.tail, value)
        case Lsymbol('vector) => seqMatch(env, pattern.tail, value)
        case Lsymbol('pair) => pairMatch(env, pattern.tail, value)
     //   case Lsymbol('regex) => regexMatch(env, pattern.tail, value)
        case r: Lregex => regexMatch(env, r, pattern.tail, value)
        case s: Lsymbol =>
          s.eval(env) match {
            case fn: Lfunction => formMatch(env, fn :: pattern.tail, value)
            case r: Lregex => regexMatch(env, r, pattern.tail, value)
            case _ => MatchNotFound // only functions as symbols allowed
          }
        case fn: Lfunction =>
          call(env, fn, value) match { // run predicat
            case Lnil => MatchNotFound
            case r =>
              pattern(1) match {
                case Lsymbol(sym) => MatchSymbolResult(sym, value)
                case _ => trivialMatch(env, r, value) // in case of : (length 1)
              }
            }
        case err => throw SyntaxError("invalid matching pattern " + err.pp, env)
      }
    }

  // (list 1 2 x)
  def seqMatch(env: Env, pattern : List[Lcommon], value: Lcommon): MatchResult =
    value match {
      case Lnil if pattern == Nil => MatchValueResult(Lnil)
     // case Llist(xs) =>
      case s:Lseq => checkList(env, pattern, s.seq.toList, false)
      case _ => MatchNotFound
    }

  def pairMatch(env: Env, pattern : List[Lcommon], value: Lcommon): MatchResult = {

    // log("match pair " + Util.pp(pattern) + " againts " + value.pp)

    value match {
      case Lpair(a,b) =>
        if (pattern.length != 2)
          throw SyntaxError("invalid matching pattern for pair " + Util.pp(pattern), env)

        val pa = dequote(env, pattern(0))
        val pb = dequote(env, pattern(1))

        (pattern(0), pattern(1)) match {
          case (Lsymbol(sa), Lsymbol(sb))  => MatchSeqResult(List(sa -> a, sb -> b))
          case (Lsymbol(sa), _) if pb == b => MatchSymbolResult(sa, a)
          case (_, Lsymbol(sb)) if pa == a => MatchSymbolResult(sb, b)
          case _ =>
            if (pb == b && pa == a)
              MatchValueResult(value)
            else
              MatchNotFound
          }

      case _ => MatchNotFound
    }
  }

  def regexMatch(env: Env, regex: Lregex, pattern : List[Lcommon], value: Lcommon): MatchResult = {
    value match {
      case Lstring(s) =>
        //log("match regex " + regex.regex + " on " + s)
        builtin.Regex.matches(regex.pattern, s) match {
          case Lnil => MatchNotFound
          case l: Llist =>
            if (pattern == Nil)
              MatchValueResult(l)
            else {
              if (pattern.length != l.length)
                throw SyntaxError("invalid matching pattern for regex " + Util.pp(pattern), env)
              checkList(env, pattern, l.seq, true)
            }
          case _ => MatchNotFound
        }
      case _ => MatchNotFound
    }
  }

  private def dequote(env: Env, p: Lcommon) = p match {
    case Lquote(f) => f
    case bq: Lmacrobackquote => bq.eval(env)
    case x => x
  }

  private def call(env: Env, fn: Lfunction, value: Lcommon) = {
    try { fn.lapply(env, List(value)) }
    catch { case e: TypeError => Lnil }
  }

  private def checkList(env: Env, pattern : List[Lcommon], xs: List[Lcommon], dropTail: Boolean): MatchResult = {

    val result = List.newBuilder[(Symbol, Lcommon)]
    var pp = pattern
    var pv = xs
    while (pp != Nil && pv != Nil) {
      pp.head match {
        case Lsymbol(sym) => // if pattern is a symbol like in (list x xs)
          if (dropTail) {
            result += (sym -> pv.head)
          } else {
            if (pp.tail == Nil)
              result += (sym -> Llist(pv)) // pattern is end, so take it all
             else
              result += (sym -> pv.head)   // pattern is not end, so head
          }
        case _ =>
          if (dequote(env, pp.head) != pv.head)
            return MatchNotFound // stop
      }
      pp = pp.tail
      pv = pv.tail
    }

    MatchSeqResult(result.result)
  }
}

