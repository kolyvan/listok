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

import collection.mutable.ArraySeq

//import java.io._

object Util {

  def pp(l: Seq[Lcommon], prefix:String="(", sep:String=" ", suffix: String=")"): String = {
    val sb = new StringBuilder
    sb ++= prefix

    /*
    l match {
      case Nil =>
      case x::xs =>
        sb ++= x.pp
        xs.foreach { x =>
          sb ++= sep
          sb ++= x.pp
        }
    }
    */
    if (l.nonEmpty) {
      sb ++= l.head.pp
      l.tail.foreach { x =>
        sb ++= sep
        sb ++= x.pp
      }
    }

    sb ++= suffix
    sb.toString
  }


  def isTrue(p: Lcommon) = p match {
    case Lnil => false
    case Llist(Nil) => false
    // ???? case Lsymbol(s) => isTrue(s.eval)
    //case fn: Lfunction => error("BUG.isTrue on function")
    //case sf: Lsform => error("BUG.isTrue on sform")
    //case m: Lmacro => error("BUG.isTrue on macro")
    case _ => true
  }

  def toLbool(b: Boolean): Lcommon = b match {
    case true => Ltrue
    case false => Lnil
  }

  // http://stackoverflow.com/questions/2601611/zip-elements-with-odd-and-even-indices-in-a-list/2602071#2602071
  def pairify[T](list: List[T]): List[(T, T)] = list match {
    case Nil => Nil
    case x :: y :: xs => (x, y) :: pairify(xs)
    case _ => bugcheck("odd length list!")
  }

  def pp(sym: Symbol) = sym.toString.substring(1)

  def ppcall(name: Symbol, args: List[Lcommon]) =
      "(" + pp(name) + args.foldLeft("")((s,p) => s + " " + p.pp) + ")"

  def backtrace(env: Env, expandAll: Boolean = true) = {
    val lb = new scala.collection.mutable.ListBuffer[Env]
    var p = env
    while (p != null) {
      lb += p
      p = if (expandAll) { if (p.getCaller == null) p.parent else p.getCaller }
      else  p.parent
    }
    lb.toList
  }

  def printBacktrace(env: Env) {
    println(" backtrace: ")
    backtrace(env).foreach { p =>
      println("  in " + p.pp + "\t: " + p.backtrace)
    }
  }

  // unescape /nbftr
  def unescape(s: String) = {

    val sb = new StringBuilder
    val it = s.iterator
    var backslash = 0

    while (it.hasNext) {
      val ch = it.next
      ch match {
        case '\\' =>
          if (backslash == 1) {
            backslash = 0
            sb.append('\\')
          }
          else
            backslash += 1

        case 'n' =>
          if (backslash == 1) {
            backslash = 0
            sb.append('\n')
          }
          else
            sb.append(ch)

        case 'b' =>
          if (backslash == 1) {
            backslash = 0
            sb.append('\b')
          }
          else
            sb.append(ch)

        case 't' =>
          if (backslash == 1) {
            backslash = 0
            sb.append('\t')
          }
          else
            sb.append(ch)

       case 'f' =>
          if (backslash == 1) {
            backslash = 0
            sb.append('\f')
          }
          else
            sb.append(ch)


       case 'r' =>
          if (backslash == 1) {
            backslash = 0
            sb.append('\r')
          }
          else
            sb.append(ch)

        case _ =>
          sb.append(ch)
      }
    }
    sb.toString
  }

  def mapTree(l: List[Lcommon])(fn: (Lcommon) => Lcommon ): List[Lcommon] = {

    def atom(p: Lcommon): Lcommon = p match {
      case Llist(Nil) => Lnil
      case Llist(xs) => Llist(atom(xs.head) :: list(xs.tail))
      case Llambda(ll, body, name) => Llambda(ll, list(body), name)
      case _ => fn(p)
    }

    def list(l: List[Lcommon]): List[Lcommon] = l map { atom _ }

    list(l)
  }

  def traverseTree(l: List[Lcommon])(fn: (Lcommon) => Lcommon ) {

    def atom(p: Lcommon) { p match {
      case Llist(Nil) =>
      case Llist(xs) => atom(xs.head); list(xs.tail)
      // case Llambda(ll, body, name) => list(body)
      case _ => fn(p)
    }}

    def list(l: List[Lcommon]) { l foreach { atom _ } }

    list(l)
  }


  // drop the cached info from symbols

  def sanitizeSymbols (l: List[Lcommon]): List[Lcommon] =
    Util.mapTree(l) {
      // case Llambda(ll, body, name) => Llambda(ll, dup(body), name)
      case s: Lsymbol => s.sanitize
      case x => x
    }

  def sanitizeSymbols(p: Lcommon):Lcommon = p match {
    case Llist(xs) => Llist(sanitizeSymbols(xs))
    case s: Lsymbol => s.sanitize
    case x => x
  }


  //

   def toLcommon(x: Any): Lcommon = x match {
    case b: Byte => Lint(b.toInt)
    case s: Short => Lint(s.toInt)
    case i: Int => Lint(i)
    case l: Long => Lint(l.toInt)
    case f: Float => Lfloat(f)
    case d: Double => Lfloat(d.toFloat)
    case s: String => Lstring(s)
    case s: Symbol => Lkeyword(s)
    case c: Char => Lchar(c)
    case true => Ltrue
    case Nil => Lnil
    case (a,b) => Lpair(toLcommon(a),toLcommon(b))
    case l: List[_] => toLlist(l:_*)
    case a: Array[_] => toLvector(a:_*)
    case m: Map[_, _] => toLhashmap(m.toSeq :_*)
    case x: Lcommon => x
    case null => Lnil
    case err =>
      bugcheck("Unable convert from " +err+ " to listok type")
      //Lnil
  }

  def toLlist(l: Any*) = Llist(l.map(toLcommon).toList)
  def toLvector(l: Any*) = Lvector(ArraySeq( l.map(toLcommon):_* ))
  def toLhashmap(l: (Any, Any)*) = Lhashmap(Map(l.map { p => (p._1.toString -> toLcommon(p._2))} :_*))

}





