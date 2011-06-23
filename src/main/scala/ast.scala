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

import scala.collection.mutable.{Map => MMap}
import scala.collection.mutable.ArraySeq
import java.io.{OutputStream, InputStream, PrintStream, BufferedReader}
import java.util.regex.{ Pattern }
import net.fyrie.ratio.Ratio

// adt

abstract sealed class Lcommon extends Typecast {
  def eval(env: Env): Lcommon = this
  def pp: String  // pretty print
  // def dump = pp
}

abstract class Lruntime extends Lcommon // non-persistent

abstract class Latom extends Lcommon

abstract class Lseq extends Lcommon {
  def cons(x: Lcommon): Lseq
  def seq: Seq[Lcommon]
  def tail: Lcommon = if (length > 1) make(seq.tail) else empty
  def isEmpty = seq.isEmpty
  def length = seq.length
  def make(seq: Seq[Lcommon]): Lcommon
  def empty: Lcommon
}

abstract class Lnumeric extends Latom {
  def float: Double
  def int: Int
  def long: Long
  def bigint: BigInt
  def scalaNumber: scala.math.ScalaNumber
  def negate: Lnumeric
  def abs: Lnumeric
}

//case class Lbyte(val byte: Byte) extends Latom {
  //def pp = byte.toString
//}

// Lint Llong - fixnum

case class Lint(val int: Int) extends Lnumeric {
  def float = int.toDouble
  def long = int.toLong
  def bigint = BigInt(int)
  def pp = int.toString
  def scalaNumber = int
  def negate = Lint(-int)
  def abs = Lint(int.abs)
}

case class Llong(val long: Long) extends Lnumeric {
  def int = long.toInt
  def float = long.toDouble
  def bigint = BigInt(long)
  def pp = long.toString
  def scalaNumber = long
  def negate = Llong(-long)
  def abs = Llong(long.abs)
}

case class Lbignum(val bigint: BigInt) extends Lnumeric {
  def int = bigint.toInt
  def long = bigint.toLong
  def float = bigint.toDouble
  def pp = bigint.toString
  def scalaNumber = bigint
  def negate = Lbignum(-bigint)
  def abs = Lbignum(bigint.abs)
}

case class Lfloat(val float: Double) extends Lnumeric {
  def int = float.toInt
  def long = float.toLong
  def bigint = BigInt(int)
  def pp = float.toString
  def scalaNumber = float
  def negate = Lfloat(-float)
  def abs = Lfloat(float.abs)
}

case class Lratio(val ratio: Ratio) extends Lnumeric {
  def int = ratio.intValue
  def long = ratio.longValue
  def float = ratio.doubleValue
  def bigint = ratio.n / ratio.d
  def pp = ratio.toString
  def scalaNumber = ratio
  def negate = Lratio(Ratio(-ratio.n, ratio.d))
  def abs = Lratio(Ratio(ratio.n.abs, ratio.d))
}

case class Lchar(val char: Char) extends Latom {
  def pp = "#\\" + char
}

case class Lstring(val str: String) extends Lseq {
  def pp = "\"" + str + "\""
  def cons(x: Lcommon): Lstring = Lstring(Util.toStr(x) + str)
  def seq: Seq[Lcommon] = str.map { Lchar(_) }
  def make(s: Seq[Lcommon]): Lstring = Lstring(s.foldLeft("")(_ + Util.toStr(_)))
  def empty: Lstring = Lstring("")
  override def isEmpty = str.isEmpty
  override def length = str.length
}

object LL {
  def apply(xs: Lcommon*) = Llist(xs.toList)
}

case class Llist(val seq: List[Lcommon]) extends Lseq {
  def pp = Util.pp(seq)
  override  def eval(env: Env):Lcommon = seq match {
    case Nil => Lnil
    case x::xs => Listok.lapply(env, x, xs)
  }
  def cons(x: Lcommon): Lseq = Llist(x :: seq)
  def make(seq: Seq[Lcommon]): Lcommon = seq match {
    case Nil => Lnil
    case _ => Llist(seq.toList)
  }
  def empty = Lnil
}

case class Lquote (val form: Lcommon) extends Lcommon {
  def pp = "(quote " + form.pp +")"
  override def eval(e: Env) = form
}

case class Lsymbol (val sym: Symbol) extends Latom  {
  private var cached: Value = null
  private def lookup(env: Env) = {
    if (cached == null)
      cached = env.lookup(sym)
    cached
   //env.lookup(sym)
  }
  override def eval(env: Env):Lcommon = lookup(env).get(env, sym)
  def set(env: Env, v: Lcommon) = lookup(env).set(env, sym, v)
 // def pp = sym.toString.substring(1)

  def pp = sym.toString.substring(1)

  def dump = pp +
    (if (cached!=null)
      cached match {
        case GlobalValue => ".g"
        case LocalValue(i) => ".l" + i
        case ReadonlyValue(v) => ".r" + v.pp
      } else ".-" )


  def sanitize: Lsymbol = cached match {
    case GlobalValue => this
    case r: ReadonlyValue => this
    case _ => Lsymbol(sym) // local value and null
  }
}

case class Llambda(val lambda_list: List[Lcommon], body: List[Lcommon], val name: Symbol = 'lambda)
  extends Latom {
  override def pp = "(lambda " + Util.pp(lambda_list) + Util.pp(body, " ")
  override def eval(env: Env):Lcommon = LambdaCalculus.make(env, name, lambda_list, body)
}

case object Lnil extends Lcommon {
  val pp = "nil"
}

case object Ltrue extends Latom {
  val pp = "t"
}

case class Lfunction(val fn: FuncCall, val name: Symbol) extends Latom {
  def lapply(env: Env, args: List[Lcommon]): Lcommon = fn(env, args)
  def pp = "#<function " + Util.pp(name) + ">"
  // def dump = "#<function " + Util.pp(name) + " {" + this.hashCode() + "}>"
}

case class Lsform(val form: FuncCall, val name: Symbol) extends Latom {
  def lapply(env: Env, args: List[Lcommon]): Lcommon = form(env, args)
  override def eval(env: Env):Lcommon = throw SyntaxError("cannot evaluate a special form", env)
  def pp = Util.pp(name)
}

case class Lkeyword(val sym: Symbol) extends Latom {
  def pp = ":" + Util.pp(sym)
}

case class Ldefmacro (val name: Symbol, val lambda_list: List[Lcommon], body: List[Lcommon]) extends Lcommon {
  def pp = "(defmacro " + Util.pp(name) + Util.pp(lambda_list) +
      Util.pp(body, "", " ", "") +")"

  override def eval(env: Env):Lcommon = {
    val fn = LambdaCalculus.makeFunc(env, 'macro, lambda_list, body)
    env.defineconst(name, Lmacro(fn, name))
    Lnil
  }
}

case class Lmacrobackquote (val form: Lcommon) extends Lcommon {
  def pp = "`" + form.pp
  override def eval(e: Env) = Macro.backquote(e, form)
}

case class Lmacrocomma (val form: Lcommon, val spliced: Boolean = false) extends Lcommon {
  def pp = (if (spliced) ",@" else ",") + form.pp
  //override def eval(e: Env) = form.eval(e)
  override def eval(e: Env) = Util.sanitizeSymbols(form.eval(e))
}

case class Lmacro(val fn: FuncCall, val name: Symbol) extends Latom {
  def pp = "#<macro " + Util.pp(name) +">"
  def lapply(env: Env, args: List[Lcommon]): Lcommon =
    fn(env, args).eval(env)
}

case class Ltcostub(val fn: Lfunction, val args: List[Lcommon]) extends Lruntime {
  def pp = Util.pp(args, "(tcostub " + fn.pp)
  override def eval(e: Env) = bugcheck("tcostub.eval")
}

case class Lvector(val array: ArraySeq[Lcommon]) extends Lseq {
  def pp = Util.pp(seq, "#(")
  override  def eval(env: Env):Lcommon = this
  def seq: Seq[Lcommon] = array
  def cons(x: Lcommon): Lseq = Lvector(array.+:(x))
  def make(seq: Seq[Lcommon]): Lvector = Lvector(ArraySeq(seq:_*))
  def empty: Lvector = Lvector(ArraySeq.empty)
}

case class Lhashtable(val map: MMap[Lcommon, Lcommon]) extends Latom {
  def pp = "#<hash-table :count " + map.size +">"
}

case class Lthread(val thread: ThreadWrapper) extends  Lruntime  {
  def pp = "#<thread " + thread.getName + " " + thread.getState + " tid: " + thread.getId + " >"
}

case class Lmailslot(val m: Mailslot) extends  Lruntime {
  def pp = "#<mailstot " + Util.pp(m.name) + " count: " +m.size+" >"
}

case class Lstream ( val reader: BufferedReader, val writer: PrintStream,
                     val in: InputStream=null, val out: OutputStream =null) extends Lruntime {

  private var closed = false
  //val reader = if (in != null) new BufferedReader(new InputStreamReader(in)) else null
  //val writer =  if (out != null) new PrintStream(out) else null

  def pp = "#<stream " + direction + ">"

  def direction = (reader, writer) match {
    case (null, null) => 'failed
    case (_, null) => 'input
    case (null, _) => 'output
    case _ => 'io
  }

  def canRead = reader != null
  def canWrite = writer != null
  def isClosed = closed

  def close {
    closed = true
    if (in != null) in.close
    if (out != null) out.close
    if (reader != null) reader.close
    if (writer != null) writer.close
  }

  def checkForWrite(env: Env) {
    if (!canWrite)
      throw SyntaxError("Unable write to " + direction + " stream", env)
    if (isClosed)
      throw SyntaxError("The stream is closed", env)
  }

  def checkForRead(env: Env) {
    if (!canRead)
      throw SyntaxError("Unable read from " + direction + " stream", env)
    if (isClosed)
      throw SyntaxError("The stream is closed", env)
  }
}

case class Lpair (val a: Lcommon, val b: Lcommon) extends Latom {
  def pp = "(pair " + a.pp + " " + b.pp + ")"
}

case class Lhashmap(val map: Map[String, Lcommon]) extends Lseq {

  def pp = "(hashmap" + map.foldLeft(""){ (s,t) => s + " (" + t._1 + " " + t._2.pp +")" } + ")"
  override def eval(env: Env):Lcommon = this

  /// lseq
  override def isEmpty = map.isEmpty
  override def length = map.size
  def seq = mapToseq
  // def tail: Lcommon = if (length > 1) make(seq.tail) else empty
  def cons(x: Lcommon) = Lhashmap(map + commonToTuple(x))
  def make(seq: Seq[Lcommon]) = Lhashmap(seqTomap(seq))
  def empty = Lhashmap(Map.empty)

  /// hashmap
  def getOrElse(k: Lcommon, notFound: Lcommon) = map.getOrElse(commonTokey(k), notFound)
  def add(k: Lcommon, v: Lcommon) = Lhashmap(map + (commonTokey(k) -> v))
  def del(k: Lcommon) = Lhashmap(map - commonTokey(k))

  /// private
  private def commonToTuple(c: Lcommon) = c match {
    case p: Lpair => (commonTokey(p.a), p.b)
    case l: Lseq if l.length == 2 => (commonTokey(l.seq(0)), l.seq(1))
    case x => (commonTokey(x), x)
  }
  private def commonTokey(p: Lcommon):String = p match {
    case Lkeyword(k) => Util.pp(k) // k.toString
    case Lstring(s)  => s
    case Lsymbol(s)  => Util.pp(s)  // :A 'A "A" is equal
    case Lint(i) => i.toString
    case Lchar(ch) => ch.toString
    case _ => p.pp
  }

  private def seqTomap (l: Seq[Lcommon]): Map[String, Lcommon] = l match {
    case Nil => Map.empty
    case m: Lhashmap => m.map
    case _ =>
      val b = Map.newBuilder[String, Lcommon]
      var p = l
      while (p != Nil) {
        val t = p.head match {
          case p: Lpair => commonToTuple(p)
          case l: Lseq =>  commonToTuple(l)
          case k => // key?
            if (p.tail != Nil) {
              val v = p.tail.head
              p = p.tail
              (commonTokey(k) -> v)
            }
            else
              (commonTokey(k) -> k)
        }
        b += t
        p = p.tail
      }
      b.result
  }

  private def mapToseq: Seq[Lcommon] = map.map {(t) => Lpair(Lstring(t._1), t._2) }.toList

}

case class Lregex(val regex: String) extends Latom {
  val pattern = Pattern.compile(regex)
  def pp = "(regex " + regex + ")"
}

case class Lstruct(val name: Symbol, val fields: MMap[Symbol, Lcommon]) extends Latom {
  def pp = "#S(" + Util.pp(name) +
    fields.foldLeft(""){(s,f) => s + " " + Util.pp(f._1) + " " + f._2 .pp } + ")"

  def get(s: Symbol) = fields.getOrElse(s, Lnil)
  def set(s: Symbol, p: Lcommon) = { fields(s) = p; p }
}

case class Llazyseq(val seq: Stream[Lcommon]) extends Lseq {
  def pp = "(lazyseq " + (
    if (seq.hasDefiniteSize) {
      val sb = new StringBuilder
      seq.foreach{ p => sb ++= p.pp; sb += ' ' }
      sb.toString
    }
    else
      seq.head.pp + " ?" ) +
    ")"

  def cons(x: Lcommon) = Llazyseq(Stream.cons(x, seq))
  override def tail: Lcommon = Llazyseq(seq.tail)
  override def isEmpty = seq.isEmpty
  override def length = if(seq.hasDefiniteSize) seq.length else Integer.MAX_VALUE // -1 ??
  def make(s: Seq[Lcommon]): Lcommon = Llazyseq(s.toStream)
  def empty: Lcommon = Llazyseq(Stream.empty)
}


case class Lprocess(cmd: String, process: Process) extends  Lruntime {
  def pp = "#<process " + cmd  + ">"
}

case class Lwrapper(obj: AnyRef) extends  Lruntime {
  def pp = "#<wrapper " + obj  + ">"
}

