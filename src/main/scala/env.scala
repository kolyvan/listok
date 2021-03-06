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

import scala.collection.mutable.ArrayBuffer

sealed abstract class Value {
  def get(env: Env, symbol: Symbol): Lcommon
  def set(env: Env, symbol: Symbol, v: Lcommon): Unit
}

object GlobalValue extends Value {
  def get(env: Env, symbol: Symbol): Lcommon = env.get(symbol)
  def set(env: Env, symbol: Symbol, v: Lcommon): Unit = env.set(symbol, v)
}

case class ReadonlyValue(value: Lcommon) extends Value {
  override def get(env: Env, symbol: Symbol) = value
  override def set(env: Env, symbol: Symbol, v: Lcommon) =
    throw SyntaxError(symbol + " is a constant and thus can't be set", env)
}

case class LocalValue(index: Int) extends Value {
  override def get(env: Env, symbol: Symbol) = env.get(index, symbol)
  override def set(env: Env, symbol: Symbol, value: Lcommon) = env.set(index, value, symbol)
}


//
case class EnvEntry(val name: Symbol, var value: Lcommon, val readonly: Boolean)


class Env ( val name: Symbol,
            val parent: Env,
            private var values: ArrayBuffer[EnvEntry],
            val host: Host,
            val mailslot: Mailslot
            ) {

  if (name != 'global) {
    require(host != null)
    require(mailslot != null)
  }

  // map to index for fast resolving
  private var dict: Map[Symbol, Int] =
    if (values.isEmpty)
      Map.empty
    else {
      val b = Map.newBuilder[Symbol, Int]
      val it = values.iterator
      var i = 0
      while (it.hasNext) {
        b += (it.next().name -> i)
        i += 1
      }
      b.result
    }

  protected def redefine(symbol: Symbol) =
    if (host.redefine)
      host.onwarning(this, "Redefining symbol " + symbol)
    else
      throw SyntaxError("Redefining symbol " + symbol, this)

  protected def defineimpl(symbol: Symbol, value: Lcommon, readonly: Boolean) = {

    def done() {
    //log("define " + symbol + " = " + value.pp + (if (readonly) " const" else ""))
      values.append(EnvEntry(symbol, value, readonly))
      dict += (symbol -> (values.length - 1))
    }

    find(symbol) match {
      case -1 => done
      case x => redefine(symbol); done
    }
  }

  def define(symbol: Symbol, value: Lcommon) = defineimpl(symbol, value, false)

  def defineconst(symbol: Symbol, value: Lcommon) = defineimpl(symbol, value, true)

  def find(symbol: Symbol): Int = {
    dict.get(symbol) match {
      case None => -1
      case Some(i) => i
    }
    //values.findIndexOf( _.name == symbol)
  }

  def isDefined(symbol: Symbol): Boolean =  find(symbol) != -1

  def lookup(symbol: Symbol): Value = lookupOption(symbol) match {
    case None => throw UnboundSymbolError(symbol.toString, this)
    case Some(x) => x
  }

  def lookupOption(symbol: Symbol): Option[Value] = lookupimpl(symbol, true)

  private def lookupimpl(symbol: Symbol, local: Boolean): Option[Value] = find(symbol) match {
    case -1 =>
      if (parent == null)
        None
      else
        parent.lookupimpl(symbol, false)
    case i =>
        val v = values(i)
       // Some(if (v.readonly) ReadonlyValue(v.value)
      //  else if (local) LocalValue(i)
      //  else GlobalValue)

        Some(if (v.readonly && (parent == null || parent.parent == null))
            ReadonlyValue(v.value) // only global and top-level
          else if (local) LocalValue(i)
          else GlobalValue)
    }

  def get(index: Int, symbol: Symbol): Lcommon = {
    assert(index < values.length,
      "invalid index " +index+ " for symbol " + symbol + " in env " + pp)
    assert(values(index).name == symbol, "invalid symbol" +symbol+" in env.get value " + values(index))
    values(index).value
  }

  def set(index: Int, value: Lcommon, symbol: Symbol) {
    assert(index < values.length,
      "invalid index " +index+ " for symbol " + symbol + " in env " + pp)
    val v = values(index)
    assert(v.name == symbol, "invalid symbol" +symbol+" in env.get value " + v)
    if (v.readonly)
      throw SyntaxError(v.name + " is a constant and thus can't be set", this)
    v.value = value
  }

  def get(symbol: Symbol): Lcommon = {
    dict.get(symbol) match {
      case None =>
        if (parent == null)
          throw UnboundSymbolError(symbol.toString, this)
        else
          parent.get(symbol)
      case Some(i) => get(i, symbol)
    }
  }

  def set(symbol: Symbol, value: Lcommon) {
    dict.get(symbol) match {
      case None =>
        if (parent == null)
          throw UnboundSymbolError(symbol.toString, this)
        else
          parent.set(symbol, value)
      case Some(i) => set(i, value, symbol)
    }
  }

  // init lambda list with args
  /*
  def init(args: List[Lcommon], caller: Env) {
    //log("env init " + Util.pp(args))
    this.caller = caller
    var i = 0
    val it = args.iterator
    while (it.hasNext) {
      val v = it.next
      values(i).value = v
      i += 1
    }
    values.reduceToSize(args.length)
  }
  */

  def getStandartInput =
    get(Symbol("*standard-input*")) match {
      case s: Lstream => s
      case _ => throw TypeError("*standard-input* is not stream", this)
    }

  def getStandartOutput =
    get(Symbol("*standard-output*")) match {
      case s: Lstream => s
      case _ => throw TypeError("*standard-output* is not stream", this)
    }

  //def getMailslot: Mailslot = global.mailslot
  def getMailslot: Mailslot = mailslot

  def getGlobal(): Env = parent match {
    case null => this
    case _ => parent.getGlobal()
  }

  // def isGlobal = false

  def load(env : Env) {
    val prefix = Util.pp(env.name)
    for( p <- env.values ) {
      val s = Symbol(prefix + ":" + Util.pp(p.name))
      if (isDefined(s))
        host.onwarning(this, "redefining name " + s + " in env " + name)
      else
        defineimpl(s, p.value, p.readonly)
    }
  }


  /// for debugging

  private var caller: Env = null
  def getCaller = caller
  def setCaller(caller: Env) {this.caller = caller}

  val tid = Thread.currentThread().getId
  var allowTCO = true
  var backtrace: String  = _

  //def host = global.host
  def backtrace(x: Lcommon)(fn: (Lcommon) => Lcommon) = host.backtrace(this, x)(fn)

  def pp = "(env " + name + ")" // "(env " + name + " " + hashCode + ")"

  def dump(all: Boolean = false, sep: String = " ") = {
    if (values.isEmpty)
      "(env " + name + ")"
    else {
      val sb = new StringBuilder
      sb ++= "(env " + name
      values.foreach { p =>
        if (all || !p.readonly) {
          sb ++= sep
          sb ++= Util.pp(p.name)
          sb ++= "="
          sb ++= p.value.pp
        }
      }
      sb ++= ")"
      sb.toString
    }
  }


  def findEnv(name: Symbol): Option[Env] =
    if (name == this.name) Some(this)
    else if (caller != null) caller.findEnv(name)
    else if (parent != null) parent.findEnv(name)
    else None

  def findEnv(pos: Int): Option[Env] =
    if (0 == pos) Some(this)
    else if (caller != null) caller.findEnv(pos - 1)
    else if (parent != null) parent.findEnv(pos - 1)
    else None

}


object Env {

  def apply(name: Symbol, parent: Env) =
    new Env(name, parent, ArrayBuffer.empty, parent.host, parent.mailslot)

  def apply(name: Symbol, parent: Env, mailslot: Mailslot) =
    new Env(name, parent, ArrayBuffer.empty, parent.host, mailslot)

  /*
  def apply(name: Symbol, parent: Env, lambda_list: Seq[Symbol]) =
    new Env(name, parent,
        (lambda_list match {
          case Nil => ArrayBuffer.empty
          case _ => ArrayBuffer(lambda_list.map { EnvEntry(_, Lnil, false) } :_* )
      }), parent.host, parent.mailslot)

  def apply(name: Symbol, parent: Env, ll: Seq[Symbol], args: Seq[Lcommon], mailslot: Mailslot) =
      new Env(name, parent,
          ((ll zip args) match {
            case Nil => ArrayBuffer.empty
            case xs => ArrayBuffer(xs.map { p => EnvEntry(p._1, p._2, false) } :_* )
        }), parent.host, mailslot)
  */

  def apply(name: Symbol, parent: Env, ll: Seq[Symbol]) =
    new Env(name, parent, values(ll, Nil), parent.host, parent.mailslot)

  def apply(name: Symbol, parent: Env, ll: Seq[Symbol], args: Seq[Lcommon], mailslot: Mailslot) =
      new Env(name, parent, values(ll, args), parent.host, mailslot)

  def apply(name: Symbol, parent: Env, ll: Seq[Symbol], args: Seq[Lcommon]): Env =
    apply(name, parent, ll, args, parent.mailslot)

  def values(ll: Seq[Symbol], args: Seq[Lcommon]):ArrayBuffer[EnvEntry] = {
    if (ll.isEmpty)
      ArrayBuffer.empty
    else {
      val l = ll.iterator
      val a = args.iterator
      val b = ArrayBuffer.newBuilder[EnvEntry]
      while (l.hasNext) {
        if (a.hasNext)
          b += EnvEntry(l.next, a.next, false)
        else
          b += EnvEntry(l.next, Lnil, false)
      }
      b.result
    }
  }

  def global(host: Host) = {
    val b = ArrayBuffer.newBuilder[EnvEntry]

    builtin.Common.all.foreach      { x => b += EnvEntry(x.name, x, true) }
    builtin.Sys.all.foreach         { x => b += EnvEntry(x.name, x, true) }
    builtin.Numbers.all.foreach     { x => b += EnvEntry(x.name, x, true) }
    builtin.Sequences.all.foreach   { x => b += EnvEntry(x.name, x, true) }
    builtin.Streams.all.foreach     { x => b += EnvEntry(x.name, x, true) }
    builtin.Regex.all.foreach       { x => b += EnvEntry(x.name, x, true) }
    Macro.all.foreach               { x => b += EnvEntry(x.name, x, true) }
    Concurrent.all.foreach          { x => b += EnvEntry(x.name, x, true) }
    Interop.all.foreach             { x => b += EnvEntry(x.name, x, true) }


  // val con = Lstream(Console.in, Console.out)
  // b += EnvEntry(Symbol("*standard-input*"), con, true)
  // b += EnvEntry(Symbol("*standard-output*"),con, true)
  // b += (Symbol("*error-output*") -> con)

    b += EnvEntry(Symbol("*standard-input*"),  Lstream(new StreamReadText(Console.in)), true)
    b += EnvEntry(Symbol("*standard-output*"), Lstream(new StreamWriteText(Console.out)), true)

    new Env('global, null, b.result, host, null) {
      override protected def defineimpl(symbol: Symbol, value: Lcommon, mutable: Boolean) =
          bugcheck("global env is locked")
 //     override val isGlobal = true
    }
  }

  def root(host: Host) =
    apply('root, Env.global(host), new Mailslot('root))

}
