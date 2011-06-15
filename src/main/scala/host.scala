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

trait Host {

  def onload(env: Env, source: String) = builtin.Streams.withFile(source){ Listok.load(env, _) }
  def onerror(env: Env, message: String) = throw ScriptError(message, env)
  def onexit(env: Env, status: Int): Unit = throw ScriptExit(status) //java.lang.Runtime.getRuntime.exit(status)
  def onwarning(env: Env, message: String): Unit = println("warning: " + message)
  def onbreak(env: Env, message: String) {}
  def onassert(env: Env, message: String) = throw ScriptAssert(message, env)

  //

  def debug = false
  def redefine = false
  def backtrace = debug
  def traceMode = traceList.nonEmpty


  def backtrace(env: Env, x: Lcommon)(fn: (Lcommon) => Lcommon): Lcommon = {
    env.backtrace = x.pp
    fn(x)
  }

  def ppcall(env: Env, call: String) {
    def stacktracedepth(e: Env, s: String = ""): String = e.getCaller match {
      case null => s
      case p => stacktracedepth(p, s + " ")
    }
    val s = stacktracedepth(env)
    log(s + s.length + ": " + call)
  }

  protected var traceList: List[Symbol] = List.empty

  def addtrace(names: Seq[Symbol]) = traceList ++= names
  def remtrace(names: Seq[Symbol]) = traceList = traceList.filterNot( names.contains(_) )

  def ontrace(env: Env, name:Symbol, args: List[Lcommon])(f: => Lcommon) = {
    val b = traceList.contains(name)
    if (b)
      ppcall(env, Util.ppcall(name, args))
    val r = f
    if (b)
      ppcall(env, Util.pp(name) + " = " + r.pp)
    r
  }

}
