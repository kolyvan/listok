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

object main {

  def main(args: Array[String]) {
    if (args.isEmpty)
      REPL.run
    else
      run(args)
   }

  val usage = "Usage: listok [--compile file dest] | [--eval cmd] | [[--load] file [args]] "

  def run(args: Array[String]) {
   // println("args: " + args.mkString(","))

    var debug = false
    var optimize = false
    val l = args.toList.filter {
      case "--debug" | "-d" => debug = true; false
      case "--optimize" | "-o" => optimize = true; false
      case _ => true
    }

    l match {
      case "-c" :: source :: dest => compile(source, dest.head, optimize)
      case "--compile" :: source :: dest => compile(source, dest.head, optimize)

      case "-e" :: cmd => evalCmd(cmd.mkString(" "))
      case "--eval" :: cmd => evalCmd(cmd.mkString(" "))

      case "-l" :: path :: args => loadFile(path, args, debug)
      case "--load" :: path :: args => loadFile(path, args, debug)
      case s => loadFile(s.head, s.tail, debug)
   }
  }

  def compile(src: String, dest: String, optimize: Boolean) {
    if (optimize)
      println("warning: optimization is an experimental feature, use with caution")
    val l = new Listok
    val text = scala.io.Source.fromFile(src).getLines.mkString("\n")
    builtin.Streams.writeFile(dest, l.compile(text, optimize))
  }

  def evalCmd(cmd: String) {
    val l = new Listok
    guard{ println(l.eval(cmd).pp) }
  }

  def loadFile(path: String, args: List[String], isdebug: Boolean) {
    val l = new Listok {
      override val debug = isdebug
      override def onexit(env: Env, status: Int) { java.lang.Runtime.getRuntime.exit(status) }
    }
    defineArgs(l, args)
    guard{ l.load(builtin.Streams.readFile(path)) }
  }

  def defineArgs(listok: Listok, args: List[String]) {
    listok.root.defineconst(Symbol("*args*"), Llist(args.map(Lstring(_))))
  }

  def guard(f: => Unit) =
    try {f}
    catch {
      case ex: ScriptExit =>

      case ParserError(msg) =>
        println(msg)

      case ex: ScriptAssert =>
        println("assertion failed: " + ex.msg)
        Util.printBacktrace(ex.env)
         if (ex.env.host.debug)
          rundebug(ex.env)

      case ex: ScriptError =>
        println("script error: " + ex.msg)
        Util.printBacktrace(ex.env)
         if (ex.env.host.debug)
          rundebug(ex.env)

      case ex: ListokRuntimeError =>
        println("runtime error: " + ex.getMessage)
        Util.printBacktrace(ex.env)
        if (ex.env.host.debug)
          rundebug(ex.env)

      case err =>
        println("internal error: " + err)
    }

  def rundebug(env: Env) {
    println(" debug mode, type ctrl+d to exit")
    var ok = true
    while (ok) {
      readLine("debug> ") match {
        case s: String => {
          try { println(Listok.eval(env, s).pp) }
          catch {
            case e => println(e)
          }
        }
        case _ => ok = false
      }
    }
  }

}
