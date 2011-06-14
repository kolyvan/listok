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


//import jline.{ConsoleReader, History}
//import java.io.{BufferedReader, OutputStreamWriter,InputStreamReader}
import java.io.{OutputStreamWriter}

object REPL extends Host {

  val in = Console //{
   // val cr = new ConsoleReader(System.in, new OutputStreamWriter(System.out))
  //  cr.setHistory(new History(new File(".listokhistory")))
  //  cr.setUseHistory(true)
  //  cr.setDefaultPrompt("> ")
  //  cr
  //  Console.in
  //}

  override val redefine = true

  def run() {
    val env = Env.root(this)
    this.backtrace = true
    var ok = true
    println("ru.listok.repl\npress ctrl+d to exit\n")
    var cmd = ""
    while (ok) {
      // in.readLine(">> ") match {
      in.readLine(if (cmd.isEmpty) ">> " else ".. ") match {
        case ":exit" | ":quit" | ":q" | ":e" => ok = false
        case s: String => {
          try {
            cmd += s
            if (isComplete(cmd)) {
              val t = cmd
              cmd = ""
              println(Listok.eval(env, t).pp)
            }
          } catch {
            case ex: ScriptExit => ok = false

            case ParserError(msg) =>
              println(msg)

            case ex: ScriptAssert =>
              println("assertion failed: " + ex.msg)
              Util.printBacktrace(ex.env)
              ok = rundebug(ex.env)

            case ex: ScriptError =>
              println("script error: " + ex.msg)
              Util.printBacktrace(ex.env)
              ok = rundebug(ex.env)

            case ex: ListokRuntimeError =>
              println("runtime error: " + ex.getMessage)
              Util.printBacktrace(ex.env)
              ok = rundebug(ex.env)

            case err =>
              println("internal error: " + err)

          }
        }
        case _ => ok = false
      }
    }
    println("bye")
  }

  def rundebug(env: Env): Boolean = {
    println(" debug mode, type . to continue or ctrl+d to exit")
    while (true) {
      in.readLine("debug> ") match {
        case "."  => return true
        case s: String => {
          try { println(Listok.eval(env, s).pp) }
          catch {
            case e => println(e)
          }
        }
        case _ => return false
      }
    }

    true
  }

  override def onbreak(env: Env, message: String){
    println(" breakpoint: " + message)
    Util.printBacktrace(env)
    rundebug(env)
  }

  def isComplete(cmd: String) =
    cmd.count( _ == '(') <= cmd.count( _ == ')')


}
