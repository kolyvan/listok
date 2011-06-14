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

package ru  {

  package object listok {

    type FuncCall = (Env, List[Lcommon]) => Lcommon

    def log(text: => String, args: Any*) {
      println(text.format(args:_*))
    }

    def bugcheck(msg: String, env: Env = null): Nothing = {
      // todo: dump env
      sys.error("bugcheck: " + msg)
    }
  }
}

package ru.listok {

  case class ParserError(msg: String) extends RuntimeException(msg)

  sealed abstract class ListokRuntimeError(msg: String) extends RuntimeException(msg) {
    def env:  Env
    def backtrace(expandAll: Boolean = true) = Util.backtrace(env, expandAll)
  }

  case class SyntaxError(msg: String, env: Env) extends ListokRuntimeError(msg)
  case class TypeError(msg: String, env: Env) extends ListokRuntimeError(msg)
  case class UnboundSymbolError(msg: String, env: Env) extends ListokRuntimeError(msg)
  case class UndefinedVariableError(msg: String, env: Env) extends ListokRuntimeError(msg)
  case class ArithmeticError(msg: String, env: Env) extends ListokRuntimeError(msg)

  case class ScriptExit(status: Int) extends RuntimeException("exit")
  case class ScriptError(msg: String, env: Env) extends ListokRuntimeError(msg)
  case class ScriptAssert(msg: String, env: Env) extends ListokRuntimeError(msg)

}
