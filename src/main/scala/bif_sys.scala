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

package ru.listok.builtin

import ru.listok._


object Sys extends Helpers {

  def func_display(env: Env, l: List[Lcommon]): Lcommon = {
    if (l.nonEmpty) {
      val s = l.head.getString(env)
      Console.printf(s, l.tail.map(_.getAny):_*)
    }
    else {
      Console.println("")
    }
    Lnil
  }

  def func_current_time(env: Env, l: List[Lcommon]): Lcommon = {
    val ms = System.currentTimeMillis()
    Lpair(Lint((ms / 1000).toInt), Lint((ms % 1000).toInt))
  }

  def func_current_directory(env: Env, l: List[Lcommon]): Lcommon = {
    Lstring(System.getProperty("user.dir"))
  }

  def func_system_name(env: Env, l: List[Lcommon]): Lcommon = {
    Lstring(System.getProperty("os.name"))
  }


  val all = List (
    Lfunction(func_display, 'display),
    Lfunction(func_current_time, Symbol("current-time")),
    Lfunction(func_current_directory, Symbol("current-directory")),
    Lfunction(func_system_name, Symbol("system-name"))
  )
}