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
import java.util.regex.Pattern

object Regex extends Helpers {

   def matches(pattern: Pattern, s: String) = {
    val m = pattern.matcher(s)
    if (m.matches) {
      if (m.groupCount == 0)
        LL(Lstring(m.group))
      else
        Llist((1 to m.groupCount).toList.map { i => Lstring(m.group(i)) })
    }
    else
      Lnil
  }

  def find(pattern: Pattern, s: String) = {
    val m = pattern.matcher(s)
    if (m.find) Lstring(m.group)
    else Lnil
  }

  def findall(pattern: Pattern, s: String) = {
    val m = pattern.matcher(s)
    val l = List.newBuilder[Lstring]
    while (m.find)
      l += Lstring(m.group)
    l.result match {
      case Nil => Lnil
      case xs => Llist(xs)
    }
  }

  def scan(pattern: Pattern, s: String) = {
    val m = pattern.matcher(s)
    val l = List.newBuilder[Lint]
    while (m.find) {
      l += Lint(m.start)
      l += Lint(m.end)
    }
    l.result match {
      case Nil => Lnil
      case xs => Llist(xs)
    }
  }

  def replace(pattern: Pattern, s: String, r: String) = {
    val m = pattern.matcher(s)
    Lstring(m.replaceAll(r))
  }

  def split(pattern: Pattern, s: String) = LL(pattern.split(s).map{Lstring(_)}:_*)


  /// regex
  def func_regex(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    Lregex(args.head.getString(env))
  }

  def func_regex_matches(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    val r = args.head.castRegex(env)
    val s = args(1).getString(env)
    matches(r.pattern, s)
  }

  def func_regex_find(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    val r = args.head.castRegex(env)
    val s = args(1).getString(env)
    find(r.pattern, s)
  }

  def func_regex_find_all(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    val r = args.head.castRegex(env)
    val s = args(1).getString(env)
    findall(r.pattern, s)
  }

  def func_regex_scan(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    val r = args.head.castRegex(env)
    val s = args(1).getString(env)
    scan(r.pattern, s)
  }

  def func_regex_split(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    val r = args.head.castRegex(env)
    val s = args(1).getString(env)
    split(r.pattern, s)
  }

  def func_regex_replace (env: Env, args: List[Lcommon]) = {
    mustEqual(env, args, 3)
    val regex = args.head match {
      case Lstring(s) => Lregex(s)
      case r: Lregex => r
      case err => throw throw TypeError("The value " + err + " is not REGEX or STRING", env)
    }
    val s = args(1).getString(env)
    val r = args(2).getString(env)
    replace(regex.pattern, s, r)
  }

  val all = List (
    Lfunction(func_regex, 'regex),
    Lfunction(func_regex_matches, Symbol("regex-matches")),
    Lfunction(func_regex_find, Symbol("regex-find")),
    Lfunction(func_regex_find_all, Symbol("regex-find-all")),
    Lfunction(func_regex_scan, Symbol("regex-scan")),
    Lfunction(func_regex_split, Symbol("regex-split")),
    Lfunction(func_regex_replace, Symbol("regex-replace"))
   )
}
