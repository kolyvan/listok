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



object LazySeq {

  def from(n: Int): Stream[Lint] = Stream.cons(Lint(n), from(n + 1))
  def range(start: Int, end: Int): Stream[Lint] = from(start).take(end-start + 1)

  def seq(env: Env, l: List[Lcommon]):Llazyseq = {

    def gen(env: Env, fn: Lfunction) : Stream[Lcommon] = {
      fn.lapply(env, List()) match {
          case l: Llazyseq => l.seq
          case n => Stream.cons(n, gen(env, fn))
        }
    }

    def gen1(env: Env, x: Lcommon, fn: Lfunction) : Stream[Lcommon] = {
      fn.lapply(env, List(x)) match {
          case l: Llazyseq => l.seq
          case n => Stream.cons(n, gen1(env, n, fn))
        }
    }

    def fill(env: Env, x: Lcommon) : Stream[Lcommon] = Stream.cons(x, fill(env, x))

    val init = l.init

    lazy val last = l.last match {
      case Llazyseq(ls) => ls
      case fn: Lfunction =>
        if (init.isEmpty)
          gen(env, fn)
        else
          gen1(env, init.last, fn)
      case x => fill(env, x)
      }

    if (init.isEmpty)
      Llazyseq(last)
    else
      Llazyseq(init.toStream.append(last))
  }
}

