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

import _root_.ru.listok._

object Numbers extends Helpers {

  def cast(env: Env, nums: List[Lcommon]): Either[List[Float], List[Int]]  = {
    if (nums.forall {
      case f: Lfloat => false
      case i: Lint => true
      case err => throw TypeError("The value " + err.pp + " is not of type NUMBER", env)
    })
    {
      Right(nums.map { x => (x: @unchecked) match {
        case Lfloat(f) => f.toInt
        case Lint(i) => i
      }})
    }
    else
    {
      Left(nums.map { x => (x: @unchecked) match {
        case Lfloat(f) => f
        case Lint(i) => i.toFloat
      }})
    }
  }

  def compare(env: Env, a: Lcommon, b: Lcommon) = {
    (a,b) match {
      case (Lint(l), Lint(r))     => l compare r
      case (Lfloat(l), Lfloat(r)) => l compare r
      case (Lint(l), Lfloat(r))   => l.toFloat compare r
      case (Lfloat(l), Lint(r))   => l compare r
      case (Lchar(l), Lchar(r))   => l compare r
      case _ => throw TypeError("The value " + a.pp + " or " + b.pp + " is not comparable", env)
    }
  }

  def compare(env: Env, args: List[Lcommon])(f: (Int) => Boolean): Lcommon = {
    if (args == Nil)
      throw SyntaxError("Invalid number of argument: 0", env)
    var r = true
    var a = args.head
    var p = args.tail
    while (p != Nil && r) {
      r = r && f(compare(env, a, p.head))
      a = p.head
      p = p.tail
    }
    Util.toLbool(r)
  }

  ///

  def func_add(env: Env, args: List[Lcommon]): Lcommon = {
    if (args == Nil)
       Lint(0)
    else
    cast(env, args) match {
      case Left(f) => Lfloat(f.reduceLeft(_ + _))
      case Right(i) => Lint(i.reduceLeft(_ + _))
    }
  }

  def func_sub(env: Env, args: List[Lcommon]): Lcommon = {
    if (args == Nil)
      throw SyntaxError("Invalid number of argument: 0", env)
    val minus = if (args.length == 1) -1 else 1
    cast(env, args) match {
      case Left(f) => Lfloat(minus * f.reduceLeft(_ - _))
      case Right(i) => Lint(minus * i.reduceLeft(_ - _))
    }
  }

  def func_mul(env: Env, args: List[Lcommon]): Lcommon = {
    if (args == Nil)
       Lint(1)
    else
      cast(env, args) match {
        case Left(f) => Lfloat(f.reduceLeft(_ * _))
        case Right(i) => Lint(i.reduceLeft(_ * _))
      }
  }

  def func_div(env: Env, args: List[Lcommon]): Lcommon = {
    if (args == Nil)
      throw SyntaxError("Invalid number of argument: 0", env)

    try {
      cast(env, args) match {
        case Left(f) => Lfloat(f.reduceLeft(_ / _))
        case Right(i) => Lint(i.reduceLeft(_ / _))
      } }
    catch  {
      case ex: java.lang.ArithmeticException =>
        throw ArithmeticError(ex.getMessage, env)
    }
  }

  def func_less(env: Env, args: List[Lcommon]): Lcommon =
    compare(env, args)(_ < 0)

  def func_less_eq(env: Env, args: List[Lcommon]): Lcommon =
    compare(env, args)(_ <= 0)

  def func_more(env: Env, args: List[Lcommon]): Lcommon =
    compare(env, args)(_ > 0)

  def func_more_eq(env: Env, args: List[Lcommon]): Lcommon =
    compare(env, args)(_ >= 0)

  def func_incr(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    args.head match {
      case Lint(i) => Lint(i+1)
      case Lfloat(f) => Lfloat(f+1)
      case err => throw TypeError("The value " + err + " is not of type NUMBER", env)
    }
  }

  def func_decr(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    args.head match {
      case Lint(i) => Lint(i-1)
      case Lfloat(f) => Lfloat(f-1)
      case err => throw TypeError("The value " + err + " is not of type NUMBER", env)
    }
  }

  def func_rem(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    val a = args.head.castInt(env).int
    val b = args(1).castInt(env).int
    Lint(a % b)
  }

  def func_oddp(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    val a = args.head.castInt(env).int
    if (a % 2 == 0) Lnil else Ltrue
  }

  def func_evenp(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    val a = args.head.castInt(env).int
    if (a % 2 == 0) Ltrue else Lnil
  }

  def func_zerop(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    val a = args.head.castInt(env).int
    Util.toLbool(a == 0)
  }

  def func_plusp(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    val a = args.head.castInt(env).int
    Util.toLbool(a > 0)
  }

  def func_minusp(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    val a = args.head.castInt(env).int
    Util.toLbool(a < 0)
  }

  def func_min(env: Env, args: List[Lcommon]): Lcommon = {
    if (args == Nil)
      throw SyntaxError("Invalid number of argument: 0", env)
    cast(env, args) match {
      case Left(f) => Lfloat(f.min)
      case Right(i) => Lint(i.min)
    }
  }

  def func_max(env: Env, args: List[Lcommon]): Lcommon = {
    if (args == Nil)
      throw SyntaxError("Invalid number of argument: 0", env)
    cast(env, args) match {
      case Left(f) => Lfloat(f.max)
      case Right(i) => Lint(i.max)
    }
  }

  def func_abs(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    args.head match {
      case Lint(i) => (Lint(i.abs))
      case Lfloat(f) => (Lfloat(f.abs))
      case err => throw TypeError("Argument is not a NUMBER: " + err.pp, env)
    }
  }

  val all = List (
    Lfunction(func_add, '+),
    Lfunction(func_sub, '-),
    Lfunction(func_mul, '*),
    Lfunction(func_div, '/),
    Lfunction(func_less, '<),
    Lfunction(func_less_eq, '<=),
    Lfunction(func_more, '>),
    Lfunction(func_more_eq, '>=),
    // Lfunction(func_eq, '=),
    // Lfunction(func_noteq, '/=),
    Lfunction(func_incr, 'incr),
    Lfunction(func_decr, 'decr),
    Lfunction(func_rem, 'rem),
    Lfunction(func_oddp, 'oddp),
    Lfunction(func_evenp, 'evenp),
    Lfunction(func_zerop, 'zerop),
    Lfunction(func_plusp, 'plusp),
    Lfunction(func_minusp, 'minusp),
    Lfunction(func_min, 'min),
    Lfunction(func_max, 'max),
    Lfunction(func_abs, 'abs)
  )
}

