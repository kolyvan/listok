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

import scala.math.{BigInt, ScalaNumber}
import scala.runtime.{RichInt, RichLong, RichDouble}
import net.fyrie.ratio.Ratio

// add|sub|mul - these code taken from http://www.java2s.com/

trait NumOp[R] {
  def apply(a: Long, b: Long): R
  def apply(a: Int, b: Int): R
  def apply(a: Double, b: Double): R
  def apply(a: BigInt, b: BigInt): R
  def apply(a: Ratio, b: Ratio): R
}

object AddOp extends NumOp[ScalaNumber] {

  def apply(a: Long, b: Long): ScalaNumber = {
    if (a > b)
      apply(b, a)   // use symmetry to reduce boundry cases
    else {
      // assert a <= b
      if (a < 0) {
        if (b < 0) {
          // check for negative overflow
          if (Long.MinValue - b <= a)
            a + b
          else
            apply(BigInt(a), BigInt(b))
        }
        else
          // oppisite sign addition is always safe
          a + b
      } else {
        // assert a >= 0
        // assert b >= 0
        // check for positive overflow
        if (a <= Long.MaxValue - b)
          a + b
        else
          apply(BigInt(a), BigInt(b))
      }
    }
  }

  def apply(a: Int, b: Int): ScalaNumber = {
    if (a > b)
      apply(b, a)   // use symmetry to reduce boundry cases
    else {
      // assert a <= b
      if (a < 0) {
        if (b < 0) {
          // check for negative overflow
          if (Int.MinValue - b <= a)
            a + b
          else
            apply(a.toLong, b.toLong)
        }
        else
          // oppisite sign addition is always safe
          a + b
      } else {
        // assert a >= 0
        // assert b >= 0
        // check for positive overflow
        if (a <= Int.MaxValue - b)
          a + b
        else
          apply(a.toLong, b.toLong)
      }
    }
  }

  def apply(a: Double, b: Double): ScalaNumber = a + b

  def apply(a: BigInt, b: BigInt): ScalaNumber = a + b

  def apply(a: Ratio, b: Ratio): ScalaNumber = a + b
}

object SubOp extends NumOp[ScalaNumber] {
  def apply(a: Long, b: Long): ScalaNumber = {
    if (b == Long.MinValue) {
      if (a < 0)
        a - b
      else
        apply(BigInt(a), BigInt(b))
    } else
      // use additive inverse
      AddOp(a, -b)
  }

  def apply(a: Int, b: Int): ScalaNumber = {
    if (b == Int.MinValue) {
      if (a < 0)
        a - b
      else
        apply(a.toLong, b.toLong)
    } else
      // use additive inverse
      AddOp(a, -b)
  }

  def apply(a: Double, b: Double): ScalaNumber = a - b

  def apply(a: BigInt, b: BigInt): ScalaNumber = a - b

  def apply(a: Ratio, b: Ratio): ScalaNumber = a - b

}

object MulOp extends NumOp[ScalaNumber] {

   def apply (a: Long, b: Long): ScalaNumber = {
     if (a > b)
       // use symmetry to reduce boundry cases
       apply(b, a)
     else {
       if (a < 0) {
         if (b < 0) {
           // check for positive overflow with negative a, negative b
           if (a >= Long.MaxValue / b)
             a * b
           else
             apply(BigInt(a), BigInt(b))

         } else if (b > 0) {
           // check for negative overflow with negative a, positive b
           if (Long.MinValue / b <= a)
             a * b
           else
             apply(BigInt(a), BigInt(b))

         } else
           // assert b == 0
           0

       } else if (a > 0) {
           // assert a > 0
           // assert b > 0
           // check for positive overflow with positive a, positive b
           if (a <= Long.MaxValue / b)
             a * b
           else
             apply(BigInt(a), BigInt(b))

       } else
         // assert a == 0
         0
     }
   }

  def apply (a: Int, b: Int): ScalaNumber = {
    if (a > b)
      // use symmetry to reduce boundry cases
      apply(b, a)
    else {
      if (a < 0) {
        if (b < 0) {
          // check for positive overflow with negative a, negative b
          if (a >= Int.MaxValue / b)
            a * b
          else
            apply(a.toLong, b.toLong)

        } else if (b > 0) {
          // check for negative overflow with negative a, positive b
          if (Int.MinValue / b <= a)
            a * b
          else
            apply(a.toLong, b.toLong)

        } else
          // assert b == 0
          0

      } else if (a > 0) {
          // assert a > 0
          // assert b > 0
          // check for positive overflow with positive a, positive b
          if (a <= Int.MaxValue / b)
            a * b
          else
            apply(a.toLong, b.toLong)

      } else
        // assert a == 0
        0
    }
  }

  def apply(a: Double, b: Double): ScalaNumber = a * b

  def apply(a: BigInt, b: BigInt): ScalaNumber = a * b

  def apply(a: Ratio, b: Ratio): ScalaNumber = a * b
}

object DivOp extends NumOp[ScalaNumber] {
  def apply(a: Long, b: Long)     = Ratio(BigInt(a), BigInt(b))
  def apply(a: Int, b: Int)       = Ratio(BigInt(a), BigInt(b))
  def apply(a: Double, b: Double) = a / b
  def apply(a: BigInt, b: BigInt) = Ratio(a, b)
  def apply(a: Ratio, b: Ratio)   = a / b
}

object CmpOp extends NumOp[Int] {
  def apply(a: Long, b: Long)     = a compare b
  def apply(a: Int, b: Int)       = a compare b
  def apply(a: Double, b: Double) = a compare b
  def apply(a: BigInt, b: BigInt) = a compare b
  def apply(a: Ratio, b: Ratio)   = a compare b
}

object RemOp extends NumOp[ScalaNumber] {
  def apply(a: Long, b: Long)     = a % b
  def apply(a: Int, b: Int)       = a % b
  def apply(a: Double, b: Double) = a % b
  def apply(a: BigInt, b: BigInt) = a % b
  def apply(a: Ratio, b: Ratio)   = bugcheck("unimplemented") //{ val d = a / b; a - d * b }
}

object ModOp extends NumOp[ScalaNumber] {
  def apply(a: Long, b: Long)     = { val r = a % b; if (r < 0) r + b else r }
  def apply(a: Int, b: Int)       = { val r = a % b; if (r < 0) r + b else r }
  def apply(a: Double, b: Double) = { val r = a % b; if (r < 0) r + b else r }
  def apply(a: BigInt, b: BigInt) = a mod b
  def apply(a: Ratio, b: Ratio)   = bugcheck("unimplemented")

}

// def gcd(x: Int, y: Int): Int =
// if (b == 0) x
// else gcd(b, x % y)

//object GcdOp extends NumOp[ScalaNumber] {
//  def apply(a: Long, b: Long)     = a gcd b
//  def apply(a: Int, b: Int)       = a gcd b
//  def apply(a: Float, b: Float)   = a gcd b
//  def apply(a: BigInt, b: BigInt) = a gcd b
//}


object NumOp {

  def calc[R] (op: NumOp[R], s: ScalaNumber, l: Lnumeric): R = (s,l) match {
    case (a: RichInt, Lint(b))     => op(a.intValue, b)
    case (a: RichInt, Llong(b))    => op(a.toLong, b)
    case (a: RichInt, Lfloat(b))   => op(a.toDouble, b)
    case (a: RichInt, Lbignum(b))  => op(BigInt(a.intValue), b)
    case (a: RichInt, Lratio(b))   => op(Ratio(a.intValue), b)

    case (a: RichLong, Lint(b))    => op(a.longValue, b.toLong)
    case (a: RichLong, Llong(b))   => op(a.longValue, b)
    case (a: RichLong, Lfloat(b))  => op(a.toDouble, b)
    case (a: RichLong, Lbignum(b)) => op(BigInt(a.longValue), b)
    case (a: RichLong, Lratio(b))  => op(Ratio(a.longValue), b)

    case (a: RichDouble, Lint(b))   => op(a.doubleValue, b.toDouble)
    case (a: RichDouble, Llong(b))  => op(a.doubleValue, b.toDouble)
    case (a: RichDouble, Lfloat(b)) => op(a.doubleValue, b.toDouble)
    case (a: RichDouble, Lbignum(b))=> op(a.doubleValue, b.toDouble)
    case (a: RichDouble, Lratio(b)) => op(a.doubleValue, b.toDouble)

    case (a: BigInt, Lint(b))      => op(a, BigInt(b))
    case (a: BigInt, Llong(b))     => op(a, BigInt(b))
    case (a: BigInt, Lbignum(b))   => op(a, b)
    case (a: BigInt, Lfloat(b))    => op(a.toDouble, b)
    case (a: BigInt, Lratio(b))    => op(Ratio(a), b)

    case (a: Ratio, Lint(b))       => op(a, Ratio(b))
    case (a: Ratio, Llong(b))      => op(a, Ratio(b))
    case (a: Ratio, Lbignum(b))    => op(a, Ratio(b))
    case (a: Ratio, Lfloat(b))     => op(a.toDouble, b)
    case (a: Ratio, Lratio(b))     => op(a, b)

    case err => bugcheck("unexpected argument in numop.add: " + err)
  }

  def calc[R] (op: NumOp[R], s: Lnumeric, l: Lnumeric): R = (s,l) match {
    case (Lint(a), Lint(b))      => op(a, b)
    case (Lint(a), Llong(b))     => op(a.toLong, b)
    case (Lint(a), Lfloat(b))    => op(a.toDouble, b)
    case (Lint(a), Lbignum(b))   => op(BigInt(a), b)
    case (Lint(a), Lratio(b))    => op(Ratio(a), b)

    case (Llong(a), Lint(b))     => op(a, b.toLong)
    case (Llong(a), Llong(b))    => op(a, b)
    case (Llong(a), Lfloat(b))   => op(a.toDouble, b)
    case (Llong(a), Lbignum(b))  => op(BigInt(a), b)
    case (Llong(a), Lratio(b))   => op(Ratio(a), b)

    case (Lfloat(a), Lint(b))    => op(a, b.toDouble)
    case (Lfloat(a), Llong(b))   => op(a, b.toDouble)
    case (Lfloat(a), Lfloat(b))  => op(a, b.toDouble)
    case (Lfloat(a), Lbignum(b)) => op(a, b.toDouble)
    case (Lfloat(a), Lratio(b))  => op(a, b.toDouble)

    case (Lbignum(a), Lint(b))   => op(a, BigInt(b))
    case (Lbignum(a), Llong(b))  => op(a, BigInt(b))
    case (Lbignum(a), Lbignum(b))=> op(a, b)
    case (Lbignum(a), Lfloat(b)) => op(a.toDouble, b)
    case (Lbignum(a), Lratio(b)) => op(Ratio(a), b)

    case (Lratio(a), Lint(b))    => op(a, Ratio(b))
    case (Lratio(a), Llong(b))   => op(a, Ratio(b))
    case (Lratio(a), Lbignum(b)) => op(a, Ratio(b))
    case (Lratio(a), Lfloat(b))  => op(a.toDouble, b)
    case (Lratio(a), Lratio(b))  => op(a, b)

    case err => bugcheck("unexpected argument in numop.add: " + err)
  }

  def add(s: ScalaNumber, l: Lnumeric): ScalaNumber = calc(AddOp, s, l)
  def sub(s: ScalaNumber, l: Lnumeric): ScalaNumber = calc(SubOp, s, l)
  def mul(s: ScalaNumber, l: Lnumeric): ScalaNumber = calc(MulOp, s, l)
  def div(s: ScalaNumber, l: Lnumeric): ScalaNumber = calc(DivOp, s, l)
}


object Numbers extends Helpers {

  def toLnumeric(value: ScalaNumber): Lnumeric = value match {
    case r: Ratio =>
      if (r.d == 1) toLnumeric(r.n) else Lratio(r)

    case b: BigInt =>
      val len = b.bitLength
      if (len < 32) Lint(b.toInt)
      else if (len < 64) Llong(b.toLong)
      else Lbignum(b)

    case l: RichLong if l.isValidInt => Lint(l.toInt)
    case l: RichLong => Llong(l.longValue)
    case i: RichInt => Lint(i.intValue)
    case f: RichDouble => Lfloat(f.doubleValue)

    case err => bugcheck("unable cast to unexpected number " + err)
  }


  def compare(env: Env, args: List[Lcommon])(f: (Int) => Boolean): Lcommon = {
    if (args == Nil)
      throw SyntaxError("Invalid number of argument: 0", env)
    var r = true
    var a = args.head
    var p = args.tail
    while (p != Nil && r) {
      r = r && f(NumOp.calc(CmpOp, a.castNumeric(env), p.head.castNumeric(env)))
      a = p.head
      p = p.tail
    }
    Util.toLbool(r)
  }

  def func_add(env: Env, args: List[Lcommon]): Lcommon = {
    if (args == Nil)
       Lint(0)
    else {
      val a = args.head.castNumeric(env).scalaNumber
      toLnumeric( args.tail.foldLeft(a){ (s,l) => NumOp.add(s, l.castNumeric(env)) })
    }
  }

  def func_sub(env: Env, args: List[Lcommon]): Lcommon = {
    if (args == Nil)
      throw SyntaxError("Invalid number of argument: 0", env)
    val a = args.head.castNumeric(env)
    if (args.length == 1)
      a.negate
    else
      toLnumeric( args.tail.foldLeft(a.scalaNumber){ (s,l) => NumOp.sub(s, l.castNumeric(env)) })
  }

  def func_mul(env: Env, args: List[Lcommon]): Lcommon = {
    if (args == Nil)
       Lint(1)
    else {
      val a = args.head.castNumeric(env).scalaNumber
      toLnumeric( args.tail.foldLeft(a){ (s,l) => NumOp.mul(s, l.castNumeric(env)) })
    }
  }

  def func_div(env: Env, args: List[Lcommon]): Lcommon = {
    if (args == Nil)
      throw SyntaxError("Invalid number of argument: 0", env)

    try {
      val a = args.head.castNumeric(env).scalaNumber
      toLnumeric( args.tail.foldLeft(a){ (s,l) => NumOp.div(s, l.castNumeric(env)) })
    }
    catch  {
      case ex: IllegalArgumentException =>
        throw ArithmeticError(ex.getMessage, env)
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
    toLnumeric(NumOp.calc(AddOp, 1, args.head.castNumeric(env)))
  }

  def func_decr(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    toLnumeric(NumOp.calc(AddOp, -1, args.head.castNumeric(env)))
  }

  def func_rem(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    toLnumeric(NumOp.calc(RemOp, args(0).castNumeric(env), args(1).castNumeric(env)))
  }

  def func_mod(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    toLnumeric(NumOp.calc(ModOp, args(0).castNumeric(env), args(1).castNumeric(env)))
  }

  def func_oddp(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    val rem = NumOp.calc(RemOp, args.head.castNumeric(env), Lint(2))
    Util.toLbool(0 != NumOp.calc(CmpOp, rem, Lint(0)))
  }

  def func_evenp(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    val rem = NumOp.calc(RemOp, args.head.castNumeric(env), Lint(2))
    Util.toLbool(0 == NumOp.calc(CmpOp, rem, Lint(0)))
  }

  def func_zerop(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    Util.toLbool(0 == NumOp.calc(CmpOp, args.head.castNumeric(env), Lint(0)))
  }

  def func_plusp(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    Util.toLbool(NumOp.calc(CmpOp, args.head.castNumeric(env), Lint(0)) > 0)
  }

  def func_minusp(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    Util.toLbool(NumOp.calc(CmpOp, args.head.castNumeric(env), Lint(0)) < 0)
  }

  def func_min(env: Env, args: List[Lcommon]): Lcommon = {
    if (args == Nil)
      throw SyntaxError("Invalid number of argument: 0", env)
    args.reduceLeft{ (x, y) =>
        val r = NumOp.calc(CmpOp, x.castNumeric(env), y.castNumeric(env))
        if (r <= 0) x else y
    }
  }

  def func_max(env: Env, args: List[Lcommon]): Lcommon = {
    if (args == Nil)
      throw SyntaxError("Invalid number of argument: 0", env)
    args.reduceLeft{ (x, y) =>
        val r = NumOp.calc(CmpOp, x.castNumeric(env), y.castNumeric(env))
        if (r >= 0) x else y
    }
  }

  def func_abs(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    args.head.castNumeric(env).abs
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
    Lfunction(func_incr, 'incr),
    Lfunction(func_decr, 'decr),
    Lfunction(func_rem, 'rem),
    Lfunction(func_mod, 'mod),
    Lfunction(func_oddp, 'oddp),
    Lfunction(func_evenp, 'evenp),
    Lfunction(func_zerop, 'zerop),
    Lfunction(func_plusp, 'plusp),
    Lfunction(func_minusp, 'minusp),
    Lfunction(func_min, 'min),
    Lfunction(func_max, 'max),
    Lfunction(func_abs, 'abs)

    // gcd lcm
    // pow
    // sqrt
    // ++ 1+ 1- --

  )
}

