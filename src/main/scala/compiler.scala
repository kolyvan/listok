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

import sbinary._
import collection.mutable.ArraySeq
import net.fyrie.ratio.Ratio

//import DefaultProtocol._
import Operations._


object Compiler extends DefaultProtocol {

  implicit object LFormat extends Format[Lcommon]{

    def writes(out : Output, value : Lcommon) = {
      def writeSign(b: Byte) = write[Byte](out, b)
      value match {
        case Lchar(c)     => writeSign(0); write(out, c)
        case Lint(i)      => writeSign(1); write(out, i)
        case Lfloat(f)    => writeSign(2); write(out, f)
        case Lstring(s)   => writeSign(3); write(out, s)
        case Llist(xs)    => writeSign(4); write(out, xs)
        case Lquote(f)    => writeSign(5); writes(out, f)
        case Lsymbol(s)   => writeSign(6); write(out, Util.pp(s))
        case Llambda(l, b, n) => writeSign(7); write(out, l); write(out, b); write(out, Util.pp(n))
        case Lnil         => writeSign(8)
        case Ltrue        => writeSign(9)
        case Lsform(_, s) => writeSign(10); write(out, Util.pp(s))
        case Lkeyword(k)  => writeSign(11); write(out, Util.pp(k))
        case Lvector(a)   => writeSign(12); write(out, a.toArray)
        case Lpair(a,b)   => writeSign(13); writes(out, a); writes(out, b)
        case Lregex(s)    => writeSign(14); write(out, s)
        case Llong(l)     => writeSign(15); write(out, l)
        case Lbignum(b)   => writeSign(16); write(out, b)
        case Lratio(r)    => writeSign(17); write(out, r.n); write(out, r.d)
        case Lbyte(b)     => writeSign(18); write(out, b)
        case Lblob(bb)    => writeSign(19); write(out, bb)
        case x => bugcheck("can't write an unknown lcommon type: " + x.pp)
        }
      }

    def reads(in : Input): Lcommon = {
      def readLL = read[List[Lcommon]](in)
      read[Byte](in) match {
        case 0  => Lchar(read[Char](in))
        case 1  => Lint(read[Int](in))
        case 2  => Lfloat(read[Double](in))
        case 3  => Lstring(read[String](in))
        case 4  => Llist(readLL)
        case 5  => Lquote(reads(in))
        case 6  => Lsymbol(read[Symbol](in))
        case 7  => Llambda(readLL, readLL, read[Symbol](in))
        case 8  => Lnil
        case 9  => Ltrue
        case 10 => val s = read[Symbol](in); SpecialForms.make(s)
        case 11 => Lkeyword(read[Symbol](in))
        case 12 => val a = read[Array[Lcommon]](in); Lvector(ArraySeq(a:_*))
        case 13 => Lpair(reads(in),reads(in))
        case 14 => Lregex(read[String](in))
        case 15 => Llong(read[Long](in))
        case 16 => Lbignum(read[BigInt](in))
        case 17 => Lratio(Ratio(read[BigInt](in), read[BigInt](in)))
        case 18 => Lbyte(read[Byte](in))
        case 19 => Lblob(read[Array[Byte]](in))
        case x => bugcheck("can't read an unknown lcommon type: " + x)
      }
    }
  }

  def compile(forms: List[Lcommon], env: Env = null, optimize: Boolean = false) =  {
    var p = if (env != null) Macro.macroexpand(env, forms) else forms
    if (optimize)
      p = Optimize.run(p)
    toByteArray(p)
  }

  def load(bytes: Array[Byte]): List[Lcommon] = {
    fromByteArray[List[Lcommon]](bytes)
  }
}
