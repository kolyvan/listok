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

import java.io.ByteArrayOutputStream
import net.fyrie.ratio.Ratio
import sbinary.{JavaOutput, Output, StandardPrimitives, Format, Input}


object Blob extends StandardPrimitives {

  implicit object StringFormat extends Format[String] {
    def reads(in: Input) = bugcheck("never called")
    def writes(out: Output, s: String) = bugcheck("never called")
  }

  // partially reused sbinary
  // only for simple type (char, int, long, float and double)

  def writes(env: Env, out: Output, value : Any): Unit = value match {
    case Nil            => out.writeByte(0)
    case false          => out.writeByte(0)
    case true           => out.writeByte(1)
    case x: Byte        => out.writeByte(x)

    case x: Char        => CharFormat.writes(out, x)
    case x: Int         => IntFormat.writes(out, x)
    case x: Long        => LongFormat.writes(out, x)
    case x: Float       => FloatFormat.writes(out, x)
    case x: Double      => DoubleFormat.writes(out, x)

    case x: BigInt      => out.writeAll(x.bigInteger.toByteArray)
    case x: Ratio       => writes(env, out, x.n)
                           writes(env, out, x.d)

    case x: String      => out.writeAll(x.getBytes)
    case x: Symbol      => out.writeAll(Util.pp(x).getBytes)

    case x: Array[Byte] => out.writeAll(x)

    case x: Seq[_]      => x.foreach(writes(env, out, _))
    case (a,b)          => writes(env, out, a)
                           writes(env, out, b)


    case x              => throw TypeError("Unable get bytes from: " + x, env)
  }

  def toBytes(env: Env, l: List[Lcommon]) = {
    val bytes = new ByteArrayOutputStream
    val output = new JavaOutput(bytes)
    l.foreach { p => writes(env, output, p.getAny) }
    bytes.toByteArray
  }

  def toByte(env: Env, x: Lcommon) = x match {
    case n: Lnumeric => n.int.toByte
    case Lchar(c) => c.toByte
    case Lnil => 0.toByte
    case Ltrue => 1.toByte
    case Lbyte(b) => b
    case err => throw TypeError("Unable to make byte from: " + err, env)
  }
}

/*
object Blob extends DefaultProtocol {

  implicit object AnyFormat extends Format[Any]{
    def writes(out : Output, value : Any) = value match {
      case Nil          => out.writeByte(0)
      case false        => out.writeByte(0)
      case true         => out.writeByte(1)

      case x: Byte      => out.writeByte(x)
      case x: Char      => write(out, x)
      case x: Int       => write(out, x)
      case x: Long      => write(out, x)
      case x: Float     => write(out, x)
      case x: Double    => write(out, x)
      case x: BigInt    => out.writeAll(x.bigInteger.toByteArray)
      case x: Ratio     => writes(out, x.n); writes(out, x.d)

      case x: String    => out.writeAll(x.getBytes)
      case x: Symbol    => out.writeAll(Util.pp(x).getBytes)

      case x: Seq[_]    => x.foreach(writes(out, _))
      case (a,b)        => writes(out,a); writes(out,b)

      case x => bugcheck("can't get bytes from an unknown type: " + x)
    }

    def reads(in : Input): Any = bugcheck("must be never called")
  }

  def toBytes(l: List[Lcommon]) = toByteArray(l.map(_.getAny))

}
*/


