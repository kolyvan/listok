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

import scala.annotation.tailrec
import java.io._

sealed abstract class StreamBase {

  type T = { def close(): Unit }
  def obj: T

  def canRead: Boolean
  def canWrite: Boolean

  def close = { closed = true; obj.close() }
  def isClosed: Boolean = closed
  protected var closed = false

  def checkForWrite(env: Env) {
     if (!canWrite)
       throw SyntaxError("Unable write to " + status + " stream", env)
     if (isClosed)
       throw SyntaxError("The stream is closed", env)
   }

   def checkForRead(env: Env) {
     if (!canRead)
       throw SyntaxError("Unable read from " + status + " stream", env)
     if (isClosed)
       throw SyntaxError("The stream is closed", env)
   }

  def status =
    if (isClosed) 'closed
    else if (canRead) 'input
    else if (canWrite) 'output
    else 'failed

  def readByte(): Lcommon = Lnil
  def readBlob(): Lcommon = Lnil
  def readBlob(num: Int): Lcommon  = Lnil

  def readChar(): Lcommon = Lnil
  def readString(): Lcommon = Lnil
  def readLine(): Lcommon = Lnil

  def write(x: Lbyte) {}
  def write(x: Lblob)  {}
  def write(x: Lchar)  {}
  def write(x: Lstring)  {}
  def newline()  {}
  def flush()  {}
}

class StreamReadBin(in: InputStream) extends StreamBase {

  val obj = in
  val canRead = true
  val canWrite = false

  override def readByte() = in.read match {
    case -1 => Lnil
    case x => Lbyte(x.toByte)
  }

  override def readBlob() = {
    val b = readBytes
    if (b.isEmpty) Lnil
    else Lblob(b)
  }

  override def readBlob(numBytes: Int) = {
    val bytes = new Array[Byte](numBytes)
    in.read(bytes) match {
      case -1 => Lnil
      case `numBytes` => Lblob(bytes)
      case x =>
        val t = new Array[Byte](x)
        bytes.copyToArray(t, 0, x)
        Lblob(t)
    }
  }

  override def readChar() = in.read match {
    case -1 => Lnil
    case x => Lchar(x.toChar)
  }

  override def readLine() = {
    val bos = new ByteArrayOutputStream

    @tailrec
    def loop(): Boolean = {
      in.read match {
        case -1 => true   // eof
        case 13 => loop() // \r     skip
        case 10 => false  // \n     stop
        case x  => bos.write(x.toByte); loop()
      }
    }

    val eof = loop()
    val b = bos.toByteArray

    if (b.isEmpty && eof) Lnil
    else Lstring(new String(b))
  }

  override def readString() = Lstring(new String(readBytes))

  protected def readBytes: Array[Byte] = {
    val bos = new ByteArrayOutputStream
    val ba = new Array[Byte](4096)

    @tailrec
    def loop(): Array[Byte] = in.read(ba) match {
      case -1 => bos.toByteArray
      case x  => bos.write(ba, 0, x); loop()
    }
    loop()
  }
}

class StreamReadText(reader: BufferedReader) extends StreamBase {

  val obj = reader
  val canRead = true
  val canWrite = false

  override def readByte() = reader.read match {
    case -1 => Lnil
    case x => Lbyte(x.toByte)
  }

  override def readBlob() = Lblob(reads.getBytes)

  override def readBlob(num: Int) = {
    val bos = new ByteArrayOutputStream
    var count = 0

    @tailrec
    def loop (): Boolean = reader.read match {
        case -1 => true
        case x =>
          val b = x.toString.getBytes
          bos.write(b)
          count += b.length
          if (count < num)
            loop
          else
            false
      }

    val eof = loop()
    val b = bos.toByteArray

    if (b.isEmpty && eof) Lnil
    else Lblob(b)

  }

  override def readChar() = reader.read match {
    case -1 => Lnil
    case x => Lchar(x.toChar)
  }

  override def readString() = Lstring(reads)

  override def readLine() = reader.readLine match {
    case null => Lnil
    case x => Lstring(x)
  }

  protected def reads = {
    val sb = new StringBuilder
    val buf = new Array[Char](4096)
    var n = reader.read(buf)
    while (n >= 0) {
      sb.appendAll(buf, 0, n)
      n = reader.read(buf)
    }
    sb.toString
  }
}


class StreamWriteBin(out: OutputStream) extends StreamBase {

  val obj = out
  val canRead = false
  val canWrite = true

  override def write(x: Lbyte)  { out.write(x.byte) }
  override def write(x: Lblob)  { out.write(x.bytes) }
  override def write(x: Lchar)  { out.write(x.char) }
  override def write(x: Lstring){ out.write(x.str.getBytes) }
  override def newline()        { out.write('\n') }
  override def flush()          { out.flush() }
}

class StreamWriteText(writer: PrintStream) extends StreamBase {

  val obj = writer
  val canRead = false
  val canWrite = true

  override def write(x: Lbyte)  { writer.write(x.byte) }
  override def write(x: Lblob)  { writer.write(x.bytes) }
  override def write(x: Lchar)  { writer.print(x.char) }
  override def write(x: Lstring){ writer.print(x.str) }
  override def newline()        { writer.println() }
  override def flush()          { writer.flush() }
}

class StreamWriteTextToBytes(writer: PrintStream, val out: ByteArrayOutputStream) extends StreamWriteText(writer)
