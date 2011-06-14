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
import java.io._
import java.net.{Socket, URL, HttpURLConnection, URLEncoder}


object Streams extends Helpers {

    //  from lift
  def readBytes(in: InputStream): Array[Byte] = {
    val bos = new ByteArrayOutputStream
    val ba = new Array[Byte](4096)

    def readOnce() {
      val len = in.read(ba)
      if (len > 0) bos.write(ba, 0, len)
      if (len >= 0) readOnce()
    }

    readOnce()

    bos.toByteArray
  }

  def readString(is: InputStream, charset: String) = {
    val in = new InputStreamReader(is, charset)
    val bos = new StringBuilder
    val ba = new Array[Char](4096)

    def readOnce() {
      val len = in.read(ba)
      if (len > 0) bos.appendAll(ba, 0, len)
      if (len >= 0) readOnce()
    }

    readOnce()

    bos.toString
  }



  def readFile(path: String): Array[Byte] = {
    val fs = new FileInputStream(new File(path))
    try { readBytes(fs) }
    finally { fs.close }
  }

  def writeFile(path: String, bytes: Array[Byte]): Unit =
    writeFile(new File(path), bytes)

  def writeFile(path: File, bytes: Array[Byte]): Unit = {
    val fs = new FileOutputStream(path)
    try { fs.write(bytes) }
    finally { fs.close }
  }

  def withFile(path: String)(fn: (InputStream) => Lcommon) = {
    val fs = new FileInputStream(path)
    try { fn(fs) }
    finally { fs.close }
  }

  // def readByte = in.read match {}

  def readObject(reader: BufferedReader): Lcommon = reader.readLine match {
    case null => Lnil
    case s => Listok.parse(s) match {
      case Nil => Lnil
      case x::Nil => x
      case xs => Llist(xs)
    }
  }

  def readChar(reader: BufferedReader) = reader.read match {
    case -1 => Lnil
    case x => Lchar(x.toChar)
  }

  def readString(reader: BufferedReader) = {
    val sb = new StringBuilder
    val buf = new Array[Char](4096)
    var n = reader.read(buf)
    while (n >= 0) {
      sb.appendAll(buf, 0, n)
      n = reader.read(buf)
    }
    Lstring(sb.toString)
  }

  def readLine(reader: java.io.BufferedReader) = reader.readLine match {
    case null => Lnil
    case x => Lstring(x)
  }

  // def writeByte
  def writeObject(writer: PrintStream, x:Lcommon) = {writer.print(x.pp);x }
  def writeChar(writer: PrintStream, x: Lchar) = {writer.print(x.char);x}
  def writeString(writer: PrintStream, x: Lstring) = {writer.print(x.str);x}
  def writeLine(writer: java.io.PrintStream, x: Lstring) = {writer.println(x.str);x}
  def newline(writer: java.io.PrintStream) = {writer.println();Lnil}

  def fileBufferedReader(path: String) = new BufferedReader(new FileReader(new File(path)))
  def filePrintStream(path:String) = new PrintStream(new FileOutputStream(new File(path)))
  def byteArrayBufferedReader(bytes: Array[Byte]) = new BufferedReader(new InputStreamReader(new ByteArrayInputStream(bytes)))
  def byteArrayPrintStream = new PrintStream(new ByteArrayOutputStream)
  def bufferedReader(in: InputStream) = new BufferedReader(new InputStreamReader(in))

  def makeSocketStream(s: java.net.Socket) = {
    val in = s.getInputStream
    val out = s.getOutputStream
    Lstream(
      bufferedReader(in),
      new PrintStream(out),
      in,
      out)
  }

  def makeConnection(env: Env, url: String, direction: Symbol) = {
    val conn = (new URL(url)).openConnection.asInstanceOf[HttpURLConnection]
    val rcode = conn.getResponseCode
    if (rcode == 200) {
      direction match {
        case 'input =>
          Lstream(bufferedReader(conn.getInputStream), null)
        case 'output =>
          conn.setDoOutput(true)
          Lstream(null, new PrintStream(conn.getOutputStream))
        case 'io =>
          conn.setDoOutput(true)
          Lstream(bufferedReader(conn.getInputStream), new PrintStream(conn.getOutputStream))
        case err =>
          throw SyntaxError("The value " + err + " is not valid direction", env)
      }
    }
    else {
      log("openconnection response code: " + rcode)
      Lnil
    }
  }

  def io(env: Env)(f: => Lcommon): Lcommon = {
    try { f } catch {
      case ex: java.io.IOException =>
        env.host.onwarning(env, ex.toString)
        Lnil
    }
  }

  /// streams

  def func_open(env: Env, args: List[Lcommon]): Lcommon = {
    notLess(env, args, 1)
    val path = args.head.getString(env)
    val direction = if (args.length == 2)
        args(1).castKeyword(env).sym
      else 'input

    io(env){
      direction match {
        case 'input  => Lstream(fileBufferedReader(path), null)
        case 'output => Lstream(null, filePrintStream(path))
        case 'io     => Lstream(fileBufferedReader(path), filePrintStream(path))
        case err => throw SyntaxError("The value " + err + " is not valid direction", env)
      }
    }
  }

  def func_open_socket(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2) // open-socket host port
    val host = args.head.getString(env)
    val port = args(1).getInt(env)
    io(env){ makeSocketStream(new Socket(host, port)) }
  }

  def func_open_url(env: Env, args: List[Lcommon]): Lcommon = {
    notLess(env, args, 1)
    val url = args.head.getString(env)
    val direction = if (args.length == 2) args(1).getSymbol(env) else 'input
    io(env){ makeConnection(env, url, direction) }
  }

  def func_close(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    args.head match {
      case Lnil => // it's ok to close nil
      case s: Lstream => s.close
      case err => throw TypeError("The value " + err + " is not STREAM", env)
    }
    Ltrue
  }

  def func_read(env: Env, args: List[Lcommon]): Lcommon = {
    if (args.length > 0) {
      val s = args.head.castStream(env)
      s.checkForRead(env)
      io(env){ readObject(s.reader) }
    }
    else
      io(env){ readObject(env.getStandartInput.reader) }
  }

  def func_read_char(env: Env, args: List[Lcommon]): Lcommon = {
    if (args.length > 0) {
      val s = args.head.castStream(env)
      s.checkForRead(env);
      io(env){ readChar(s.reader) }
    } else
      io(env){ readChar(env.getStandartInput.reader) }
  }

  def func_read_text(env: Env, args: List[Lcommon]): Lcommon = {
    val reader = if (args.length > 0) {
      val s = args.head.castStream(env)
      s.checkForRead(env);
      s.reader
    } else
      env.getStandartInput.reader
    io(env){ readString(reader) }
  }

  def func_read_line(env: Env, args: List[Lcommon]): Lcommon = {
    if (args.length > 0) {
      val s = args.head.castStream(env)
      s.checkForRead(env);
      io(env){ readLine(s.reader) }
    } else
      io(env){ readLine(env.getStandartInput.reader) }
  }

  def func_print(env: Env, args: List[Lcommon]): Lcommon = {
    notLess(env, args, 1)
    if (args.length > 1) {
      val s = args(1).castStream(env)
      s.checkForWrite(env);
      io(env){
        newline(s.writer)
        writeObject(s.writer, args.head)
      }
    } else {
      val writer = env.getStandartOutput.writer
      io(env){
        newline(writer)
        writeObject(writer, args.head)
      }
    }
  }

  def func_write(env: Env, args: List[Lcommon]): Lcommon = {
    notLess(env, args, 1)
    if (args.length > 1) {
      val s = args(1).castStream(env)
      s.checkForWrite(env);
      io(env){ writeObject(s.writer, args.head) }
    } else {
      val writer = env.getStandartOutput.writer
      io(env){ writeObject(writer, args.head) }
    }
  }

  def func_write_char(env: Env, args: List[Lcommon]): Lcommon = {
    notLess(env, args, 1)
    val ch = args.head.castChar(env)
    if (args.length > 1) {
      val s = args(1).castStream(env)
      s.checkForWrite(env);
      io(env){ writeChar(s.writer, ch) }
    } else
      io(env){ writeChar(env.getStandartOutput.writer, ch) }
  }

  def func_write_string(env: Env, args: List[Lcommon]): Lcommon = {
    notLess(env, args, 1)
    val str = args.head.castString(env)
    if (args.length > 1) {
      val s = args(1).castStream(env)
      s.checkForWrite(env);
      io(env){ writeString(s.writer, str) }
    } else
      io(env){ writeString(env.getStandartOutput.writer, str) }
  }

  def func_write_line(env: Env, args: List[Lcommon]): Lcommon = {
    notLess(env, args, 1)
    val str = args.head.castString(env)
    if (args.length > 1) {
      val s = args(1).castStream(env)
      s.checkForWrite(env);
      io(env){ writeLine(s.writer, str) }
    } else
      io(env){ writeLine(env.getStandartOutput.writer, str) }
  }

  def func_terpri (env: Env, args: List[Lcommon]): Lcommon = {
    if (args.length > 0) {
      val s = args.head.castStream(env)
      s.checkForWrite(env);
      io(env){ newline(s.writer) }
    } else
      io(env){ newline(env.getStandartOutput.writer) }
  }

  def func_make_string_input_stream(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    val str = args.head.getString(env)
    Lstream(byteArrayBufferedReader(str.getBytes), null)
  }

  def func_make_string_output_stream(env: Env, args: List[Lcommon]): Lcommon = {
    val out = new ByteArrayOutputStream
    Lstream(null, new PrintStream(out), null, out)
  }

  def func_get_output_stream_string(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    val s = args.head.castStream(env)
    s.out match {
      case b: ByteArrayOutputStream => Lstring(b.toString)
      case err => throw TypeError("The value " + err + " is not STRING-OUTPUT-STREAM", env)
    }
  }

  def func_url_encode(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    val s = args.head.getString(env)
    Lstring(URLEncoder.encode(s, "UTF-8"))
  }

  val all = List (
    Lfunction(func_open, 'open),
    Lfunction(func_open_socket, Symbol("open-socket")),
    Lfunction(func_open_url, Symbol("open-url")),
    Lfunction(func_close, 'close),
    Lfunction(func_read, 'read),
    Lfunction(func_read_char, Symbol("read-char")),
    Lfunction(func_read_text, Symbol("read-text")),
    Lfunction(func_read_line, Symbol("read-line")),
    Lfunction(func_print, 'print),
    Lfunction(func_write, 'write),
    Lfunction(func_write_char, Symbol("write-char")),
    Lfunction(func_write_string, Symbol("write-string")),
    Lfunction(func_write_line, Symbol("write-line")),
    Lfunction(func_terpri, 'terpri),
    Lfunction(func_terpri, 'newline),
    Lfunction(func_make_string_input_stream, Symbol("make-string-input-stream")),
    Lfunction(func_make_string_output_stream, Symbol("make-string-output-stream")),
    Lfunction(func_get_output_stream_string, Symbol("get-output-stream-string")),
    Lfunction(func_url_encode, Symbol("url-encode"))

  )

}

/*
abstract class Stream {

  protected var closed = false
  def isClosed = closed
  def close: Unit
  def canRead = reader != null
  def canWrite = writer != null
  def reader: BufferedReader
  def writer: PrintStream

  def checkForWrite(env: Env) {
     if (!canWrite)
       throw SyntaxError("Unable write to " + direction + " stream", env)
     if (isClosed)
       throw SyntaxError("The stream is closed", env)
   }

   def checkForRead(env: Env) {
     if (!canRead)
       throw SyntaxError("Unable read from " + direction + " stream", env)
     if (isClosed)
       throw SyntaxError("The stream is closed", env)
   }

  def direction = (reader, writer) match {
    case (null, null) => 'failed
    case (_, null) => 'input
    case (null, _) => 'output
    case _ => 'io
  }

}

class BiStream(val reader: BufferedReader,
                    val writer: PrintStream,
                    val in: InputStream=null,
                    val out: OutputStream =null) extends Stream {

  def close {
    closed = true
    if (in != null) in.close
    if (out != null) out.close
    if (reader != null) reader.close
    if (writer != null) writer.close
  }
}
*/
