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

  def fileInputStream(path: String) = new FileInputStream(new File(path))
  def fileOutputStream(path:String) = new FileOutputStream(new File(path))
  def fileBufferedReader(path: String) = new BufferedReader(new FileReader(new File(path)))
  def filePrintStream(path:String) = new PrintStream(new FileOutputStream(new File(path)))
  def byteArrayBufferedReader(bytes: Array[Byte]) = new BufferedReader(new InputStreamReader(new ByteArrayInputStream(bytes)))
  def byteArrayPrintStream = new PrintStream(new ByteArrayOutputStream)
  def bufferedReader(in: InputStream) = new BufferedReader(new InputStreamReader(in))


  def makeConnection(host: String, port: Int, textMode: Boolean) = {
    val s = new Socket(host, port)
    val in = s.getInputStream
    val out = s.getOutputStream
    val url = host + ":" + port
    if (textMode)
      Lconnection(url, new StreamReadText(bufferedReader(in)), new StreamWriteText(new PrintStream(out)))
    else
      Lconnection(url, new StreamReadBin(in), new StreamWriteBin(out))
  }

  def makeConnection(url: String, direction: Symbol) = {
    val conn = (new URL(url)).openConnection.asInstanceOf[HttpURLConnection]
    val rcode = conn.getResponseCode
    if (rcode == 200) {
      (direction: @unchecked) match {
        case 'input =>
          Lconnection(url,
            new StreamReadText(bufferedReader(conn.getInputStream)),
            null)
        case 'output =>
          conn.setDoOutput(true)
          Lconnection(url,
            null,
            new StreamWriteText(new PrintStream(conn.getOutputStream)))
        case 'io =>
          conn.setDoOutput(true)
          Lconnection(url,
            new StreamReadText(bufferedReader(conn.getInputStream)),
            new StreamWriteText(new PrintStream(conn.getOutputStream)))
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

  def read(env: Env, args: List[Lcommon])(f: (StreamBase)=> Lcommon): Lcommon = {
    val s = if (args.length > 0)
      args.head.castStream(env).stm
    else
      env.getStandartInput.stm
    s.checkForRead(env)
    io(env){ f(s) }
  }

  def write (env: Env, args: List[Lcommon])(f: (StreamBase, Lcommon)=> Lcommon): Lcommon = {
    notLess(env, args, 1)
    val s = if (args.length > 1)
      args(1).castStream(env).stm
    else
      env.getStandartOutput.stm
    s.checkForWrite(env)
    io(env){ f(s, args.head) }
  }

  /// streams

  def func_open(env: Env, args: List[Lcommon]): Lcommon = {
    notLess(env, args, 1)

    val path = args.head.getString(env)

    val direction = if (args.length > 1)
        args(1).castKeyword(env).sym
      else 'input

    val mode = if (args.length > 2)
      args(2).castKeyword(env).sym
      else 'text

    if (mode == 'text) {
      io(env){
        direction match {
          case 'input  => Lstream(new StreamReadText(fileBufferedReader(path)))
          case 'output => Lstream(new StreamWriteText(filePrintStream(path)))
          case err => throw SyntaxError("The value " + err + " is not valid direction", env)
        }
      }
    } else { //if (mode == 'binary)

      io(env){
        direction match {
          case 'input  => Lstream(new StreamReadBin(fileInputStream(path)))
          case 'output => Lstream(new StreamWriteBin(fileOutputStream(path)))
          case err => throw SyntaxError("The value " + err + " is not valid direction", env)
        }
      }

    }
  }

  def func_open_tcp_conn(env: Env, args: List[Lcommon]): Lcommon = {
    notLess(env, args, 2) // open-socket host port [:text|:binary]
    val mode = if (args.length > 2)
      args(2).castKeyword(env).sym
      else 'text
    val host = args.head.getString(env)
    val port = args(1).getInt(env)
    io(env){ makeConnection(host, port, mode == 'text) }
  }

  def func_open_http_conn(env: Env, args: List[Lcommon]): Lcommon = {
    notLess(env, args, 1)
    val url = args.head.getString(env)

    val direction = if (args.length > 1)
      args(1).getSymbol(env)
      else 'input

    if (direction != 'input && direction != 'output && direction != 'io)
      throw SyntaxError("The value " + direction + " is not valid direction", env)

    io(env){ makeConnection(url, direction) }
  }

  def func_close(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    args.head match {
      case Lnil => // it's ok to close nil
      case c: Lconnection => c.close
      case s: Lstream => s.stm.close
      case err => throw TypeError("The value " + err + " is not STREAM", env)
    }
    Ltrue
  }

  def func_flush(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    val s = args.head.castStream(env)
    s.stm.flush
    s
  }

  def func_read(env: Env, args: List[Lcommon]): Lcommon = read(env, args) {
    _.readLine match {
        case Lstring(s) => Listok.parse(s) match {
          case Nil => Lnil
          case x::Nil => x
          case xs => Llist(xs)
        }
        case _ => Lnil
      }
  }

  def func_read_byte(env: Env, args: List[Lcommon]): Lcommon = read(env, args) { _.readByte }

  def func_read_blob(env: Env, args: List[Lcommon]): Lcommon = read(env, args) { _.readBlob }

  def func_read_char(env: Env, args: List[Lcommon]): Lcommon = read(env, args) { _.readChar }

  def func_read_text(env: Env, args: List[Lcommon]): Lcommon = read(env, args) { _.readString }

  def func_read_line(env: Env, args: List[Lcommon]): Lcommon = read(env, args) { _.readLine }


  def func_print(env: Env, args: List[Lcommon]): Lcommon =
    write(env, args) {(s,x) => s.write(Lstring(x.pp)); s.newline(); x}

  def func_write(env: Env, args: List[Lcommon]): Lcommon =
    write(env, args) {(s,x) => s.write(Lstring(x.pp)); x}

  def func_write_byte(env: Env, args: List[Lcommon]): Lcommon =
    write(env, args) {(s,x) => val b = Lbyte(Blob.toByte(env, x)); s.write(b); b}

  def func_write_blob(env: Env, args: List[Lcommon]): Lcommon =
    write(env, args) {(s,x) => s.write(x.castBlob(env)); x}

  def func_write_char(env: Env, args: List[Lcommon]): Lcommon =
    write(env, args) {(s,x) => s.write(x.castChar(env)); x }

  def func_write_string(env: Env, args: List[Lcommon]): Lcommon =
    write(env, args) {(s,x) => s.write(x.castString(env)); x}

  def func_write_line(env: Env, args: List[Lcommon]): Lcommon =
    write(env, args) {(s,x) => s.write(x.castString(env)); s.newline(); x }

  def func_terpri (env: Env, args: List[Lcommon]): Lcommon = {
    val s = if (args.length > 0)
      args(0).castStream(env).stm
    else
      env.getStandartOutput.stm
    s.checkForWrite(env)
    io(env){ s.newline(); Lnil }
  }


  def func_make_string_input_stream(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    val str = args.head.getString(env)
    Lstream(new StreamReadText(byteArrayBufferedReader(str.getBytes)))
  }

  def func_make_string_output_stream(env: Env, args: List[Lcommon]): Lcommon = {
    val out = new ByteArrayOutputStream
    Lstream(new StreamWriteTextToBytes(new PrintStream(out), out))
  }

  def func_make_blob_input_stream(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    val blob = args.head.castBlob(env)
    Lstream(new StreamReadBin(new ByteArrayInputStream(blob.bytes)))
  }

  def func_make_blob_output_stream(env: Env, args: List[Lcommon]): Lcommon = {
    Lstream(new StreamWriteBin(new ByteArrayOutputStream))
  }

  def func_get_output_stream_string(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    val s = args.head.castStream(env)
   s.stm match {
    case x: StreamWriteTextToBytes => Lstring(x.out.toString)
    case err => throw TypeError("The value " + err + " is not STRING-OUTPUT-STREAM", env)
   }
  }

  def func_get_output_stream_blob(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    val s = args.head.castStream(env)
    s.stm.obj match {
      case b: ByteArrayOutputStream => Lblob(b.toByteArray)
      case err => throw TypeError("The value " + err + " is not BLOB-OUTPUT-STREAM", env)
    }
  }

  def func_url_encode(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    val s = args.head.getString(env)
    Lstring(URLEncoder.encode(s, "UTF-8"))
  }

  def func_get_output_stream(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    args.head match {
      case Lprocess(_, p) => Lstream(new StreamWriteText(new PrintStream(p.getOutputStream)))
      case Lconnection(_, _, out) => Lstream(out)
      case err => throw TypeError("The value " + err + " is not PROCESS or CONNECTION", env)
    }
  }

  def func_get_input_stream(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    args.head match {
      case Lprocess(_, p) => Lstream(new StreamReadText(bufferedReader(p.getInputStream)))
      case Lconnection(_, in, _) => Lstream(in)
      case err => throw TypeError("The value " + err + " is not PROCESS or CONNECTION", env)
    }
  }

  def func_open_streamp(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    Util.toLbool(!l.head.castStream(env).stm.isClosed)
  }

  def func_input_streamp(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    Util.toLbool(l.head.castStream(env).stm.canRead)
  }

  def func_output_streamp(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    Util.toLbool(l.head.castStream(env).stm.canWrite)
  }


  val all = List (
    Lfunction(func_open, 'open),
    Lfunction(func_close, 'close),
    Lfunction(func_flush, 'flush), // rename to force_output ?
    Lfunction(func_read, 'read),
    Lfunction(func_read_byte, Symbol("read-byte")),
    Lfunction(func_read_blob, Symbol("read-blob")),
    Lfunction(func_read_char, Symbol("read-char")),
    Lfunction(func_read_text, Symbol("read-text")),
    Lfunction(func_read_line, Symbol("read-line")),
    Lfunction(func_print, 'print),
    Lfunction(func_write, 'write),
    Lfunction(func_write_byte, Symbol("write-byte")),
    Lfunction(func_write_blob, Symbol("write-blob")),
    Lfunction(func_write_char, Symbol("write-char")),
    Lfunction(func_write_string, Symbol("write-string")),
    Lfunction(func_write_line, Symbol("write-line")),
    Lfunction(func_terpri, 'terpri),
    Lfunction(func_terpri, 'newline),
    Lfunction(func_make_string_input_stream, Symbol("make-string-input-stream")),
    Lfunction(func_make_string_output_stream, Symbol("make-string-output-stream")),
    Lfunction(func_make_blob_input_stream, Symbol("make-blob-input-stream")),
    Lfunction(func_make_blob_output_stream, Symbol("make-blob-output-stream")),
    Lfunction(func_get_output_stream_string, Symbol("get-output-stream-string")),
    Lfunction(func_get_output_stream_blob, Symbol("get-output-stream-blob")),
    Lfunction(func_url_encode, Symbol("url-encode")),

    Lfunction(func_open_tcp_conn, Symbol("open-tcp-connection")),
  Lfunction(func_open_http_conn, Symbol("open-http-connection")),
    Lfunction(func_get_output_stream, Symbol("get-output-stream")),
    Lfunction(func_get_input_stream, Symbol("get-input-stream")),

    Lfunction(func_open_streamp, Symbol("open-stream-p")),
    Lfunction(func_input_streamp, Symbol("input-stream-p")),
    Lfunction(func_output_streamp, Symbol("output-stream-p"))
  )

}
