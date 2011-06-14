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

object Listok {

  def lapply(env: Env, f: Lcommon, args: List[Lcommon]): Lcommon = f match {

     case fn: Lfunction => evalArgs(env, args)(fn.lapply(_,_))

     // don't eval args!
     case macro: Lmacro => macro.lapply(env, args)
     case sf: Lsform => sf.lapply(env, args)

     case s: Lsymbol => lapply(env, s.eval(env), args)
     case lm: Llambda => lapply(env, lm.eval(env), args)

 //  case ll: Llist => lapply(env, ll.eval(env), args)
     case err => throw TypeError("The value "+err.pp+" is not of type FUNCTION", env)
   }

  private def evalArgs(env: Env, args: List[Lcommon])(f: FuncCall) = {
    val t = env.allowTCO
    env.allowTCO = false
    val a = args.map(_.eval(env))
    //val a = GreenThreads.evalArgs(env, args)
    env.allowTCO = t
    f(env, a)
  }

  def eval(env: Env, forms: List[Lcommon]): Lcommon = {
  //  GreenThreads.eval(env, forms)
    var r: Lcommon = Lnil
    if (env.host.backtrace)
      for (form <- forms)
        r = env.backtrace(form){_.eval(env)}
    else
      for (form <- forms)
        r = form.eval(env)
    r
  }

  def eval(env: Env, text: String): Lcommon =
    //eval(env, Macro.macroexpand(env, parse(text)))
    eval(env, parse(text))

  def parse(text: String): List[Lcommon] = {
    Parser.read(dropComments(text)) match {
      case Right(l) => l
      case Left(msg) => throw ParserError(msg)
    }
  }

  private def dropComments(text: String) = text.split('\n') map { s =>
      s.indexOf(';') match {
        case -1 => s
        case n => s.substring(0, n)
      }
    } mkString("\n")

  // listok01
  // val bytecodesign = Array(108.toByte, 105.toByte, 115.toByte, 116.toByte, 111.toByte, 107.toByte, 48.toByte, 49.toByte)
  private lazy val bytecodesign = Array('l'.toByte, 'i'.toByte, 's'.toByte, 't'.toByte, 'o'.toByte, 'k'.toByte, '0'.toByte, '1'.toByte)

  def compile(env: Env, forms: List[Lcommon]): Array[Byte] =
    Array.concat(bytecodesign, Compiler.compile(forms, env))

  def compile(env: Env, text: String): Array[Byte] = compile(env, parse(text))

  def load(env: Env, in: java.io.InputStream): Lcommon = eval(env, loadAst(in))

  def loadAst(in: java.io.InputStream): List[Lcommon] =
    loadAst(builtin.Streams.readBytes(in))

  def loadAst(bytes: Array[Byte]): List[Lcommon] = {
    if (bytes.length >= bytecodesign.length)
    {
      val (sign, code) = bytes.splitAt(bytecodesign.length)
      if (bytecodesign.sameElements(sign)) {
        return Compiler.load(code)
      }
    }
    parse(new String(bytes))
  }


}

class Listok extends Host {
  lazy val root = Env.root(this)
  def eval(text: String) = Listok.eval(root, text)
  def load(in: java.io.InputStream) = Listok.load(root, in)
  def load(bytes: Array[Byte]) = Listok.eval(root, Listok.loadAst(bytes))
  def compile(forms: List[Lcommon]) = Listok.compile(root, forms)
  def compile(text: String) = Listok.compile(root, text)
}


