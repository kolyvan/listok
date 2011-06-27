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

trait Typecast { self: Lcommon =>

  def castChar(env: Env) = self match {
      case x: Lchar => x
      case err => throw TypeError("The value " + err + " is not of type CHAR", env)
    }

  def castInt(env: Env) = self match {
      case i: Lint => i
      case l: Llong => Lint(l.int)
      case b: Lbignum => Lint(b.int)
      case r: Lratio => Lint(r.int)
      case err => throw TypeError("The value " + err + " is not of type INTEGER", env)
    }

  def castFloat(env: Env) = self match {
      case x: Lfloat => x
      case err => throw TypeError("The value " + err + " is not of type FLOAT", env)
    }

  def castSeq(env: Env) = self match {
     case Lnil => Llist(Nil)
     case s: Lseq => s
     case err => throw TypeError("The value " + err + " is not sequence", env)
  }

  def castList(env: Env) = self match {
      case Lnil => Llist(Nil)
      case x: Llist => x
      case err => throw TypeError("The value " + err + " is not of type LIST", env)
    }

  def castString(env: Env) = self match {
      // case Lnil => Lstring("")
      case x: Lstring => x
      case err => throw TypeError("The value " + err + " is not of type STRING", env)
    }

  def castSymbol(env: Env) = self match {
      case x: Lsymbol => x
      case err => throw TypeError("The value " + err + " is not of type SYMBOL", env)
    }

  def castKeyword(env: Env) = self match {
      case x: Lkeyword => x
      case err => throw TypeError("The value " + err + " is not of type KEYWORD", env)
    }

  def castFunction(env: Env) = self match {
      case x: Lfunction => x
      case err => throw TypeError("The value " + err + " is not of type FUNCTION", env)
    }

  def castVector(env: Env) = self match {
      case x: Lvector => x
      case err => throw TypeError("The value " + err + " is not of type VECTOR", env)
    }

  def castStream(env: Env) = self match {
      case x: Lstream => x
      case err => throw TypeError("The value " + err + " is not of type STREAM", env)
    }

  def castHashmap(env: Env) = self match {
      case x: Lhashmap => x
      case err => throw TypeError("The value " + err + " is not of type HASHMAP", env)
    }

  def castHashtable(env: Env) = self match {
      case x: Lhashtable => x
      case err => throw TypeError("The value " + err + " is not of type HASHTABLE", env)
    }

  def castRegex(env: Env) = self match {
      case Lstring(s) => Lregex(s)
      case r: Lregex => r
      case err => throw throw TypeError("The value " + err + " is not REGEX or STRING", env)
    }

  def castProcess(env: Env) = self match {
      case p: Lprocess => p
      case err => throw throw TypeError("The value " + err + " is not PROCESS", env)
    }

   def castNumeric(env: Env): Lnumeric = self match {
      case n: Lnumeric => n
      case err => throw TypeError("The value " + err + " is not of type NUMBER", env)
    }

  def castByte(env: Env) = self match {
      case b: Lbyte => b
      case err => throw TypeError("The value " + err + " is not of type BYTE", env)
    }

  def castBlob(env: Env) = self match {
      case b: Lblob => b
      case err => throw TypeError("The value " + err + " is not of type BLOB", env)
    }

  def getString(env: Env) = self match {
    case Lstring(s) => s
    case Lkeyword(k) => Util.pp(k)
    case err => throw TypeError("The value "+ err +" is not of type STRING or KEYWORD", env)
  }

  def getSymbol(env: Env) = self  match {
    case Lstring(s) => Symbol(s)
    case Lkeyword(k) => k
    case Lsymbol(s) => s
    case err => throw TypeError("The value "+ err +" is not of type STRING, SYMBOL or KEYWORD", env)
  }

  def getInt(env: Env) = castInt(env).int
  def getChar(env: Env) = castChar(env).char

  def getAny: Any = self match {
    case Lchar(ch) => ch
    case Lint(i) => i
    case Llong(l) => l
    case Lbignum(bi) => bi
    case Lratio(r) => r
    case Lfloat(f) => f
    case Lstring(s) => s
    case Lsymbol(s) => s
    case Lkeyword(k) => Util.pp(k)
    case Lpair(a,b) => (a.getAny,b.getAny)
    case s: Lseq => s.seq.map(_.getAny)
    case Lnil => Nil
    case Ltrue => true
    case Lquote(x) => x.getAny
    case Lhashtable(m) => m.map(p => (p._1.getAny, p._2.getAny))
    case Lthread(t) => t
    case r: Lregex => r.pattern
    case Lprocess(cmd, p) => p
    case Lwrapper(x) => x
    case Lbyte(b) => b
    case Lblob(a) => a
    case Lstream(s) => s.obj
   // case Lstruct(_, m) => ?
   // case Llazyseq(s) => ?
    case x => x.pp
  }

}
