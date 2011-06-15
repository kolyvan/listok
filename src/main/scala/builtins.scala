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
import util.parsing.json.JSON

trait Helpers {

  def mustEqual (env: Env, args: List[Lcommon], need: Int) {
    if (args.length != need)
      throw SyntaxError("Invalid number of argument: " + args.length, env)
  }

  def notLess (env: Env, args: List[Lcommon], n: Int) {
    if (args.length < n)
      throw SyntaxError("Invalid number of argument: " + args.length, env)
  }

}


object Common extends Helpers {

  def func_eq(env: Env, args: List[Lcommon]): Lcommon = {
    Util.toLbool(args.tail.forall(args.head == _))
  }

  def func_noteq(env: Env, args: List[Lcommon]): Lcommon = {
    Util.toLbool(args.tail.forall(args.head != _))
  }

  def func_not(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    Util.toLbool(!Util.isTrue(l.head.eval(env)))
   }


  def func_eval(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    //log("eval:" + Util.pp(l))
    Listok.eval(env, l)
  }

  def func_apply(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 2)
    val xs = l(1).castList(env).seq
    Listok.lapply(env, l.head, xs) // it's equal to - Llist(f :: xs).eval(env)

   // val args = l.tail.head
   // args match {
   //    case Llist(xs) =>
   //      val f = l.head
   //      Listok.lapply(env, f, xs) // it's equal to - Llist(f :: xs).eval(env)
   //    case err  =>  throw TypeError("The value "+ err +" is not of type LIST", env)
   //  }
  }

  def func_load(env: Env, l: List[Lcommon]): Lcommon = {
    notLess(env, l, 1)
    val name = l.head.getString(env)

    if (l.length > 1) {
      val prefix = l(1).getSymbol(env)
      val e = Env(prefix, env)
      val r = env.host.onload(e, name)
      env.load(e)
      r
    } else {
      env.host.onload(env, name)
    }

  }

  def func_exit(env: Env, l: List[Lcommon]): Lcommon = {
    val status = if (l.length > 0)
      try { l.head.getInt(env) }
      catch { case _ => -1 }
    else 0
    env.host.onexit(env, status)
    Lnil
  }

  def func_error(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    val message = l.head.getString(env)
    env.host.onerror(env, message)
  }

  def func_break(env: Env, l: List[Lcommon]): Lcommon = {
    if (env.host.debug) {
      mustEqual(env, l, 1)
      val message = l.head.getString(env)
      env.host.onbreak(env, message)
    }
    Lnil
  }

  /*
  def func_assert(env: Env, l: List[Lcommon]): Lcommon = {
    notLess(env, l, 1)
    if (!Util.isTrue(l.head)) {
      val msg = if (l.length > 1) l(1).pp else ""
      env.host.onassert(env, msg)
    }
    Lnil
  }
  */

  def func_trace(env: Env, l: List[Lcommon]): Lcommon = {
    notLess(env, l, 1)
    val s = l.map {
      case Lsymbol(s) => s
      case Lfunction(_, s) => s
    //  case Ltcofunction(_, s) => s
      case err => throw TypeError("The value "+ err +" is not of type symbol or function", env)
    }
    env.host.addtrace(s)
    Llist(l)
  }

  def func_untrace(env: Env, l: List[Lcommon]): Lcommon = {
    notLess(env, l, 1)
    val s = l.map {
      case Lsymbol(s) => s
      case Lfunction(_, s) => s
    //  case Ltcofunction(_, s) => s
      case err => throw TypeError("The value "+ err +" is not of type symbol or function", env)
    }
    env.host.remtrace(s)
    Llist(l)
  }

  ///

  def func_curry(env: Env, l: List[Lcommon]): Lcommon = {
    notLess(env, l, 2)
    l.head match {
      case f:Lfunction  =>
        Lfunction(
          (env2: Env, l2: List[Lcommon]) => f.lapply(env2, l.tail ::: l2),
          'curry)
    //  case f:Ltcofunction  =>
    //    Lfunction(
    //      (env2: Env, l2: List[Lcommon]) => f.lapply(env2, l.tail ::: l2),
    //      'curry)
      case err => throw TypeError(err.pp + " can't be converted to type STRING", env)
    }
  }

  ////

  def func_tostr(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    l.head match {
      case Lnil => Lstring("")
      case Ltrue => Lstring("true")
      case Lint(i) => Lstring(i.toString)
      case Lfloat(f) => Lstring(f.toString)
      case Lchar(c) => Lstring(c.toString)
      case s: Lstring => s
      case Lkeyword(k) => Lstring(Util.pp(k))
      case ls: Lseq => Lstring("").make(ls.seq)
      case Lregex(s) => Lstring(s)
      case err => throw TypeError(err.pp + " can't be converted to type STRING", env)
    }
  }

   def func_tochar(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    l.head match {
      case Lint(i) => Lchar(i.toChar)
      case ch: Lchar => ch
      case err => throw TypeError(err.pp + " can't be converted to type CHAR", env)
    }
  }

  def func_toint(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    l.head match {
      case Lnil => Lint(0)
      case Ltrue => Lint(1)
      case i:Lint => i
      case Lfloat(f) => Lint(f.toInt)
      case Lchar(c) => Lint(c.toInt)
      case Lstring(s) => Lint(s.toInt)
      case err => throw TypeError(err.pp + " can't be converted to type STRING", env)
    }
  }

  def func_tofloat(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    l.head match {
      case Lint(i) => Lfloat(i.toFloat)
      case f: Lfloat => f
      case Lstring(s) => Lfloat(s.toFloat)
      case err => throw TypeError(err.pp + " can't be converted to type STRING", env)
    }
  }

  def func_tolist(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    l.head match {
      case s:Lseq if s.isEmpty  => Lnil
      case s:Lseq  => Llist(s.seq.toList)
     // case s: Lstruct => s.fields
      case err => throw TypeError(err.pp + " can't be converted to type STRING", env)
    }
  }

  def func_tovector(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    l.head match {
      case s:Lseq  => Lvector(scala.collection.mutable.ArraySeq(s.seq:_*))
      case err => throw TypeError(err.pp + " can't be converted to type VECTOR", env)
    }
  }


/// predicats

  def func_atom(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    l.head match {
      case Lnil => Ltrue
      case x: Latom => Ltrue
      case _ => Lnil
    }
  }

  def func_listp(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    l.head match {
      case Lnil => Ltrue
      case x: Llist => Ltrue
      case _ => Lnil
    }
  }

   def func_null(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    l.head match {
      case Lnil => Ltrue
      case xs: Llist if xs.length == 0 => Ltrue
      case _ => Lnil
    }
  }

  def func_sequencep(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    l.head match {
      case Lnil => Ltrue
      case x: Lseq => Ltrue
      case _ => Lnil
    }
  }

  def func_keywordp(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    l.head match {
      case x: Lkeyword => Ltrue
      case _ => Lnil
    }
  }

  def func_numberp(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    l.head match {
      case x: Lnumeric => Ltrue
      case _ => Lnil
    }
  }

  def func_symbolp(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    l.head match {
      case x: Lsymbol => Ltrue
      case _ => Lnil
    }
  }

  def func_functionp(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    l.head match {
      case lm: Llambda => Ltrue
      case fn: Lfunction => Ltrue
      case _ => Lnil
    }
  }

  def func_stringp(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    l.head match {
      case x: Lstring => Ltrue
      case _ => Lnil
    }
  }

  def func_vectorp(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    l.head match {
      case x: Lvector => Ltrue
      case _ => Lnil
    }
  }

  def func_charp(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    l.head match {
      case x: Lchar => Ltrue
      case _ => Lnil
    }
  }

  def func_streamp(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    l.head match {
      case x: Lstream => Ltrue
      case _ => Lnil
    }
  }

  def func_regexp(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    l.head match {
      case x: Lregex => Ltrue
      case _ => Lnil
    }
  }

  def func_hashtablep(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    l.head match {
      case x: Lhashtable => Ltrue
      case _ => Lnil
    }
  }

  def func_hashmapp(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    l.head match {
      case x: Lhashmap => Ltrue
      case _ => Lnil
    }
  }

  def func_threadp(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    l.head match {
      case x: Lthread => Ltrue
      case _ => Lnil
    }
  }

  def func_mailslotp(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    l.head match {
      case x: Lmailslot => Ltrue
      case _ => Lnil
    }
  }

  def func_structp(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    l.head match {
      case x: Lstruct => Ltrue
      case _ => Lnil
    }
  }

  def func_lazyseqp(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    l.head match {
      case x: Llazyseq => Ltrue
      case _ => Lnil
    }
  }

  def func_pairp(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    l.head match {
      case x: Lpair => Ltrue
      case _ => Lnil
    }
  }

 ///

  def func_format(env: Env, l: List[Lcommon]): Lcommon = {
    notLess(env, l, 1)
    val s = l.head.getString(env)
    Lstring(s.format(l.tail.map(_.getAny):_*))
  }

  def func_display(env: Env, l: List[Lcommon]): Lcommon = {
    if (l.nonEmpty) {
      val s = l.head.getString(env)
      Console.printf(s, l.tail.map(_.getAny):_*)
    }
    else {
      Console.println("")
    }
    Lnil
  }

  def func_current_time(env: Env, l: List[Lcommon]): Lcommon = {
    val ms = System.currentTimeMillis()
    Lpair(Lint((ms / 1000).toInt), Lint((ms % 1000).toInt))
  }

  def func_current_directory(env: Env, l: List[Lcommon]): Lcommon = {
    Lstring(System.getProperty("user.dir"))
  }


  def func_json_parse(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    val s = l.head.getString(env)
    JSON.parseFull(s) match {
      case Some(x) => Util.toLcommon(x)
      case None => Lnil
    }
  }


  //

  def func_gensym(env: Env, l: List[Lcommon]): Lcommon = Macro.gensym

  def func_macroexpand(env: Env, l: List[Lcommon]): Lcommon = {
    mustEqual(env, l, 1)
    Macro.macroexpand(env, l) match {
      case Nil => Lnil
      case x::Nil => x
      case xs => Llist(xs)
    }
  }


 ///

  val all = List (
    Lfunction(func_eq, '=),
    Lfunction(func_noteq, '/=),
    Lfunction(func_eq, 'eq),
    Lfunction(func_not, 'not),
    Lfunction(func_eval, 'eval),
    Lfunction(func_apply, 'apply),
    Lfunction(func_load, 'load),
    Lfunction(func_exit, 'exit),
    Lfunction(func_error, 'error),
    Lfunction(func_break, 'break),
  //  Lfunction(func_assert, 'assert),
    Lfunction(func_trace, 'trace),
    Lfunction(func_untrace, 'untrace),
    Lfunction(func_curry, 'curry),
    Lfunction(func_display, 'display),
    Lfunction(func_current_time, Symbol("current-time")),
    Lfunction(func_current_directory, Symbol("current-directory")),
    Lfunction(func_json_parse, Symbol("json-parse")),

    Lfunction(func_tostr, Symbol("to-str")),
    Lfunction(func_tochar, Symbol("to-char")),
    Lfunction(func_toint, Symbol("to-int")),
    Lfunction(func_tofloat, Symbol("to-float")),
    Lfunction(func_tolist, Symbol("to-list")),
    Lfunction(func_tovector, Symbol("to-vector")),


    Lfunction(func_atom, 'atom),
    Lfunction(func_listp, 'listp),
    Lfunction(func_null, 'null),
    Lfunction(func_sequencep, 'sequencep),
    Lfunction(func_keywordp, 'keywordp),
    Lfunction(func_numberp, 'numberp),
    Lfunction(func_symbolp, 'symbolp),
    Lfunction(func_functionp, 'functionp),
    Lfunction(func_stringp, 'stringp),
    Lfunction(func_vectorp, 'vectorp),
    Lfunction(func_charp, 'charp),
    Lfunction(func_streamp, 'streamp),
    Lfunction(func_regexp, 'regexp),
    Lfunction(func_hashtablep, 'hashtablep),
    Lfunction(func_hashmapp, 'hashmapp),
    Lfunction(func_threadp, 'threadp),
    Lfunction(func_mailslotp, 'mailslotp),
    Lfunction(func_structp, 'structp),
    Lfunction(func_lazyseqp, 'lazyseqp),
    Lfunction(func_pairp, 'pairp),

    Lfunction(func_format, 'format),
    Lfunction(func_gensym, 'gensym),
    Lfunction(func_macroexpand, 'macroexpand)

  )

}