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


object SpecialForms {

  def form_define(env: Env, l: List[Lcommon]): Lcommon = {
    // (def name value)
    val s = l.head.castSymbol(env)
    val value = l.tail.length match {
      case 0 => Lnil
      case 1 => l.tail.head.eval(env)
      case _ => throw SyntaxError("Invalid number of argument to DEF in " + Util.pp(l), env)
    }
    env.define(s.sym, value)
    s
  }

  def form_defconstant(env: Env, l: List[Lcommon]): Lcommon = {
    // (def name value)
    val s = l.head.castSymbol(env)
    val value = l.tail.length match {
      case 0 => Lnil
      case 1 => l.tail.head.eval(env)
      case _ => throw SyntaxError("Invalid number of argument to DEFCONSTANT in " + Util.pp(l), env)
    }
    env.defineconst(s.sym, value)
    s
  }

  def form_setf(env: Env, l: List[Lcommon]): Lcommon = {
    // (setf name value)
    val s = l.head.castSymbol(env)
    l.tail.length match {
      case 1 =>
        val p = l.tail.head.eval(env)
        s.set(env, p)
        p
      case _ => throw SyntaxError("Invalid number of argument to SETF in " + Util.pp(l), env)
    }
      }

  def form_if(env: Env, l: List[Lcommon]): Lcommon = {
    // (set! name value)
    if (l.length > 1 && l.length < 4) {
      val r = l.head.eval(env) //cond
      if (Util.isTrue(r)) { //then
        l.tail.head.eval(env)
      }
      else if (l.length == 3) { //else
        l.tail.tail.head.eval(env)
      }
      else
        Lnil
    }
     else
      throw SyntaxError("Mailformed IF: " + Util.pp(l), env)
  }

  def form_cond(env: Env, l: List[Lcommon]): Lcommon = {
    var p = l
    var r:Lcommon = Lnil
    while (p != Nil) {
      p.head match {
        case Llist(l) if l.length > 0 =>
          r = l.head.eval(env) // check if
          p = if (Util.isTrue(r)) {
            if (l.tail != Nil)
              r = Listok.eval(env, l.tail)
            Nil // stop
          }
          else
            p.tail // next if
        case err => throw SyntaxError("COND clause is not a list: " + err.pp, env)
      }
    }
    r
  }

  def form_and(env: Env, l: List[Lcommon]): Lcommon = {
    var p = l
    var r :Lcommon = Lnil
    while (p != Nil) {
      r = p.head.eval(env)
      p = if (Util.isTrue(r))
        p.tail
      else
        Nil // stop
    }
    r
  }

  def form_or(env: Env, l: List[Lcommon]): Lcommon = {
    var p = l
    var r :Lcommon = Lnil
    while (p != Nil) {
      r = p.head.eval(env)
      p = if (Util.isTrue(r))
        Nil // stop
      else
        p.tail
    }
    r
  }

  def form_do(env: Env, l: List[Lcommon]): Lcommon = {
    /*
    (do ((var1 init1 step1)
         (var2 init2 step2)
          ...)
         (end-test result)
        statement1
        ...)
  */
  if (l.isEmpty)
    throw SyntaxError("Invalid number of argument in DO", env)

    val vars = l.head match {
      case Llist(xs) => xs
      case _ => throw SyntaxError(l.head.pp + " is an illegal form for a DO varlist", env)
    }

    val (endtest, result) = l.tail.head match {
      case Llist(xs) if xs.length == 1 =>
        (xs.head, Lnil) // end-test [result]
      case Llist(xs) if xs.length == 2 =>
        (xs.head, xs.tail.head)
      case err => throw SyntaxError("ill-formed DO, end-test is mailformed: " + err.pp, env)
    }

    //log("do-endtest=" + endtest + " result=" + result)

    val setf = SpecialForms.make('setf)
    val ll = List.newBuilder[Symbol]
    val args = List.newBuilder[Lcommon]
    val code = List.newBuilder[Lcommon]
    code ++= l.tail.tail // body

    vars.foreach { // var [init[step]]
      case Lsymbol(s) =>
        ll += s
        args += Lnil
      case Llist(xs) if xs.length == 2 =>
        xs.head match {
          case Lsymbol(s) =>
            ll += s
            args += xs.tail.head.eval(env)
          case _ => throw SyntaxError("ill-formed DO, var is not symbol", env)
        }
      case Llist(xs) if xs.length == 3 =>
        xs.head match {
          case Lsymbol(s) =>
            ll += s
            args += xs.tail.head.eval(env)
            code += LL(setf, Lsymbol(s), xs.tail.tail.head)
          case _ => throw SyntaxError("ill-formed DO, var is not symbol", env)
        }
      case err => throw SyntaxError("ill-formed DO, var's list is mailformed: " + err.pp, env)
    }

    // init env
    val e = Env('do, env, ll.result, args.result)

    // do loop
    def loop(): Unit =
      if (!Util.isTrue(endtest.eval(e))){
        Listok.eval(e, code.result)
        loop()
       }

    loop()

    result.eval(e)
  }

  def form_defun(env: Env, l: List[Lcommon]): Lcommon = {
    if (l.length < 3)
      throw SyntaxError("invalid number of elements in DEFUN: " + Util.pp(l), env)
    val name = l.head match {
      case s: Lsymbol => s
      case _ => throw SyntaxError("not legal function name: " + l.head.pp, env)
    }
    //val body = l.tail.tail
    val ll = l.tail.head match {
      case Llist(l) => l
      case _ => throw SyntaxError("the DEFUN has a missing or non-list lamba list: " + Util.pp(l), env)
    }

    val (body,istco) = LambdaCalculus.makeBody(name.sym, l.tail.tail)
    val fn = LambdaCalculus.makeFunc(env, name.sym, ll, body)
    val lfn = if (istco) {
      new Lfunction(fn, name.sym) with TCOFunction
    } else
      Lfunction(fn, name.sym)

    env.defineconst(name.sym, lfn)
    name
  }

  def form_match(env: Env, l: List[Lcommon]): Lcommon = {
    if (l.length < 2)
      throw SyntaxError("invalid number of elements in MATCH: " + Util.pp(l), env)
    val value = l.head.eval(env)
    PatternMatching.runMatches(env, l.tail, value)
  }

  // (defstruct name (x d f))
  def form_defstruct(env: Env, l: List[Lcommon]): Lcommon = {
    if (l.length < 1)
      throw SyntaxError("invalid number of elements in DEFSTRUCT: " + Util.pp(l), env)
    val name = l.head match {
      case Lsymbol(s) => s
      case Lkeyword(s) => s
      case err => throw TypeError("The value "+err.pp+" is not of type SYMBOL", env)
    }
    val fields = l.tail

    val fieldsSymbols = fields map {
      case Lsymbol(s) => s
        // todo: :type and default value
      case err => throw TypeError("The slot name "+err.pp+" is not of type SYMBOL", env)
    }

    // define ctor make-name
    val ctor: FuncCall = (env: Env, args: List[Lcommon]) => {
      if (args.length != fields.length)
        throw SyntaxError("invalid number of elements in DEFSTRUCT: " + Util.pp(l), env)
      val m = scala.collection.mutable.Map((fieldsSymbols zip args) :_*)
      Lstruct(name, m)
    }

    val ctorname = Symbol("make-" + Util.pp(name))
    env.defineconst(ctorname, Lfunction(ctor, ctorname))

    for (field <- fieldsSymbols) {

      val fnget = (env: Env, args: List[Lcommon]) => {
        if (args.length != 1)
          throw SyntaxError("Invalid number of argument: " + args.length, env)
        args.head match {
          case st: Lstruct => st.get(field)
          case err => throw TypeError("The value "+err.pp+" is not of type STRUCT", env)
        }
      }

      val fnset = (env: Env, args: List[Lcommon]) => {
        if (args.length != 2)
          throw SyntaxError("Invalid number of argument: " + args.length, env)
        args.head match {
          case st: Lstruct => st.set(field, args(1))
          case err => throw TypeError("The value "+err.pp+" is not of type STRUCT", env)
        }
      }

      val fnname = Util.pp(name) + "-" + Util.pp(field)
      val fngetname = Symbol(fnname)
      val fnsetname = Symbol("set-" + fnname)

      env.defineconst(fngetname, Lfunction(fnget, fngetname))
      env.defineconst(fnsetname, Lfunction(fnset, fnsetname))
    }

    Lsymbol(name)
  }

  def form_assert(env: Env, l: List[Lcommon]): Lcommon = {
    if (env.host.debug) {
      if (l.length < 1)
        throw SyntaxError("invalid number of elements in ASSERT: " + Util.pp(l), env)
      if (!Util.isTrue(l.head.eval(env))) {
        val msg = if (l.length > 1)
          l.head.pp + " " + l(1).pp
        else
          l.head.pp
        env.host.onassert(env, msg)
      }
    }
    Lnil
  }

  def make(name: Symbol) = name match {
    case 'def => Lsform(form_define _, name)
    case 'defconstant => Lsform(form_defconstant _, name)
    case 'defun => Lsform(form_defun _, name)
    case 'setf => Lsform(form_setf _, name)
    case 'if => Lsform(form_if _, name)
    case 'cond => Lsform(form_cond _, name)
    case 'do => Lsform(form_do _, name)
    case 'and => Lsform(form_and _, name)
    case 'or => Lsform(form_or _, name)
    case 'spawn => Lsform(Concurrent.form_spawn _, name)
    case 'match => Lsform(form_match _, name)
    case 'defstruct => Lsform(form_defstruct _, name)
    case 'assert => Lsform(form_assert _, name)
    case _ => bugcheck("unable make unknown sform: " + name)
  }

}
