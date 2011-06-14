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


object LambdaCalculus {

  def make (env: Env, name: Symbol, lambda_list: List[Lcommon], body: List[Lcommon]): Lfunction =
    Lfunction(makeFunc(env, name, lambda_list, body), name)

  def makeFunc (env: Env, name: Symbol, lambda_list: List[Lcommon], body: List[Lcommon]): FuncCall = {

    val ll = makeLambdaList(env, lambda_list)
    val code = body

    //log(" make" + name + " " + env.pp)

    (caller: Env, xargs: List[Lcommon]) => {
      if (Nil == body)
        Lnil
      else {

        val args = if (ll.length != lambda_list.length) {
          val (a,rest) = xargs.splitAt(ll.length - 1) // prepare &rest
          a :+ (rest match {
            case Nil => Lnil
            case _ => Llist(rest)
          })
          //a :+ Llist(rest)
        } else xargs

        if (ll.length != args.length)
            throw SyntaxError("Invalid number of arguments: " + args.length, env)

        val fnenv = Env(name, env, ll, args)
        fnenv.setCaller(caller)

      //  log(" call" + name + " " + env.pp + " " + fnenv.pp)

        if (env.host.traceMode)
          env.host.ontrace(fnenv, name, args) {
            Listok.eval(fnenv, code)
          }
        else
          Listok.eval(fnenv, code)

      }
    }
  }

  def makeCall (name: Symbol, env: Env, ll: List[Symbol], body: List[Lcommon], args: List[Lcommon]) =
    if (Nil == body)
      Lnil
    else {
      if (ll.length != args.length)
        throw SyntaxError("Invalid number of arguments: " + args.length, env)
      val fnenv = Env(name, env, ll, args)
      Listok.eval(fnenv, body)
    }


  private def makeLambdaList(env: Env, lambda_list: List[Lcommon]) = {
    var pos = 0
    val it = lambda_list.iterator
    val lb = List.newBuilder[Symbol]
    while (it.hasNext) {
      it.next match {
        case Lsymbol(Symbol("&rest")) =>
          if (pos != lambda_list.length - 2)
            throw SyntaxError(" misplaced &REST in lambda list: " + Util.pp(lambda_list), env)
        case Lsymbol(sym) => lb += sym
        case err => throw new SyntaxError("Required argument is not a symbol: " + err.pp, env)
      }
      pos += 1
    }
    lb.result
  }

  private def extractDeclare(l: List[Lcommon]) = l match {
    case Nil => (Nil, Nil)
    case _ => l.head match {
      case Llist(xs) =>
        xs.head match {
          case Lsymbol('declare) => (l.tail, xs.tail)
          case _ => (l, Nil)
        }
      case _ => (l, Nil)
    }
  }

   def makeBody(name: Symbol, l: List[Lcommon]): (List[Lcommon], Boolean) = extractDeclare(l) match {
    case (body, Nil) => (body, false)
    case (body, decl) =>
      if (decl.exists {
        case Lkeyword('tco) => true
        case _ => false
      }) {
        (body, true)
      }
      else {
        println("warning : unknown declaration " + Util.pp(decl)) // todo: host.onwarning
        (body, false)
      }
    }

  /*
  def resolveSymbols(env: Env, l: List[Lcommon]): List[Lcommon] =
    Util.mapTree(l) {
      // case Llambda(ll, body, name) => Llambda(ll, resolveSymbols(env, body), name)
      case ls: Lsymbol =>
        env.lookupOption(ls.sym) match {
          case None => ls  // warning about ????
          case Some(v) => Lsymbolglobal(ls.sym, v)
        }
      case x => x
    }
 */

}


trait TCOFunction { self: Lfunction =>

  def inRecurse (env: Env): Boolean =
    if (name == env.name)
      true
    else if (null != env.getCaller)
      inRecurse(env.getCaller)
    else
      false

  override def lapply(env: Env, args: List[Lcommon]): Lcommon = {
    if (inRecurse(env)) {
      if (env.allowTCO) {
        //log(" in recurse and tco is allowed " + pp)
        Ltcostub(this, args)
      }
      else
        //log(" in recurse but tco is deny " + pp)
        fn(env, args)
     }
    else {
      //log(" begin tco.call " + pp)
      var ta = args
      var ok = true
      var r: Lcommon = Lnil
      while (ok) {
        r = fn(env, ta)
        r match {
          case t: Ltcostub =>
            //log(" recursive tco.call : " + t.fn.pp + " inside " + pp)
            if (t.fn != this) {
              return t      // not my stub - pass outside
            }
            else {
              ta = t.args // my tco call
            }
          case _ => ok = false
        }
      }
      //log(" finish tco.call " + pp + " result=" + r.pp)
      r
    }
  }

}
