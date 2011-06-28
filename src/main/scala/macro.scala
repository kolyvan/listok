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


object Macro {

  def macro_progn (env: Env, l: List[Lcommon]): Lcommon = {
    LL(Llambda(Nil, l, 'progn))
  }

  /*
  def macro_let (env: Env, l: List[Lcommon]): Lcommon = {
     if (l.length < 1)
      throw SyntaxError("invalid number of elements in macro let: " + Util.pp(l), env)

    val ll = List.newBuilder[Lcommon]
    val args = List.newBuilder[Lcommon]

    l.head match {
      case Llist(xs) =>
        ll.sizeHint(xs.length)
        args.sizeHint(xs.length)
        xs.foreach {
          case Llist(xs) if (xs.length  == 1) =>
            ll += xs.head
            args += Lnil

          case Llist(xs) if (xs.length  == 2) =>
            ll += xs.head
            args += xs.tail.head

          case s: Lsymbol =>
            ll += s
            args += Lnil

          case _ => throw SyntaxError(" Malformed LET", env)
        }

      case _ => throw SyntaxError(" Malformed LET", env)
    }

    Llist(Llambda(ll.result, l.tail, 'let) :: args.result)
  }
  */

  def macro_dotimes (env: Env, l: List[Lcommon]): Lcommon = {
    // (dotimes (n 10) l)
    // (do (n 0 (+ n 1) (eq n 10) ...)

    if (l.length < 1)
      throw SyntaxError("invalid number of elements in macro dotimes: " + Util.pp(l), env)

    val (n, num) = l.head match {
      case Llist(xs) if xs.length == 2 => (xs.head, xs.tail.head)
      case _ => throw SyntaxError("Mailformed condition for dotimes", env)
    }

    val ns = n match {
      case s:Lsymbol => s.sanitize
      case _ => throw TypeError("DO step variable is not a symbol: " + n.pp, env)
    }

    val evalednum = num.eval(env) match {
      case x:Lnumeric => Lint(x.int)
      case err => throw TypeError("The value "+err.pp+" is not of type NUMBER", env)
    }

    val code = l.tail
    val sdo = SpecialForms.make('do)
    val seq = Lsymbol('eq)
    val splus = Lsymbol('+)
    val cond = LL(LL(seq, ns, evalednum))
    val step = LL(splus, Lint(1), ns)
    val init = LL(LL(ns, Lint(0), step))

    Llist(sdo :: init :: cond :: code)
  }

  def macro_doseq (env: Env, l: List[Lcommon]): Lcommon = {

    // (doseq (n l) ...)

    // (do ((x l (rest x)))
    //     ((null x))
    //     ((lambda (n) ...) (first x)))

    if (l.length < 1)
      throw SyntaxError("invalid number of elements in macro doseq: " + Util.pp(l), env)

    val (n, seq) = l.head match {
      case Llist(xs) if xs.length == 2 => (xs.head, xs.tail.head)
      case _ => throw SyntaxError("Mailformed condition for doseq", env)
    }

    n match {
      case _:Lsymbol =>
      case _ => throw TypeError("DO step variable is not a symbol: " + n.pp, env)
    }

   // seq match {
   //   case s:Lseq =>
   //   case _ => throw TypeError("The value "+seq.pp+" is not of type SEQUENCE", env)
   // }


    val code = l.tail

    val x = gensym

    val snull = Lsymbol('null)
    val stail = Lsymbol('tail)
    val shead = Lsymbol('head)

    val sdo = Lsform(SpecialForms.form_do _, 'do)
    val body = List(LL(Llambda(List(n), code, 'doseq), LL(shead, x)))
    val step = LL(stail, x)
    val init = LL(LL(x, seq, step))
    val cond = LL(LL(snull, x))

    Llist(sdo :: init :: cond :: body)
  }


  def macro_dowhile (env: Env, l: List[Lcommon]): Lcommon = {
    // (dowhile cond l)
    // ((def f (lambda () (if cond (progn l (f))))))
    // (do () (cond) l)
    if (l.length < 1)
      throw SyntaxError("invalid number of elements in macro dowhile: " + Util.pp(l), env)

    val code = l.tail
    val sdo = Lsform(SpecialForms.form_do _, 'do)
    val snot = Lsymbol('not)
    val cond = LL(LL(snot, l.head))
    val init = Llist(Nil)

    Llist(sdo :: init :: cond :: code)
  }

  def macro_loop (env: Env, l: List[Lcommon]): Lcommon = {
    // (loop ...)
    // (do () (nil) l)
    if (l.length < 1)
      throw SyntaxError("invalid number of elements in macro loop: " + Util.pp(l), env)

    val sdo = SpecialForms.make('do)
    val cond = LL(Lnil)
    val init = Llist(Nil)

    Llist(sdo :: init :: cond :: l)
  }

  def macro_when (env: Env, l: List[Lcommon]): Lcommon = {
      // (when cond ...)
      // (if cond (lambda () ...))
    if (l.length < 1)
      throw SyntaxError("invalid number of elements in macro when: " + Util.pp(l), env)

    val cond = l.head
    val sif = SpecialForms.make('if)
    val lmb = LL(Llambda(Nil, l.tail, 'if))

    LL(sif, cond, lmb)
  }

  def macro_unless (env: Env, l: List[Lcommon]): Lcommon = {
      // (unless cond ...)
      // (if cond nil (lambda () ...))
    if (l.length < 1)
      throw SyntaxError("invalid number of elements in macro unless: " + Util.pp(l), env)

    val cond = l.head
    val sif = SpecialForms.make('if)
    val lmb = LL(Llambda(Nil, l.tail, 'if))

    LL(sif, cond, Lnil, lmb)
  }

  val all = List (
    Lmacro(macro_progn _, 'progn),
//    Lmacro(macro_let _, 'let),
    Lmacro(macro_dotimes _, 'dotimes),
    Lmacro(macro_dowhile _, 'dowhile),
    Lmacro(macro_when _, 'when),
    Lmacro(macro_unless _, 'unless),
    Lmacro(macro_loop _, 'loop),
    Lmacro(macro_doseq _, 'doseq)
  )


  def macroexpand(env: Env, forms: List[Lcommon]): List[Lcommon] = {

    import scala.collection.mutable.{Map => MMap}
    val macros: MMap[Symbol, Lmacro] = MMap(all.map {x => (x.name -> x)} :_*)

    def expandForm(l: Llist):Lcommon = {
      val tail:List[Lcommon] = expandList(l.seq.tail)
      val head = l.seq.head

      head match {
        case Lsymbol(s) =>
          macros.get(s) match {
            case Some(m) =>
              val r = m.fn(env, tail)
              // log("macroexpand2 "+s+" => " + r.pp)

              r match {
                case ll: Llist =>  expandForm(ll)
                case x => x
              }

            case None =>  Llist(head::tail)
          }

        case lmb: Llambda =>
          Llist(expandLambda(lmb) :: tail)

        case l2: Llist if l2.seq.length > 0 =>
          Llist(expandForm(l2) :: tail)

        case _ => Llist(head::tail)
      }
    }

    def expandLambda(lmb: Llambda): Llambda =
        Llambda(lmb.lambda_list, expandList(lmb.body), lmb.name)

    def expandList(l: List[Lcommon]): List[Lcommon] = l.flatMap {
      case m: Ldefmacro =>
          val fn = LambdaCalculus.makeFunc(env, 'macro, m.lambda_list, m.body)
          macros += (m.name -> Lmacro(fn, m.name))
          // log("macrostore " + m.name +" -> "+m.pp)
          None

      //case l: Llist if l.seq.length == 0 => None //?
      case l: Llist if l.seq.length > 0 => Some(expandForm(l))
      case lmb: Llambda => Some(expandLambda(lmb))
      case x => Some(x)
      }

   expandList(forms)
  }


  /*
  def backquote(env: Env, x: Lcommon):Lcommon = {
    x match {
      case Llist(l) =>
        Llist(l.map(backquote(env, _)))
      case c: Lmacrocomma =>
        c.eval(env)
      case _ => x
    }
  }
  */

  def backquote(env: Env, form: Lcommon):Lcommon = {

    def splice(xs: List[Lcommon]):List[Lcommon] = {

      val lb = List.newBuilder[Lcommon]

      for (p <- xs)
        p match {
          case c: Lmacrocomma =>
            if (c.spliced) {
              c.eval(env) match {
                case Llist(l) => lb ++= l //splice list
                case r => lb += r
              }
            }
            else
              lb += c.eval(env)

          case Llist(l) =>
            lb += Llist(splice(l))

          case _ =>
            lb += p
      }

      lb.result
    }

    form match {
      case Llist(xs) => Llist(splice(xs))
      case c: Lmacrocomma if !c.spliced => c.eval(env)
      case c: Lmacrocomma if c.spliced =>
        throw SyntaxError("Macroexpand fail: ,@ after backquote in " + c.pp, env)
      case _ => form
    }
  }

  private var gensymCount = 0
  def gensym = {
    gensymCount += 1
    Lsymbol(Symbol(("#:g" + gensymCount)))
  }
}
