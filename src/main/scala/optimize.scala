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

import collection.mutable.ArrayBuffer

// constant folding optimization
// warning: experimental feature and is badly tested

object Optimize extends Host {

  def optimized (p: Lcommon):Boolean = p match {
    case Lnil => true
    case Ltrue => true
    case _: Lint => true
    case _: Lfloat => true
    case _: Lchar => true
    case _: Lstring => true
    case _: Lkeyword => true
    case _: Lregex => true
    case Lquote(q) => optimized(q)
    case Lpair(a,b) => optimized(a) && optimized(b)
    case _: Llazyseq => false
    case l: Lseq => l.seq.forall(optimized)

    case _ => false
  }


  def run(forms: List[Lcommon]): List[Lcommon] = {

    val env = root

    def atom(p: Lcommon): Lcommon = p match {

      // case Llist(Nil) => Llist(Nil) //Lnil

      case l:Llist =>

        if (mustSkip(l.seq))
          l
        else {

          val xs = l.seq.map {atom _}

          val l2 = Llist(xs)

          if (xs.tail.forall(optimized) ) {

            (try { Some(Listok.lapply(env, xs.head, xs.tail)) }
            catch {
              case ex =>
              //  log("exception " + ex.toString + " during optimizing " + l2.pp)
                None
            }) match {
              case Some(r) =>
                if (optimized(r)) {
                //  log("optimize " + l2.pp + " to " + r.pp)
                  if (r.isInstanceOf[Llist])
                    return Lquote(r)
                  else
                    return r
                }
                //else log(" unable opt for " + l2.pp)
              case _ =>
            }
          } // else log(" skip opt for " + l2.pp)

          l2
        }

      case Llambda(ll, body, name) => Llambda(ll, list(body), name)

      case x => x
    }

    def list(l: List[Lcommon]): List[Lcommon] = l map { atom _ }

    val r = list(forms)

   // log("optimize result - forms before: " + count(forms) + " after: " + count(r))
   // log("before\n" + Util.pp(forms, "", "\n", ""))
   // log("after\n"  + Util.pp(r, "", "\n", ""))
   // log("")

    r
  }

  def mustSkip(xs: List[Lcommon]) = {
    if (xs.isEmpty)
      true
    else {
      xs.head match {
        case sf: Lsform if (sf.name == 'match || sf.name == 'assert || sf.name == 'spawn) => true
        case _ => false
      }
    }
  }

  def root = {
     val b = ArrayBuffer.newBuilder[EnvEntry]

    builtin.Common.all.foreach      { x => b += EnvEntry(x.name, x, true) }
    builtin.Numbers.all.foreach     { x => b += EnvEntry(x.name, x, true) }
    builtin.Sequences.all.foreach   { x => b += EnvEntry(x.name, x, true) }
    builtin.Regex.all.foreach       { x => b += EnvEntry(x.name, x, true) }

     new Env('root, null, b.result, this, new FakeMailslot) {
       override protected def defineimpl(symbol: Symbol, value: Lcommon, mutable: Boolean) =
           throw NotAllowed("deny def")
     }
   }

  case class NotAllowed(msg: String) extends RuntimeException(msg)

  class FakeMailslot extends Mailslot('fake) {
    override def receive(timeout: Long) = throw NotAllowed("deny maislot")
    override def send(msg: Lcommon, from: Lmailslot)  = throw NotAllowed("deny maislot")
  }

  def count(l: List[Lcommon]): Int = {
    var n = 0
    Util.traverseTree(l) {
      case Llambda(ll, body, name) => n += count(body)
      case _ => n += 1
    }
    n
  }
}
