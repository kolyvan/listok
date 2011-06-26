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
import scala.collection.mutable.ArraySeq

object Sequences extends Helpers {


  def func_list (env: Env, args: List[Lcommon]): Lcommon = args match {
    case Nil => Lnil
    case _ => Llist(args)
  }

  def func_vector (env: Env, args: List[Lcommon]): Lcommon = args match {
    case Nil => Lvector(ArraySeq.empty)
    case _ => Lvector(ArraySeq(args:_*))
  }

  def func_string (env: Env, args: List[Lcommon]): Lcommon = args match {
    case Nil => Lstring("")
    case _ => Lstring("").make(args)
  }

   def func_range (env: Env, args: List[Lcommon]) = {
    mustEqual(env, args, 2)
    val start = args(0).getInt(env)
    val end = args(1).getInt(env)
    Llist(List.range(start, end).map(Lint(_)))
  }

  def func_length(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    args.head match {
      case Lnil => Lint(0)
      case s: Lseq => Lint(s.length)
      case Lblob(b) => Lint(b.length)
      case err => throw TypeError("The value " + err + " is not sequence", env)
    }
  }

  def func_head(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    args.head match {
      case Lnil => Lnil
      case s: Lseq if s.isEmpty => Lnil
      case s: Lseq => s.seq.head
      case Lpair(a, _) => a
      case err => throw TypeError("The value " + err + " is not sequence", env)
    }
  }

  def func_second(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    args.head match {
      case Lnil => Lnil
      case s: Lseq if s.length < 2 => Lnil
      case s: Lseq => s.seq.tail.head
      case Lpair(_, b) => b
      case err => throw TypeError("The value " + err + " is not sequence", env)
    }
  }

  def func_last(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    args.head match {
      case Lnil => Lnil
      case s: Lseq if s.length == 0 => Lnil
      case s: Lseq => s.seq.last
      case err => throw TypeError("The value " + err + " is not sequence", env)
    }
  }

  def func_tail(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    args.head match {
      case Lnil => Lnil
      case s: Lseq => s.tail
      case err => throw TypeError("The value " + err + " is not sequence", env)
    }
  }

  def func_cons(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    args.tail.head match {
      case Lnil => LL(args.head) //Llist(Nil).cons(args.head)
      case l:Lseq => l.cons(args.head)
      case err => throw SyntaxError("Invalid argument: " + err.pp, env)
    }
  }

  def func_append(env: Env, args: List[Lcommon]): Lcommon = {
    notLess(env, args, 2)
    var p = args.tail
    var r = args.head
    while (p != Nil) {
      r = (r, p.head) match {
        case (Lnil, Lnil) => Lnil
        case (Lnil, r:Lseq) => r
        case (Lnil, r) => LL(r)
        case (l:Lseq, Lnil) => l
        case (l:Lseq, r:Lseq) => l.make(l.seq ++ r.seq)
        case (l:Lseq, r) => l.make(l.seq :+ r)
        case (err, _) => throw SyntaxError("Invalid argument: " + err.pp, env)
      }
      p = p.tail
    }
    r
  }

  def func_reverse(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    args.head match {
      case Lnil => Lnil
      case s: Lseq => s.make(s.seq.reverse)
      case err => throw TypeError("The value " + err + " is not sequence", env)
    }
  }

  def func_elt(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    val index = args(1).castInt(env).int
    args.head match {
      case Lnil => throw SyntaxError("The index " + index + " is too large.", env)
      case s: Lseq =>
        if (index >= s.length)
          throw SyntaxError("The index " + index + " is too large.", env)
        s.seq(index)
      case Lblob(a) =>
        if (index >= a.length)
          throw SyntaxError("The index " + index + " is too large.", env)
        Lbyte(a(index))
      case err => throw TypeError("The value " + err + " is not sequence", env)
    }
  }

  def func_setelt(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 3)
    val index = args(1).castInt(env).int
    val value = args(2)
    args.head match {
      case Lnil => throw SyntaxError("The index " + index + " is too large.", env)

      case Lvector(a) =>
        if (index >= a.length)
          throw SyntaxError("The index " + index + " is too large.", env)
        a(index) = value

      case Lblob(a) =>
        if (index >= a.length)
          throw SyntaxError("The index " + index + " is too large.", env)
        a(index) = Blob.toByte(env, value)

      case err => throw TypeError("The value " + err + " is not of type VECTOR", env)
    }
    value
  }

  def func_subseq(env: Env, args: List[Lcommon]): Lcommon = {
    if (args.length != 2 && args.length != 3)
      throw SyntaxError("Invalid number of argument: " + args.length, env)

    val lseq = args.head.castSeq(env)
    val len = lseq.length
    val from = args(1).castInt(env).int
    val until = if (args.length == 3)
      args(2).castInt(env).int
    else
      lseq.length

    if (until == from) // && until > 0 && until <= len)
      Lnil
    else {
      if (until < from || until < 0 || from < 0 || from >= len)
        throw SyntaxError("The bounding indices "+from+" and "+until+" are bad for a sequence of length " + len, env)
      lseq.make(lseq.seq.slice(from, until))
    }
  }

  def func_find(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    args(1).castSeq(env).seq.find(_ == args.head) match {
      case Some(x) => x
      case None => Lnil
    }
  }

  def func_find_if(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    val fn = args.head.castFunction(env)
    args(1).castSeq(env).seq.find(
      x => Util.isTrue(fn.lapply(env, List(x)))) match {
        case Some(x) => x
        case None => Lnil
      }
  }

  def func_position(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    args(1).castSeq(env).seq.indexOf(args.head) match {
      case -1 => Lnil
      case n => Lint(n)
    }
  }

  def func_position_if(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    val fn = args.head.castFunction(env)
    // args(1).castSeq(env).seq.findIndexOf(
    args(1).castSeq(env).seq.indexWhere(
        x => Util.isTrue(fn.lapply(env, List(x)))) match {
        case -1 => Lnil
        case n => Lint(n)
      }
   }

  def func_count(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    Lint(args(1).castSeq(env).seq.count(args.head == _))
  }

  def func_count_if(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    val fn = args.head.castFunction(env)
    Lint(args(1).castSeq(env).seq.count(
      x => Util.isTrue(fn.lapply(env, List(x)))))
  }

  def func_remove(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    val s = args(1).castSeq(env)
    if (s.isEmpty)
      Lnil
    else
      s.make(s.seq.filterNot(_ == args.head))
  }

  def func_remove_if(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    val fn = args.head.castFunction(env)
    val s = args(1).castSeq(env)
    if (s.isEmpty)
      Lnil
    else
      s.make(s.seq.filterNot(
        x => Util.isTrue(fn.lapply(env, List(x)))))
  }

  def func_sort(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    val fn = args(1).castFunction(env)
    val s = args(0).castSeq(env)
    if (s.isEmpty)
      Lnil
    else
      s.make(s.seq.sortWith(
        (a,b) => Util.isTrue(fn.lapply(env, List(a,b)))))
  }

  def func_take(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    val n = args(1).castInt(env).int
    val s = args(0).castSeq(env)
    if (s.isEmpty)
      Lnil
    else
      s.make(s.seq.take(n))
  }

  def func_drop(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    val n = args(1).castInt(env).int
    val s = args(0).castSeq(env)
    if (s.isEmpty)
      Lnil
    else
      s.make(s.seq.drop(n))
   }


   // (foreach f l)
  def func_foreach(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    val fn = args(0).castFunction(env)
    val s = args(1).castSeq(env)
    if (!s.isEmpty)
      s.seq.foreach{ p => fn.lapply(env, List(p))}
    Lnil
  }

   // (reduce f l)
  def func_reduce(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    val fn = args(0).castFunction(env)
    val s = args(1).castSeq(env)
    if (s.isEmpty)
      Lnil
    else
      s.seq.reduceLeft{ (r,p) => fn.lapply(env, List(r, p))}
  }

   // (fold f init l)
  def func_fold(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 3)
    val fn = args(0).castFunction(env)
    val s = args(2).castSeq(env)
    if (s.isEmpty)
      args(1)
    else
      s.seq.foldLeft(args(1)){ (r,p) => fn.lapply(env, List(r, p)) }
  }

   // (map f l)
  def func_map(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    val fn = args(0).castFunction(env)
    val s = args(1).castSeq(env)
    if (s.isEmpty)
      Lnil
    else
      s.make(s.seq.map{ p => fn.lapply(env, List(p))})
  }

   // (flatmap f l)
  def func_flatmap(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    val fn = args(0).castFunction(env)
    val s = args(1).castSeq(env)
    if (s.isEmpty)
      Lnil
    else
      s.make(s.seq.flatMap{ p =>
        fn.lapply(env, List(p)) match {
          case Lnil => Nil
          case s: Lseq => s.seq
          case x => List(x)
        }
      })
  }

  def func_filter(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    val fn = args(0).castFunction(env)
    val s = args(1).castSeq(env)
    if (s.isEmpty)
      Lnil
    else
      s.make(s.seq.filter{
        p => Util.isTrue(fn.lapply(env, List(p))) })
  }

  def func_partition(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    val fn = args(0).castFunction(env)
    val s = args(1).castSeq(env)
    if (s.isEmpty)
      Lpair(Lnil, Lnil)
    else {
      val (a,b) = s.seq.partition{
        p => Util.isTrue(fn.lapply(env, List(p))) }
      Lpair(s.make(a),s.make(b))
    }
  }

  def func_search(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    val t = args(0).castSeq(env).seq
    val s = args(1).castSeq(env).seq
    s.indexOfSlice(t) match {
      case -1 => Lnil
      case n => Lint(n)
    }
  }

  // hashmap
  def func_hashmap (env: Env, args: List[Lcommon]): Lcommon = {
    Lhashmap(Map.empty).make(args)
  }

  def func_hm_get(env: Env, args: List[Lcommon]): Lcommon = {
    notLess(env, args, 2)
    val notfound = if (args.length > 2) args(2) else Lnil
    args(0).castHashmap(env).getOrElse(args(1), notfound)
  }

  def func_hm_add(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 3)
    args(0).castHashmap(env).add(args(1), args(2))
  }

  def func_hm_del(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    args(0).castHashmap(env).del(args(1))
  }

  def func_pair(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    Lpair(args(0), args(1))
  }

  /// hash table
  // in fact the hash-table doesn't support the coll interface
  // but it's like collection - so it's place is here

  def func_hash_table (env: Env, args: List[Lcommon]): Lcommon = args match {
    case Nil => Lhashtable(scala.collection.mutable.Map.empty)
    case _ => throw SyntaxError("Invalid number of argument: " + args.length, env)
  }

  def func_gethash(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    args(1).castHashtable(env).map.get(args.head) match {
        case None => Lnil
        case Some(x) => x
      }
  }

  def func_sethash(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 3)
    val ht = args(1).castHashtable(env)
    val v = args(2)
    ht.map += (args.head -> v)
    v
  }

  def func_remhash(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    val ht = args(1).castHashtable(env)
    val k = args.head
    val r = ht.map.contains(k);
    ht.map -= k
    Util.toLbool(r)
  }

  def func_clrhash(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 1)
    val ht = args.head.castHashtable(env)
    ht.map.clear
    ht
  }

  def func_checkhash(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    val ht = args(1).castHashtable(env)
    Util.toLbool(ht.map.contains(args.head))
  }

  def func_maphash(env: Env, args: List[Lcommon]): Lcommon = {
    mustEqual(env, args, 2)
    val ht = args(1).castHashtable(env)
    val fn = args.head.castFunction(env)
    Lhashtable(ht.map.map{ (kv) =>
      fn.lapply(env, List(kv._1,kv._2)) match {
        case s: Lseq if s.length == 2 => (s.seq.head, s.seq.tail.head)
        case x => (kv._1, x)
      }
    })
  }

   /// lazy seq
  def func_lazyseq (env: Env, args: List[Lcommon]) = {
    if (args.isEmpty)
      Llazyseq(Stream.empty)
    else
      LazySeq.seq(env, args)
  }

  def func_lazyseq_from (env: Env, args: List[Lcommon]) = {
    mustEqual(env, args, 1)
    val from = args.head.getInt(env)
    Llazyseq(LazySeq.from(from))
  }

  def func_lazyseq_range (env: Env, args: List[Lcommon]) = {
    mustEqual(env, args, 2)
    val start = args(0).getInt(env)
    val end = args(1).getInt(env)
    Llazyseq(LazySeq.range(start, end))
  }

  def func_lazyseq_force (env: Env, args: List[Lcommon]) = {
    mustEqual(env, args, 1)
    args.head match {
      case l: Llazyseq => Llazyseq(l.seq.force)
      case err => throw throw TypeError("The value " + err + " is not LAZYSEQ", env)
    }
  }

  ///

  val all = List (
    Lfunction(func_list, 'list),
    Lfunction(func_vector, 'vector),
    Lfunction(func_string, 'string),
    Lfunction(func_range, 'range),
    Lfunction(func_length, 'length),
    Lfunction(func_head, 'head),
    Lfunction(func_head, 'first),
   // Lfunction(func_head, 'car),
    Lfunction(func_second, 'second),
    Lfunction(func_last, 'last),
    Lfunction(func_tail, 'tail),
    Lfunction(func_tail, 'rest),
  // Lfunction(func_tail, 'cdr),
    Lfunction(func_cons, 'cons),
    Lfunction(func_append, 'append),
    Lfunction(func_reverse, 'reverse),
    Lfunction(func_elt, 'elt),
    Lfunction(func_setelt, 'setelt),
    Lfunction(func_setelt, Symbol("set-elt")),
    Lfunction(func_subseq, 'subseq),
    Lfunction(func_find, 'find),
    Lfunction(func_find_if, Symbol("find-if")),
    Lfunction(func_position, 'position),
    Lfunction(func_position_if, Symbol("position-if")),
    Lfunction(func_count, 'count),
    Lfunction(func_count_if, Symbol("count-if")),
    Lfunction(func_remove, 'remove),
    Lfunction(func_remove_if, Symbol("remove-if")),
    Lfunction(func_sort, 'sort),
    Lfunction(func_take, 'take),
    Lfunction(func_drop, 'drop),
    Lfunction(func_foreach, 'foreach),
    Lfunction(func_reduce, 'reduce),
    Lfunction(func_fold, 'fold),
    Lfunction(func_map, 'map),
    Lfunction(func_flatmap, 'flatmap),
    Lfunction(func_filter, 'filter),
    Lfunction(func_partition, 'partition),
    Lfunction(func_search, 'search),

    // hashmap
    Lfunction(func_hashmap, 'hashmap),
    Lfunction(func_hm_get, Symbol("hm-get")),
    Lfunction(func_hm_add, Symbol("hm-add")),
    Lfunction(func_hm_del, Symbol("hm-del")),
    Lfunction(func_pair, 'pair),

  // hash table
    Lfunction(func_hash_table, Symbol("hash-table")),
    Lfunction(func_gethash, 'gethash),
    Lfunction(func_sethash, 'sethash),
    Lfunction(func_remhash, 'remhash),
    Lfunction(func_clrhash, 'clrhash),
    Lfunction(func_checkhash, 'checkhash),
    Lfunction(func_maphash, 'maphash),

    // lazy
    Lfunction(func_lazyseq, 'lazyseq),
    Lfunction(func_lazyseq_from, Symbol("lazyseq-from")),
    Lfunction(func_lazyseq_range, Symbol("lazyseq-range")),
    Lfunction(func_lazyseq_force, Symbol("lazyseq-force"))
  )

}
