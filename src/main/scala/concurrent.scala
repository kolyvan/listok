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

import builtin.Streams
import scala.collection.mutable.{Queue => MQueue}
import java.lang.{Thread}



class Mailslot(val name: Symbol) {

  type Message = Pair[Lcommon, Lmailslot]

  protected var messages = new MQueue[Message]

  def receive(timeout: Long): Option[Message] = synchronized {

    def waitTime(timeout: Long, endtime: Long) =
      if (timeout > 0)
        (endtime - System.currentTimeMillis).max(0)
      else
        timeout

    var stillwait = timeout
    val endtime = timeout + System.currentTimeMillis

    while (true) {
      if (messages.nonEmpty)
        return Some(messages.dequeue)

      stillwait = waitTime(stillwait, endtime)
      if (!waitMsg(stillwait)) // false is timeout
        return None
    }
    None
  }

  def send(msg: Lcommon, from: Lmailslot) = synchronized {
    messages += (msg -> from)
    msg
    notifyAll()
  }

  def size = synchronized { messages.length }

  protected def waitMsg(timeout: Long):Boolean = synchronized {

    if (timeout == 0) {
      wait(0)
    }
    else if (timeout < 0) {
      while (messages.isEmpty)
        wait()
    }
    else {
      var now = System.currentTimeMillis
      val end = now + timeout
      while (messages.isEmpty) {
        wait(end - now)
        now = System.currentTimeMillis
        if (messages.isEmpty && now >= end) {
          return false   // timeout
        }
      }
    }

    messages.nonEmpty
  }

}



object Concurrent {

  def getMailslot(env: Env) = env.getMailslot match {
    case null => throw new RuntimeException("bugcheck, lack of mailslot")
    case x => x
    }


  def form_spawn(env: Env, l: List[Lcommon]): Lcommon = {
    if (l.length < 3)
      throw SyntaxError("Invalid number of argument: " + l.length, env)
    val name = l.head.getSymbol(env)
    val vars = l(1).castList(env).seq
    Threads.spawn(name, env, vars, l.tail.tail)
  }

  def func_join(env: Env, l: List[Lcommon]): Lcommon = {
    if (l.length < 1)
      throw SyntaxError("Invalid number of argument: " + l.length, env)

    val timeout = if (l.length == 2) l(1).getInt(env) else -1

    l.head match {
      case th: Lthread => Threads.join(th, timeout)
      case err => throw TypeError("The value "+ err +" is not of type COROUTINE or THREAD", env)
    }
  }

  def func_thread_wait_init(env: Env, l: List[Lcommon]): Lcommon = {
    if (l.length < 1)
      throw SyntaxError("Invalid number of argument: " + l.length, env)

    val timeout = if (l.length == 2) l(1).getInt(env) else -1

    l.head match {
      case th: Lthread => th.thread.waitInit(timeout); Lnil
      case err => throw TypeError("The value "+ err +" is not of type THREAD", env)
    }
  }

  def func_mailslot(env: Env, l: List[Lcommon]): Lcommon = {
    if (l.length == 1) {
      l.head match {
        case m: Lmailslot => m
        case th: Lthread => Lmailslot(th.thread.getMailslot) //Lmailslot(th.getMailslot)
        case Lkeyword('parent) => Lmailslot(env.parent.getMailslot)
        case Lkeyword('self) => Lmailslot(env.getMailslot)
        case err => throw TypeError("The value "+ err +" is not of type THREAD or COROUTINE", env)
      }
    } else {
      Lmailslot(getMailslot(env))
    }
  }

  def func_receive(env: Env, l: List[Lcommon]): Lcommon = {
    val timeout = if (l.length == 2) l.head.getInt(env) else -1
    getMailslot(env).receive(timeout) match {
      case None => Lnil
      case Some(m) => Lpair(m._1, m._2)
    }
  }

  def func_send(env: Env, l: List[Lcommon]): Lcommon = {
    if (l.length < 2)
      throw SyntaxError("Invalid number of argument: " + l.length, env)

    val to = l.head match {
      case Lmailslot(m) => m
      case th: Lthread => th.thread.getMailslot
      //case fb: Lfiber => fb.fiber.getMailslot
      case Lkeyword('parent) => env.parent.getMailslot
      case Lkeyword('self) => env.getMailslot
      case err => throw TypeError("The value "+ err +" is not of type MAILSLOT", env)
    }

    val msg = l.tail.head
    to.send(msg, Lmailslot(getMailslot(env)))
    msg
  }

  def func_sleep(env: Env, l: List[Lcommon]): Lcommon = {
    if (l.length < 1)
      throw SyntaxError("Invalid number of argument: " + l.length, env)
    val ms = l.head.getInt(env)
    Thread.sleep(ms)
     Lnil
  }

  //def func_green_thread_mode(env: Env, l: List[Lcommon]): Lcommon = {
  //  GreenThreads.start(env)
  //  Lnil
  //}

  // process

  def func_process(env: Env, l: List[Lcommon]): Lcommon = {
    if (l.length < 1)
      throw SyntaxError("Invalid number of argument: " + l.length, env)

    val cmd = l map  { _.getString(env) }
    try {
      Lprocess(cmd.head, Runtime.getRuntime.exec(cmd.toArray, null))
    } catch {
      case ex: java.io.IOException =>
        env.host.onwarning(env, "fail exec " + cmd + " " + ex.getMessage)
        Lnil
    }
  }

  def func_process_exit_value(env: Env, l: List[Lcommon]): Lcommon = {
    if (l.length < 1)
      throw SyntaxError("Invalid number of argument: " + l.length, env)
    val p = l.head.castProcess(env)
    try { Lint(p.process.exitValue()) } catch {
    case ex: IllegalThreadStateException => Lnil }
  }

  def func_process_wait(env: Env, l: List[Lcommon]): Lcommon = {
    if (l.length < 1)
      throw SyntaxError("Invalid number of argument: " + l.length, env)
    val p = l.head.castProcess(env)
    Lint(p.process.waitFor())
  }

  def func_process_output_stream(env: Env, l: List[Lcommon]): Lcommon = {
    if (l.length < 1)
      throw SyntaxError("Invalid number of argument: " + l.length, env)
    val p = l.head.castProcess(env)
    val out = p.process.getOutputStream
    Lstream(null, new java.io.PrintStream(out), null, out)
  }

  def func_process_input_stream(env: Env, l: List[Lcommon]): Lcommon = {
    if (l.length < 1)
      throw SyntaxError("Invalid number of argument: " + l.length, env)
    val p = l.head.castProcess(env)
    val in = p.process.getInputStream
    Lstream(Streams.bufferedReader(in), null, in, null)
  }

  def func_process_error_stream(env: Env, l: List[Lcommon]): Lcommon = {
    if (l.length < 1)
      throw SyntaxError("Invalid number of argument: " + l.length, env)
    val p = l.head.castProcess(env)
    val in = p.process.getErrorStream
    Lstream(Streams.bufferedReader(in), null, in, null)
  }
  ///

  val all = List (
    Lfunction(func_join, 'join),
    Lfunction(func_thread_wait_init, Symbol("thread-wait-init")),
    Lfunction(func_sleep, 'sleep),
    Lfunction(func_mailslot, 'mailslot),
    Lfunction(func_receive, 'receive),
    Lfunction(func_send, 'send),
   // Lfunction(func_green_thread_mode, Symbol("green-thread-mode"))

    //
    Lfunction(func_process, 'process),
    Lfunction(func_process_exit_value, Symbol("process-exit-value")),
    Lfunction(func_process_wait, Symbol("process-wait")),
    Lfunction(func_process_output_stream, Symbol("process-output-stream")),
    Lfunction(func_process_input_stream, Symbol("process-input-stream")),
    Lfunction(func_process_error_stream, Symbol("process-error-stream"))

  )

}


object Threads {

  def spawn(name: Symbol, env: Env, vars: List[Lcommon], forms: List[Lcommon]) = {

    val th = Lthread(new ThreadWrapper(name, env, vars, forms))
    th.thread.start
    th
  }

  def join(t: Lthread, timeout: Int):Lcommon = {
    if (timeout < 0)
      t.thread.join()
    else
      t.thread.join(timeout)
    t.thread.getResult
  }
}

final class ThreadWrapper(val name: Symbol, env: Env, vars: List[Lcommon], forms: List[Lcommon]) extends Thread(Util.pp(name)) {

  private class Sem {
    private var signal_ = false

    def waitSignal(timeout: Long) = synchronized {
      if (!signal_)  {
        if (timeout < 0)
          wait()
        else
          wait(timeout)
      }
    }

    def signal() = synchronized {
      signal_ = true
      notify()
    }
  }

  private var tenv: Env = null
  private var result: Lcommon = Lnil

  val root = {
    val ll = List.newBuilder[Symbol]
    val args = List.newBuilder[Lcommon] // get args

    ll.sizeHint(vars.length)
    args.sizeHint(vars.length)

    vars.foreach {
      case Llist(xs) if (xs.length == 1) =>
        val s = xs.head.castSymbol(env)
        ll += s.sym
        args += s.eval(env)

      case Llist(xs) if (xs.length == 2) =>
        val s = xs.head.castSymbol(env)
        ll += s.sym
        args += xs.tail.head.eval(env)

      case s: Lsymbol =>
        ll += s.sym
        args += s.eval(env)

      case err => throw TypeError("Mailformed thread vars : "+ err.pp, env)
    }

    Env('thread, env.getGlobal(), ll.result, args.result, new Mailslot(name))
  }

  private val sem = new Sem

  override def run {
    tenv = Env(name, root)
    sem.signal    // ok, init is complete
    result = Listok.eval(tenv, forms)
  }

  override def start {
    super.start
  }

  def waitInit(timeout: Long) = sem.waitSignal(timeout)

  def getMailslot = tenv.getMailslot // race condition here, must call waitInit before
  def getResult = result
}



