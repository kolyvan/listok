package ru.listok.test

import _root_.ru.listok._


trait Helper {
  def listok = new Listok { override val debug = true }
  def peval(l: Listok, s: String) = {
    try {
      val r = l.eval(s)
      println(r)
      r
    } catch {
      case e: ListokRuntimeError =>
        println(e)
        Util.printBacktrace(e.env)
      case e: RuntimeException => println(e)
    }
  }
  def eval(l: Listok, s: String) = {
    try { l.eval(s) }
    catch {
      case e: ListokRuntimeError =>
        println(e)
        Util.printBacktrace(e.env)
      case e: RuntimeException => println(e)
    }
  }
}