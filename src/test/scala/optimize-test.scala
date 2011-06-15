package ru.listok.test

import _root_.ru.listok._
import org.scalatest.FunSuite


class OptimizeTest extends FunSuite {

  def listok = new Listok

  def optimize(text: String) = {
    val r = Optimize.run(Macro.macroexpand(Optimize.root, Listok.parse(text)))
    if (r.length != 1)
      fail("otpimize returns an invalid result: " + r)
    r.head
  }

  test("numbers") {
    expect(Lint(15)){optimize("(* 5 (+ 1 2))")}
  }

  test("lambda") {
    expect(Lint(33)){optimize("""
      (+ 1
        ((lambda (z) (def x 3) (* x 5 z)) 2)
        2)
    """)}
  }

  test("list") {
    expect(Lquote(LL(Lint(1), Lint(2), Lint(3)))){optimize("(list 1 2 3)")}
    expect(Lquote(LL(Lint(2), Lint(3)))){optimize("(tail (list 1 2 3))")}
    expect(Lquote(LL(Lint(3)))){optimize("(tail (tail (list 1 2 3)))")}
    expect(Lquote(LL(Lint(2),Lint(1)))){optimize("(cons 2 (cons 1 nil))")}
  }

  test("def") {
    expect(List('def, Lsymbol('x), Lint(23))){
      optimize("""
        (def x (progn
          (defun f (x) (+ 1 x))
          (f 22)))
      """) match {
        case Llist(xs) => xs.map {
            case Lsform(_, name) => name
            case x => x
          }
        case err => fail("expected list but got " + err.pp)
      }
    }
  }

}