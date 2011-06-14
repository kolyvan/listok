package ru.listok.test

import _root_.ru.listok._
import org.scalatest.FunSuite


class MacroTest extends FunSuite {

  def listok = new Listok

  test("defmacro") {

    expect(Lint(11)){listok.eval(
      """
      (defmacro test (a) `(+ 10 ,a))
      (test 1)
      """
    )}

    expect(Lint(21)){listok.eval(
    """
    (defmacro test (a b) `(setf ,a (+ 10 ,b)))
    (def x 0)
    (test x 11)
    """)}


    expect(LL(Lint(2), Lint(1))){listok.eval(
    //expect(Lint(2)){listok.eval(
    """
    (defmacro swap (a b)
     `(let ((temp ,a))
         (setf ,a ,b)
         (setf ,b temp)))
    (def x 1)
    (def y 2)
    (swap x y)
    (list x y)
    """)}

  }


  test("macro") {
     expect(Lint(100)){listok.eval("""
     (defmacro sqone (x)
      `(let ((y (+ ,x 1))) (* y y)))
     (sqone 9)
     """)}

    expect(LL(Lint(1))){listok.eval("""
      (defmacro reverse-cons (rest first)
          `(cons ,first ,rest))
      (reverse-cons nil 1)
    """)}

  }

  test("macro nested") {
   expect(LL(Lint(100), Lint(9))){listok.eval("""
     (defmacro reverse-cons (rest first)
      `(cons ,first ,rest))
     (defmacro sqone (x)
      `(let ((y (+ ,x 1)))
         (reverse-cons (list ,x) (* y y))))
     (def z (sqone 9))
     z
    """)}
  }

  test("comma and backquote") {
    expect(LL(Lint(1), Lkeyword('ABC))){listok.eval("(def a :ABC) `(1 ,a)")}
    expect(LL(Lint(1), LL(Ltrue, Lnil))){listok.eval("(def a '(t nil)) `(1 ,a)")}
    expect(LL(Lint(1), Ltrue, Lnil)){listok.eval("(def a '(t nil)) `(1 ,@a)")}
    expect(LL(Lint(1), Ltrue, Lnil)){listok.eval("`(1 ,@(list t nil))")}
  }

  test("when,unless") {
    expect(Lnil){ listok.eval("(when t)") }
    expect(Lnil){ listok.eval("(when nil)") }
    expect(Lnil){ listok.eval("(when nil :ok)") }
    expect(Lkeyword('ok)){ listok.eval("(when t :ok)") }
    expect(Lnil){ listok.eval("(unless t)") }
    expect(Lnil){ listok.eval("(unless nil)") }
    expect(Lnil){ listok.eval("(unless t :ok)") }
    expect(Lkeyword('ok)){ listok.eval("(unless nil :ok)") }
  }

   test("loop") {
    intercept[ScriptExit]{ listok.eval("(loop (exit))") }
   // intercept[ScriptExit]{ listok.eval(
   //   "(def n 0)(loop (setf n (incr n)) (when (= n 5) (exit)))") }
  }

  test("more macro") {
    expect(Lint(3)){listok.eval(
    """
    (defmacro repeat (times &rest body)
      `(dotimes (x ,times)
        ,@body))
    (def n 0)
    (repeat 3 (setf n (incr n)))
    n
    """)}

  }

  test("doseq") {
    expect(LL(Lint(3),Lint(2),Lint(1))){listok.eval(
      "(def r nil)(doseq (p '(1 2 3)) (setf r (cons p r))) r")}
  }

  test("gensym") {
    expect(Ltrue){listok.eval("(symbolp (gensym))")}
  }

  test("macroexpand") {
    //expect(Lkeyword('ok)){listok.eval("(eval '(if t :ok) )")}
    expect(Lkeyword('ok)){listok.eval("(eval (macroexpand '(when t :ok)))")}
  }

}
