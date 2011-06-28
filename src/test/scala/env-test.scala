package ru.listok.test

import _root_.ru.listok._
import org.scalatest.FunSuite


class EnvTest extends FunSuite with Helper {

//  def listok = new Listok

  test("def") {
    expect(Lint(42)){listok.eval("(def a 42) a")}
    expect(Lint(43)){listok.eval(
      """
      (def a 42)
      ((lambda () (setf a (+ a 1))))
      a
      """)}
    expect(Lint(42)){listok.eval(
      """
      (def a 42)
      ((lambda (a) (setf a (+ a 1))) 1)
      a
      """)}
  }

  test("constants") {
    expect(Lint(42)){listok.eval("(defconstant a 42) a")}
    intercept[SyntaxError]{listok.eval(
      """
      (defconstant a 42)
      (setf a (+ a 1))
      """)}
  }

  test("redefine") {
    intercept[SyntaxError]{listok.eval("(def a 1)(def a 2)")}
    intercept[SyntaxError]{listok.eval("(def a 1)(defconstant a 2)")}
    intercept[SyntaxError]{listok.eval("(defun fn ())(def fn 2)")}
    intercept[SyntaxError]{listok.eval("(defmacro m ())(def m 2)")}
    expect(Lkeyword('inlet)){listok.eval("(def a 1)(let ((a :inlet)) a)")}
    expect(Lint(2)){listok.eval("(def a 1)(progn (setf a 2) a)")}
    expect(Lint(2)){listok.eval("(def a 1)(progn (def a 2) a)")}
    //expect(Lint(1)){listok.eval("(def a 1)(progn (def a 2) a)")}
  }

  test("test scope") {
    expect(LL(Lint(22), Lint(11), Lint(3), Lint(2))){listok.eval(
      """
      (def a 1)
      (setf a (+ a 1))
      (def r (list a))
      (progn
        (setf r (cons (+ 1 a) r))
        ;(def a 11)
        (let ((a 11))
          (setf r (cons a r))
          (let ((a 22))
            (setf r (cons a r))
        )))
      r
      """)}
  }

  test("env and nested vars") {
   expect(LL(Lint(1),Lint(2),Lint(3),Lint(4),Lint(5),Lint(6),Lint(7),
     Lint(8),Lint(9),Lint(10),Lint(11),Lint(12))){
     listok.eval(
     """
     (def g1 1)
     (def g2 2)
     (def l1 3)
     (def l2 4)
     (let ((b1 5) (b2 6))
        (def ll1 7)
        (def ll2 8)
        ((lambda (x y)
          (def ll3 11)
          (def g3 12)
          (list g1 g2 l1 l2 b1 b2 ll1 ll2 x y ll3 g3))
        9 10))
      """)}
  }

  test("nested func") {
    expect(Lint(18)){listok.eval(
    """
    (defun f1 (x1 x2 x3 x4)
      (defun f2 (x1 x2) (+ x1 x2 x3 x4))
      (f2 5 6))
    (f1 1 2 3 4)
    """)}

    expect(LL(Lint(7),Lint(2))){listok.eval(
      """
      (defun foo (n)
        (defun bar () n)
        (progn (bar)))
      (list (foo 7) (foo 2))
      """)}
  }

  test("func and let") {
    expect(LL(Lint(2), Lint(1))){listok.eval(
    """
     (defun swap (a b)
      (let ((temp a))
         (setf a b)
         (setf b temp)
         (list a b)))
    (swap 1 2)
    """)}
  }

  test("recurse") {

    expect(Lint(55)){listok.eval("""
    (defun fn (x sum)
      ;(declare :tco)
      ;(write-line (string "fn " x " " sum))
      (match x
        ((numberp n)
          ;(write-line (string " x=" x " n=" n " sum=" sum)) (read-line)
          (fn (if (< n 10) (+ n 1) :done) (+ n sum)))
        (:done
          ;(write-line "work is done")
          sum)
      ))
    (fn 1 0)
    """)}

    expect(Lint(10)){listok.eval(
    """
     (defun fn (a)
        (if (= a 10)
          a
          (progn (fn (+ a 1)))))
    (fn 1)
    """)}
  }

  test("load with prefix") {

    val m =
    """
    (def a :ma)
    (defun f1 () :mf1)
    (defun f2 (x) (list x :mf2 (f1) a))
    (f2 nil)
    """

    def makelistok = new Listok  {
       override def onload(env: Env, source: String) = {
        Listok.load(env, new java.io.ByteArrayInputStream(m.getBytes))
       }
    }

    val l = makelistok

    expect(LL(Lnil, Lkeyword('mf2), Lkeyword('mf1), Lkeyword('ma))) {l.eval("(load :unused :m)")}
    expect(Lkeyword('ma)){l.eval("m:a")}
    expect(Lkeyword('mf1)){l.eval("(m:f1)")}
    expect(LL(Ltrue, Lkeyword('mf2), Lkeyword('mf1), Lkeyword('ma))) {l.eval("(m:f2 t)")}

    val l1 = makelistok

    expect(LL(Lnil, Lkeyword('mf2), Lkeyword('mf1), Lkeyword('ma))){l1.eval("(load :unused)")}
    expect(Lkeyword('ma)){l1.eval("a")}
    expect(Lkeyword('mf1)){l1.eval("(f1)")}
    expect(LL(Ltrue, Lkeyword('mf2), Lkeyword('mf1), Lkeyword('ma))) {l1.eval("(f2 t)")}
  }

  test("macro") {
    expect(LL(Lint(2), Lint(1))) {
      listok.eval(
      """
      (defmacro swap (a b)
        `(let ((z ,a))
          (setf ,a ,b)
          (setf ,b z)
        ))
      (def x 1)
      (def y 2)
      (swap x y)
      (list x y)
      """)}
  }

}