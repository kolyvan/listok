package ru.listok.test

import _root_.ru.listok._
import org.scalatest.FunSuite


class DoTest extends FunSuite {

  def listok = new Listok


  test("do") {

    expect(Lnil){listok.eval("(do () (t))")}
    expect(Lnil){listok.eval("(do () (t) t)")}
    expect(Lkeyword('ok)){listok.eval("(do () (t :ok) t)")}


    expect(Lint(9)){listok.eval(
     """
     (def r 0)
     (do
       ((x 3 (- x 1)))
       ((eq x 0))
       (setf r (+ 1 r))
       (setf r (+ 2 r))
       )
     r
     """)}

    expect(Lint(0)){listok.eval(
     """
     (def r 0)
     (do () (t) (setf r 100))
     r
     """)}

    expect(LL(Lint(2), Lint(1))){listok.eval(
     """
     (do
       ((n 0 (+ 1 n)) (x nil (cons n x)))
       ((eq n 2) x))
     """)}

    expect(Lint(6)){listok.eval(
     """
     (do
       ((x 4 (- x 1))
       (r 0 (+ x r)))

       ((eq x 0) r)

       ;(write-line (string x " - " r))
       )
     """)}
     //expect(Lnil){listok.eval("(do ((n 0 (+ 1 n))) ((eq n 3)) (print n)  )")}


  }

  test("while") {
     expect(Lnil){listok.eval("(dowhile nil)")}
     expect(Lint(0)){listok.eval(
       """
       (def n 3)
       (dowhile (< 0 n)
        (setf n (- n 1))
        ;(print n)
        )
       n
       """
     )}
   }

   test("dotimes") {
     expect(Lnil){listok.eval("(dotimes (i 0))")}

     expect(LL(Lint(2), Lint(1), Lint(0))){listok.eval(
       """
       (def l nil)
         (do
         ((i 0 (+ 1 i)))
         ((eq i 3))
         (def x)
         (setf l (cons i l)))
       l
       """
     )}

     expect(LL(Lint(2), Lint(1), Lint(0))){listok.eval(
       """
       (def l nil)
       (dotimes (i 3) (setf l (cons i l)))
       l
       """
     )}

     expect(Lint(3)){listok.eval(
       """
       (def n 3)
       (def r 0)
       (dotimes (i n) (setf r (incr r)))
       r
       """
     )}
   }

}