package ru.listok.test

import _root_.ru.listok._
import org.scalatest.FunSuite


class TcoTest extends FunSuite {

  def listok = new Listok


  test("tco 1") {

      expect(Lint(10000)) {listok.eval(
      """
      (defun f (x max)
        (declare :tco)
        (if (eq x max)
            x
            (f (+ x 1) max)))
      (f 0 10000)
      """
      )}
  }


  test("tco 3") {
      expect(Lint(20))(listok.eval( // cannot into tco
        """
         (def z (lambda (x) (+ x 1)))
         (defun f (x max)
            (declare :tco)
            (if (eq x max)
                x
                (z (f (+ x 1) max))))
          (f 0 10)
          """
          ))
  }

   test("tco 4") {
      expect(Lint(28))( // 10
      //intercept[java.lang.StackOverflowError](
      //println(
      listok.eval( // cannot into tco
        """
        (defun f (x res max)
          (declare :tco)
          (+ 1 (if (< x max) (f (+ 1 x) (+ x x) max) res))
          )
        (f 1 0 10)
        """))
  }

  test("tco 5") {
      expect(Ltrue)(listok.eval(
      """
      (defun evenp (n)
        (declare :tco)
        (if (eq n 0)
          t
          (oddp (- n 1))))

      (defun oddp (n)
        (declare :tco)
         (if (eq n 0)
           nil
         (evenp (- n 1))))
      (evenp 10002)
      """))
  }

  test("tco 6") {
    if (false) // cannot tco
      intercept[java.lang.StackOverflowError] {listok.eval(
      """
       (def z (lambda (x) (+ x 1)))
       (defun f (x max)
          (declare :tco)
          (if (eq x max)
              x
              (z (f (+ x 1) max))))
        (f 0 1000)
      """
      )}
  }


  test("fact") {
      val fact =
      """
      ;(trace 'iter 'fact)
      (defun fact (n)
        (defun iter (sum i)
          (declare :tco)
          (if (> i n) sum
            (iter (* i sum)
                  (+ i 1))))
        (iter 1 1))
      """

      val l = listok
      l.eval(fact)
      expect(Lint(1)) { l.eval("(fact 0)") }
      expect(Lint(1)) { l.eval("(fact 1)") }
      expect(Lint(2)) { l.eval("(fact 2)") }
      expect(Lint(6)) { l.eval("(fact 3)") }
      expect(Lint(3628800)) { l.eval("(fact 10)") }
  }

}