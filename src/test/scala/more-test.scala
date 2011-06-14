package ru.listok.test

import _root_.ru.listok._
import org.scalatest.FunSuite

class MoreTest extends FunSuite {

  import Util.toLlist

  def listok = new Listok

   def lints(args: Int*) = LL(args.map (Lint(_)) :_*)

  test("flatmap") {

    val l = listok
    l.eval(
    """
    (defun flatmap (proc seq)
      (fold append nil (map proc seq)))
    """)

   expect(toLlist(3, 4)){l.eval(
    """
    (flatmap
      (lambda (x) (if (> (length x) 1) x nil))
      '((1) nil (3 4)))
    """)}
  }

  test("recurse") {
    val l = listok

    l.eval(
    """
    (def car head)
    (def cdr tail)

    (defun droplast (l)
      (cond ((null l) nil)
        ((null (cdr l)) nil)
        (t (cons (car l) (droplast (cdr l))))))

    (defun atomlist (l)
      (cond ((null l) t)
        ((atom l) nil)
        ((not (atom (car l))) nil)
        (t (atomlist (cdr l)))))

    (defun bulb (n)
      (cond ((zerop n) nil)
        ((= 1 n) (list nil))
        (t (list (bulb (- n 1))))))

    (defun first-atom (l)
      (cond ((null l) nil)
        ((atom l) l)
        (t (first-atom (car l)))))

    (defun remove-first (a l)
      (cond ((null l) nil)
        ((eq a (car l)) (cdr l)) ; < remove a
        (t (cons (car l)
          (remove-first a (cdr l)))) ; continue
        ))

    (defun atom-count (l)
      (cond ((null l) 0)
        ((atom l) 1)
        (t (+ (atom-count (car l))
              (atom-count (cdr l))))))

    (defun depth-of-list (l)
      (cond ((null l) 0)
        ((atom l) 0)
        (t (max
            (+ 1 (depth-of-list (car l)))
            (depth-of-list (cdr l))))
        ))


    (defun  calc-infix (&rest l)
      (defun infix2prefix (l)
      (cond ((null l) nil)
          ((atom l) l)
          (t (list
              (car (cdr l))
              (infix2prefix (car l))
              (infix2prefix (car (cdr (cdr l))))
              ))))
      (eval (infix2prefix l)))
    """)

    expect(toLlist(1, 2)){l.eval("(droplast '(1 2 3))")}
    expect(Ltrue){l.eval("(atomlist '(1 2 3))")}
    expect(Lnil){l.eval("(atomlist '(1 (2) 3))")}
    expect(Lnil){l.eval("(bulb 0)")}
    expect(toLlist(Nil)){l.eval("(bulb 1)")}
    expect(toLlist(List(Nil))){l.eval("(bulb 2)")}
    expect(Lint(1)){l.eval("(first-atom '((1) 2 3))")}
    expect(toLlist(1, 3, 2)){l.eval("(remove-first 2 '(1 2 3 2))")}
    expect(Lint(3)){l.eval("(atom-count '(((1)) 2 3))")}
    expect(Lint(4)){l.eval("(depth-of-list '((1) ((2 (nil)) 3) 4))")}
    expect(Lint(7)){l.eval("(calc-infix 2 + 5)")}
  }

  test("fibonacci") {
    val s = """
    (defun makefib ()
      (defun fibgen (n)
        (lambda () (setf n (+ n 1)) (fib n)))
      (defun fib (n)
        (cond ((= 0 n) 1)
          ((= 1 n) 1)
          (t (+ (fib (- n 2)) (fib (- n 1))))))
      (fibgen (- 1)))
    """

    val l = listok
    l.eval(s)
    expect(Lint(8)){l.eval(
      """
      (def f (makefib))
      (dotimes (n 5) (f))
      (f)
      """
      )}
  }

}