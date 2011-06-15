package ru.listok.test

import _root_.ru.listok._
import org.scalatest.FunSuite


class ThreadTest extends FunSuite {

  def listok = new Listok

  test("run") {
    expect(Lkeyword('ok)){listok.eval("""
    (def th (spawn worker ()
      ;(write-line "in thread")
      :ok))
    (join th)
    """)}
  }


  test("vars") {
    expect(Lint(42)){listok.eval("""
    (def x 41)
    (def th (spawn worker (x)
      (+ 1 x)))
    (join th)
    """)}

    expect(Lint(42)){listok.eval("""
    (def th (spawn worker ((x 41))
      (+ 1 x)))
    (join th)
    """)}
  }


  test("mailslot") {
    expect(Lstring("hello")){listok.eval(
    """
    (def th
      (spawn worker ((mymail (mailslot)))
        ;(write-line "in thread")
        (send mymail "hello")
        :ok))
    (first (receive))
    """
    )}

  }

  test("mailslot 2") {
    expect(Lint(43)){listok.eval(
    """
    (def th (spawn worker ()
      ;(write-line "in thread")
      (def x (receive))
      (send (second x) (incr (first x)))
      :ok))
    (thread-wait-init th 10) ; for avoiding race cond
    (send th 42)
    (first (receive))
    """)}
  }

  test("mailslot 3") {
    expect(Lint(45000)){listok.eval(
    """
    (def mymail (mailslot))

    (def th (spawn worker (mymail)
      ;(write-line "start worker thread")
      (dotimes (n 10000)
        (when (= 0 (rem n 1000))
          (send mymail n)))
      ;(terpri)
      ;(write-line "finish worker thread")
      (send mymail :done)
      :ok))

    (defun monitor (sum)
      ;(declare :tco)
      (match (first (receive))
        ((numberp n)
          ;(write-line (string n " = " sum))
          (monitor (+ n sum)))
        (:done
          ;(write-line "work is done")
          sum)
      ))

    (monitor 0)
    """
    )}
  }

}