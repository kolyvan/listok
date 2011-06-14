package ru.listok.test

import _root_.ru.listok._
import org.scalatest.FunSuite


class LazyTest extends FunSuite {

  def listok = new Listok

  def toInts(args: Int*) = LL(args.map (Lint(_)) :_*)

  test("lazy seq") {

    expect(toInts(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)){
      listok.eval("(to-list (take (lazyseq 0) 10))")
    }

    expect(Lnil){
      listok.eval("(to-list (lazyseq))")
    }

    expect(toInts(0, 1)){
      listok.eval("(to-list (lazyseq 0 1 (lazyseq)))")
    }

    expect(toInts(0,1,2,2,2)){
      listok.eval("(to-list (take (lazyseq 0 1 2) 5))")
    }

    expect(toInts(1, 1, 1, 1, 1)){
      listok.eval("(to-list (take (lazyseq (lambda () 1)) 5))")
    }

    expect(toInts(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)){
      listok.eval("(to-list (take (lazyseq 0 (lambda (n) (+ n 1))) 10))")
    }

    expect(toInts(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)){
      listok.eval("(to-list (take (lazyseq 0 incr) 10))")
    }

    expect(toInts(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)){
      listok.eval("(to-list (take (lazyseq-from 0) 10))")
    }

    expect(toInts(1, 2, 3, 4, 5)){
      listok.eval("(to-list (lazyseq-range 1 5))")
    }

    expect(Lint(55)){
      listok.eval("(reduce + (lazyseq-range 1 10))")
    }

    expect(toInts(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)){
      listok.eval("""
      (defun sieve (s)
        ;(def r 0)
        (lazyseq (head s)
          (lambda (n) ;n == (head s)
            (sieve
              (filter
                (lambda (x)
                  ;(write-line (string "n=" n " (head s)=" (head s)))
                  (assert (= n (head s)))
                  (/= 0 (rem x n)))
                (tail s)  ))
            )))

      (def primes (sieve (lazyseq-from 2)))
      ;(print (lazyseq-force (take primes 10)))
      (to-list (take primes 10))
      """)}
  }
}