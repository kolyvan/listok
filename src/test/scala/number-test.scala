package ru.listok.test

import _root_.ru.listok._
import org.scalatest.FunSuite


class NumberTest extends FunSuite {

  def listok = new Listok

   test("add") {
    expect(Lint(0)) {listok.eval("(+)")}
    expect(Lint(2)) {listok.eval("(+ 2)")}
    expect(Lint(7)) {listok.eval("(+ 2 5)")}
    expect(Lfloat(14f)) {listok.eval("(+ 2 5 7.0)")}
    expect(Lfloat(8f)) {listok.eval("(+ 1.0 0.0 7.0 )")}
  }

  test("sub") {
    intercept[SyntaxError] {listok.eval("(-)")}
    expect(Lint(-20)) {listok.eval("(- 20)")}
    expect(Lint(5)) {listok.eval("(- 20 5 7 3)")}
    expect(Lfloat(10f)) {listok.eval("(- 20 10.0)")}
    expect(Lfloat(-10f)) {listok.eval("(- 10.0 20)")}
  }

  test("mul") {
    expect(Lint(1)) {listok.eval("(*)")}
    expect(Lint(24)) {listok.eval("(* 2 3 4)")}
    expect(Lfloat(6f)) {listok.eval("(* 2 3.0)")}
    expect(Lfloat(24f)) {listok.eval("(* 2.0 3.0 4.0 1.0)")}
  }

  test("div") {
    intercept[SyntaxError]{listok.eval("(/)")}
    intercept[ArithmeticError]{listok.eval("(/ 1 0)")}
    expect(Lint(2)) {listok.eval("(/ 2)")}
    expect(Lint(2)) {listok.eval("(/ 20 2 5)")}
    expect(Lfloat(10f)) {listok.eval("(/ 20 2.0)")}
    expect(Lfloat(10f)) {listok.eval("(/ 20.0 2.0 1.0)")}
  }

  test("less") {
    expect(Ltrue){listok.eval("(< 1)")}
    expect(Ltrue){listok.eval("(< 1 2 3)")}
    expect(Lnil){listok.eval("(< 3 2 1)")}
    expect(Lnil){listok.eval("(< 1 3 2)")}
    expect(Lnil){listok.eval("(< 1 1 1)")}

    expect(Ltrue){listok.eval("(<= 1)")}
    expect(Ltrue){listok.eval("(<= 1 2 3)")}
    expect(Lnil){listok.eval("(<= 3 2 1)")}
    expect(Lnil){listok.eval("(<= 1 3 2)")}
    expect(Ltrue){listok.eval("(<= 1 1 1)")}
  }

  test("more") {
    expect(Ltrue){listok.eval("(> 1)")}
    expect(Lnil){listok.eval("(> 1 2 3)")}
    expect(Ltrue){listok.eval("(> 3 2 1)")}
    expect(Lnil){listok.eval("(> 1 3 2)")}
    expect(Lnil){listok.eval("(> 1 1 1)")}

    expect(Ltrue){listok.eval("(>= 1)")}
    expect(Lnil){listok.eval("(>= 1 2 3)")}
    expect(Ltrue){listok.eval("(>= 3 2 1)")}
    expect(Lnil){listok.eval("(>= 1 3 2)")}
    expect(Ltrue){listok.eval("(>= 1 1 1)")}
  }

  test("incr decr") {
    expect(Lint(1)){listok.eval("(incr 0)")}
    expect(Lint(-1)){listok.eval("(decr 0)")}
  }

  test("rem") {
    expect(Lint(1)){listok.eval("(rem 3 2)")}
    expect(Lint(0)){listok.eval("(rem 4 2)")}
  }

  test("predicat") {
    expect(Lnil){listok.eval("(oddp 0)")}
    expect(Ltrue){listok.eval("(oddp 1)")}
    expect(Lnil){listok.eval("(oddp 2)")}

    expect(Ltrue){listok.eval("(evenp 0)")}
    expect(Lnil){listok.eval("(evenp 1)")}
    expect(Ltrue){listok.eval("(evenp 2)")}

    expect(Lnil){listok.eval("(zerop (- 1))")}
    expect(Ltrue){listok.eval("(zerop 0)")}
    expect(Lnil){listok.eval("(zerop 1)")}

    expect(Lnil){listok.eval("(plusp (- 1))")}
    expect(Lnil){listok.eval("(plusp 0)")}
    expect(Ltrue){listok.eval("(plusp 1)")}

    expect(Ltrue){listok.eval("(minusp (- 1))")}
    expect(Lnil){listok.eval("(minusp 0)")}
    expect(Lnil){listok.eval("(minusp 1)")}

  }

  test("min max") {
    expect(Lint(1)){listok.eval("(min 1)")}
    expect(Lint(-1)){listok.eval("(min 2 (- 1) 3)")}
    expect(Lint(1)){listok.eval("(max 1)")}
    expect(Lint(3)){listok.eval("(max 2 (- 1) 3)")}

    expect(Lfloat(1.0f)){listok.eval("(min 1.0)")}
    expect(Lfloat(-1.0f)){listok.eval("(min 2 (- 1) 3.0)")}
    expect(Lfloat(1.0f)){listok.eval("(max 1.0)")}
    expect(Lfloat(3.0f)){listok.eval("(max 2.0 (- 1) 3)")}

  }
}