package ru.listok.test

import _root_.ru.listok._
import org.scalatest.FunSuite
import net.fyrie.ratio.Ratio

class NumberTest extends FunSuite {

  def listok = new Listok

   test("add") {
    expect(Lint(0)) {listok.eval("(+)")}
    expect(Lint(2)) {listok.eval("(+ 2)")}
    expect(Lint(7)) {listok.eval("(+ 2 5)")}
    expect(Lfloat(14f)) {listok.eval("(+ 2 5 7.0)")}
    expect(Lfloat(8f)) {listok.eval("(+ 1.0 0.0 7.0 )")}
    expect(Llong(2147483648l)) {listok.eval("(+ 1 2147483647)")}
    expect(Llong(-2147483649l)) { listok.eval("(+ 1 -2147483650)")}
    expect(Lbignum(BigInt("9223372036854775808"))) {listok.eval("(+ 1 9223372036854775807)")}
    expect(Lfloat(10000000000000000000.0f)) {listok.eval("(+ 10000000000000000000 0.0)")}
    expect(Lbignum(BigInt("10000000000000000000"))){listok.eval("(+ 10000000000000000000 0)")}

  }

  test("sub") {
    intercept[SyntaxError] {listok.eval("(-)")}
    expect(Lint(-20)) {listok.eval("(- 20)")}
    expect(Lint(5)) {listok.eval("(- 20 5 7 3)")}
    expect(Lfloat(10f)) {listok.eval("(- 20 10.0)")}
    expect(Lfloat(-10f)) {listok.eval("(- 10.0 20)")}
    expect(Llong(-2147483657l)) {listok.eval("(- -10 2147483647)")}
    expect(Llong(-3147483640l)) { listok.eval("(- 10 3147483650)")}
    expect(Lbignum(BigInt("-9323372036854775806"))) {listok.eval("(- 1 9323372036854775807)")}
  }

  test("mul") {
    expect(Lint(1)) {listok.eval("(*)")}
    expect(Lint(24)) {listok.eval("(* 2 3 4)")}
    expect(Lfloat(6f)) {listok.eval("(* 2 3.0)")}
    expect(Lfloat(24f)) {listok.eval("(* 2.0 3.0 4.0 1.0)")}
    expect(Llong(21474836470l)) {listok.eval("(* 10 2147483647)")}
    expect(Lfloat(10000000000000000000.0f)) {listok.eval("(* 10000000000000000000 1.0)")}
    expect(Lbignum(BigInt("10000000000000000000"))){listok.eval("(* 10000000000000000000 1)")}
  }

  test("div") {
    intercept[SyntaxError]{listok.eval("(/)")}
    intercept[ArithmeticError]{listok.eval("(/ 1 0)")}
    expect(Lint(2)) {listok.eval("(/ 2)")}
    expect(Lint(2)) {listok.eval("(/ 4 2)")}
    expect(Lfloat(10f)) {listok.eval("(/ 20 2.0)")}
    expect(Lfloat(10f)) {listok.eval("(/ 20.0 2.0 1.0)")}
    expect(Lratio(Ratio(3, 4))) {listok.eval("(/ 3 4)")}
    expect(Lratio(Ratio(-4, 3))) {listok.eval("(/ 4 -3)")}
    expect(Lratio(Ratio(3, 8))) {listok.eval("(/ 3 4 2)")}
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

    expect(Lnil){listok.eval("(< 2147483648 2147483647)")}
    expect(Ltrue){listok.eval("(< -2147483648 -2147483647)")}
    expect(Ltrue){listok.eval("(< 1 2147483647 3147483647 1e11)")}
    expect(Lnil){listok.eval("(< -3147483647 -1e11)")}

    expect(Lnil){listok.eval("(< (/ 2 4) (/ 1 4))")}
    expect(Ltrue){listok.eval("(< (/ 1 4) (/ 3 4))")}
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

    expect(Ltrue){listok.eval("(> 2147483648 2147483647)")}
    expect(Lnil){listok.eval("(> -2147483648 -2147483647)")}
    expect(Lnil){listok.eval("(> 1 2147483647 3147483647 1e11)")}
    expect(Ltrue){listok.eval("(> -3147483647 -1e11)")}

    expect(Lnil){listok.eval("(> (/ 1 5) (/ 6 5))")}
    expect(Ltrue){listok.eval("(> (/ 2 4) (/ 1 5))")}
  }


  test("incr decr") {
    expect(Lint(1)){listok.eval("(incr 0)")}
    expect(Lint(-1)){listok.eval("(decr 0)")}
    expect(Llong(2147483648l)){listok.eval("(incr 2147483647)")}
    expect(Lint(2147483647)){listok.eval("(decr 2147483648)")}
  }

  test("rem") {
    expect(Lint(1)){listok.eval("(rem 3 2)")}
    expect(Lint(0)){listok.eval("(rem 4 2)")}
    expect(Llong(10000000000l)){listok.eval("(rem 30000000000 20000000000)")}
    expect(Lint(0)){listok.eval("(rem 40000000000 20000000000)")}
  }

  test("mod") {
   // expect(Lint(1)){listok.eval("(mod 3 2)")}
   // expect(Lint(-1)){listok.eval("(mod 3 -2)")}
   // expect(Lint(0)){listok.eval("(mod 4 2)")}
   // expect(Lint(0)){listok.eval("(mod 4 -2)")}
  }

  test("predicat") {
    expect(Lnil){listok.eval("(oddp 0)")}
    expect(Ltrue){listok.eval("(oddp 1)")}
    expect(Lnil){listok.eval("(oddp 2)")}
    expect(Lnil){listok.eval("(oddp 2147483648)")}
    expect(Ltrue){listok.eval("(oddp 2147483649)")}

    expect(Ltrue){listok.eval("(evenp 0)")}
    expect(Lnil){listok.eval("(evenp 1)")}
    expect(Ltrue){listok.eval("(evenp 2)")}
    expect(Ltrue){listok.eval("(evenp 2147483648)")}
    expect(Lnil){listok.eval("(evenp 2147483649)")}

    expect(Lnil){listok.eval("(zerop -1)")}
    expect(Ltrue){listok.eval("(zerop 0)")}
    expect(Lnil){listok.eval("(zerop 1)")}
    expect(Lnil){listok.eval("(zerop -2147483649)")}
    expect(Lnil){listok.eval("(zerop 2147483649)")}

    expect(Lnil){listok.eval("(plusp -1)")}
    expect(Lnil){listok.eval("(plusp 0)")}
    expect(Ltrue){listok.eval("(plusp 1)")}
    expect(Lnil){listok.eval("(plusp -2147483649)")}
    expect(Ltrue){listok.eval("(plusp 2147483649)")}

    expect(Ltrue){listok.eval("(minusp -1)")}
    expect(Lnil){listok.eval("(minusp 0)")}
    expect(Lnil){listok.eval("(minusp 1)")}
    expect(Ltrue){listok.eval("(minusp -2147483649)")}
    expect(Lnil){listok.eval("(minusp 2147483649)")}
  }

  test("min max") {
    expect(Lint(1)){listok.eval("(min 1)")}
    expect(Lint(-1)){listok.eval("(min 2 (- 1) 3)")}
    expect(Lint(1)){listok.eval("(max 1)")}
    expect(Lint(3)){listok.eval("(max 2 (- 1) 3)")}
    expect(Lfloat(1.0f)){listok.eval("(min 1.0)")}
    expect(Lint(-1)){listok.eval("(min 2 -1 3.0)")}
    expect(Lfloat(1.0f)){listok.eval("(max 1.0)")}
    expect(Llong(2147483649l)){listok.eval("(max 2.0 -1 2147483649)")}
  }

  test("abs") {
    expect(Lint(0)){listok.eval("(abs 0)")}
    expect(Lint(1)){listok.eval("(abs 1)")}
    expect(Lint(1)){listok.eval("(abs -1)")}
    expect(Lfloat(1.0f)){listok.eval("(abs -1.0)")}
    expect(Llong(2147483649l)){listok.eval("(abs -2147483649)")}
  }
}