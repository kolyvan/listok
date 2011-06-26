package ru.listok.test

import _root_.ru.listok._
import org.scalatest.FunSuite
import java.io.{File, FileInputStream}
import scala.math.BigInt
import net.fyrie.ratio.Ratio


class CompilerTest extends FunSuite {

  def listok = new Listok


  test("sbinary") {
    val l1 = List(Lint(1), Lfloat(10.0), LL(Lstring("foo"), Lchar('x')))
    expect(l1) { Compiler.load(Compiler.compile(l1)) }

    val l2 = List(Lquote(Lint(42)))
    expect(l2) { Compiler.load(Compiler.compile(l2)) }

    val l3 = List(Lsymbol('foo))
    expect(l3) { Compiler.load(Compiler.compile(l3)) }

    val l4 = List(Lnil)
    expect(l4) { Compiler.load(Compiler.compile(l4)) }

    val l5 = List(Llambda(List(Lsymbol('a), Lsymbol('b)), List(LL( Lint(42), Lnil, Ltrue)), 'lambda))
    expect(l5) { Compiler.load(Compiler.compile(l5)) }

    val l6 = List(SpecialForms.make('def))
    Compiler.load(Compiler.compile(l6)) match {
      case xs: List[_] if xs.length == 1  =>
        xs.head match {
          case Lsform(_, s) if s == 'def =>
          case _ => fail("expected sform def but got " + xs)
        }
      case err =>  fail("expected List('def) but got " + err)
    }

    val l7 = List(Lkeyword('xyz))
    expect(l7) { Compiler.load(Compiler.compile(l7)) }

    val l8 = List(LV(Lint(1), Lint(2)))
    expect(l8) { Compiler.load(Compiler.compile(l8)) }

    val l9 = List(Lpair(Lint(42), Lregex("\\w+")))
    expect(l9) { Compiler.load(Compiler.compile(l9)) }

    val l10 = List(Llong(Long.MaxValue))
    expect(l10) { Compiler.load(Compiler.compile(l10)) }

    val l11 = List(Lbignum(BigInt("10000000000000000000")))
    expect(l11) { Compiler.load(Compiler.compile(l11)) }

    val l12 = List(Lratio(Ratio(13, 27)))
    expect(l12) { Compiler.load(Compiler.compile(l12)) }

    val l13 = List(Lbyte(65))
    expect(l13) { Compiler.load(Compiler.compile(l13)) }

    val l14 = List(Lblob(Array(1.toByte, 65.toByte, 127.toByte, 255.toByte, 2.toByte)))
    expect(l14) { Compiler.load(Compiler.compile(l14)) }
  }

  test("compile") {

    expect(Lint(6)) {
      val p = Listok.parse("(def fact (lambda (n) (if (< n 2) 1 (* n (fact (- n 1))))))(fact 3)")
      Listok.eval(listok.root, Compiler.load(Compiler.compile(p)))
    }

    expect(Lint(3)) {
      val p = Listok.parse("""
        (defmacro add1 (x) `(setf ,x (+ 1 ,x)))
        (def x 1)
        (add1 x)
        (add1 x)
        x
        """)
      val b = Compiler.compile(p, listok.root)
      val l = Compiler.load(b)
      Listok.eval(listok.root, l)
     }
  }

}

