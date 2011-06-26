package ru.listok.test

import _root_.ru.listok._
import org.scalatest.FunSuite


class BytesTest extends FunSuite with Helper {

  test("byte") {

    //peval(listok, "(byte 1)")

    expect(Lbyte(0)){listok.eval("(byte 0)")}
    expect(Lbyte(1)){listok.eval("(byte 1)")}
    expect(Lbyte(255.toByte)){listok.eval("(byte 255)")}
    expect(Lbyte('a'.toByte)){listok.eval("(byte #\\a)")}
    expect(Lint(65)){listok.eval("(to-int (byte 65))")}
    expect(Lchar('A')){listok.eval("(to-char (byte 65))")}
    expect(Lstring("65")){listok.eval("(to-str (byte 65))")}
    expect(Ltrue){listok.eval("(bytep (byte 65))")}
    expect(Lnil){listok.eval("(bytep 1)")}

  }

  test("blob") {

    expect(Lblob(Array.empty))
      { listok.eval("(blob)") }

    expect(Ltrue)
      { listok.eval("(eq (blob) (blob))") }

    expect(Ltrue)
      { listok.eval("(eq (blob 1) (blob 1))") }

    expect(Lnil)
      { listok.eval("(eq (blob 1) (blob 2))") }

    expect(Ltrue){listok.eval("(blobp (blob))")}

    expect(Lnil){listok.eval("(blobp 1)")}

    expect(Lint(0))
      { listok.eval("(length (blob))") }

    expect(Lint(8)) // 2 int == 8 bytes
      { listok.eval("(length (blob 1 2))") }

    expect(Lbyte(1))
      { listok.eval("(elt (blob (byte 0) (byte 1) (byte 2)) 1)") }

    expect(Lbyte(42))
      { listok.eval("(let ((b (blob (byte 0) (byte 1) (byte 2)))) (setelt b 1 42) (elt b 1))") }

    expect(Lblob(Array(0.toByte,1.toByte,2.toByte)))
      {listok.eval("(blob (byte 0) (byte 1) (byte 2))")}

    // 2 bytes per char
    expect(Lblob(Array(0.toByte, 'A'.toByte, 0.toByte, 'B'.toByte, 0.toByte, 'C'.toByte)))
      {listok.eval("(blob #\\A #\\B #\\C)") }

    expect(Lblob("ABC".getBytes))
      {listok.eval("(blob \"ABC\")") }

    expect(Lblob("ABCfoo".getBytes))
      {listok.eval("(blob (blob \"ABC\") (blob) (blob \"foo\"))") }

    //peval(listok, "(to-list (blob #\\A #\\B #\\C))")


  }



}