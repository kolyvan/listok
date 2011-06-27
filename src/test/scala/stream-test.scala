package ru.listok.test

import _root_.ru.listok._
import org.scalatest.FunSuite


class StreamTest extends FunSuite with Helper {


  test("read char") {
    val l = listok
    l.eval("(def is (make-string-input-stream \"123\"))")
    expect(Lchar('1')) {l.eval("(read-char is)")}
    expect(Lchar('2')) {l.eval("(read-char is)")}
    expect(Lchar('3')) {l.eval("(read-char is)")}
    expect(Lnil) {l.eval("(read-char is)")}
  }

  test("read line") {
    val l = listok
    //l.eval("(def is (make-string-input-stream \"123\"))")
    l.eval("(def is (make-string-input-stream (string \"123\" #\\Newline \"abc\")))")
    expect(Lstring("123")) {l.eval("(read-line is)")}
    expect(Lstring("abc")) {l.eval("(read-line is)")}
    expect(Lnil) {l.eval("(read-line is)")}
  }

  test("read") {
    val l = listok
    l.eval("(def is (make-string-input-stream \":A\"))")
    expect(Lkeyword('A)) {l.eval("(read is)")}
    expect(Lnil) {l.eval("(read is)")}
  }

  test("write char") {
    val l = listok
    l.eval("(def os (make-string-output-stream))")
    expect(Lchar('a')) {l.eval("(write-char #\\a os)")}
    expect(Lchar('b')) {l.eval("(write-char #\\b os)")}
    expect(Lchar('c')) {l.eval("(write-char #\\c os)")}
    expect(Lstring("abc")) {l.eval("(get-output-stream-string os)")}
  }

  test("write string") {
    val l = listok
    l.eval("(def os (make-string-output-stream))")
    expect(Lstring("123")) {l.eval("(write-string \"123\" os)")}
    expect(Lstring("123")) {l.eval("(get-output-stream-string os)")}
  }

  test("print") {
    val l = listok
    l.eval("(def os (make-string-output-stream))")
    expect(Lkeyword('A)) {l.eval("(print :A os)")}
    expect(Lstring(":A\n")) {l.eval("(get-output-stream-string os)")} // \n:A -
  }

  test("standart") {
    val l = listok

    if (false)
    l.eval(
      """
      (print "hello anonymous")
      (terpri)
      (write-string "please, input your name: ")
      (def name (read-line))
      (write-string "your name is ")
      (write-line name)
      (write-line "bye")
     """)

    expect(Lstring("123")) {l.eval(
      """
      (def s (progn
        (def *standard-output* (make-string-output-stream))
        (write-string "123")
        (get-output-stream-string *standard-output*)))
      ;(write-line "abc")
      s
     """)}
  }

  test("file") {
    expect(Lint(58))
      {listok.eval(
        """
        (def s (open (string (current-directory) "/src/test/resources/test.file" )))
        (assert s)
        ;(when s
        ;  (dowhile (print (read-line s))))
        (def len (length (read-text s)))
        (close s)
        len
        """)}

    {listok.eval(
        """
        (def s (open (string (current-directory) "/src/test/resources/test.file") :input :binary ))
        (assert s)
        (def len (length (read-blob s)))
        (close s)
        len
        """)}

    //expect(Lnil)
    //  {listok.eval(
    //    """(open (string (current-directory) "/src/test/resources/no-such-file"))""")}
  }

  test("read byte") {
    val l = listok
    l.eval("(def is (make-blob-input-stream (blob (byte 1) (byte 2) (byte 3))))")
    expect(Lbyte(1)) {l.eval("(read-byte is)")}
    expect(Lbyte(2)) {l.eval("(read-byte is)")}
    expect(Lbyte(3)) {l.eval("(read-byte is)")}
    expect(Lnil) {l.eval("(read-byte is)")}
  }

  test("read blob") {
    val l = listok
    l.eval("(def is (make-blob-input-stream (blob (byte 1) (byte 2) (byte 3))))")
    expect(Lblob(Array(1.toByte, 2.toByte, 3.toByte))) {l.eval("(read-blob is)")}
    expect(Lnil) {l.eval("(read-blob is)")}
  }

  test("write byte") {
    val l = listok
    l.eval("(def os (make-blob-output-stream))")
   // peval(l, "(write-byte #\\a os)")
    expect(Lbyte('a')) {l.eval("(write-byte #\\a os)")}
    expect(Lbyte('b')) {l.eval("(write-byte #\\b os)")}
    expect(Lbyte('c')) {l.eval("(write-byte #\\c os)")}
    expect(Lblob(Array('a'.toByte, 'b'.toByte, 'c'.toByte))) {l.eval("(get-output-stream-blob os)")}
  }

  test("write blob") {
    val l = listok
    l.eval("(def os (make-blob-output-stream))")
    expect(Lblob(Array(1.toByte, 2.toByte, 3.toByte)))
      {l.eval("(write-blob (blob (byte 1) (byte 2) (byte 3)) os)")}
    expect(Lblob(Array(1.toByte, 2.toByte, 3.toByte)))
      {l.eval("(get-output-stream-blob os)")}
  }

  test("predicats") {
    val l = listok
    l.eval("(def is (make-string-input-stream \"123\"))")
    expect(Ltrue) {l.eval("(streamp is)")}
    expect(Ltrue) {l.eval("(open-stream-p is)")}
    expect(Ltrue) {l.eval("(input-stream-p is)")}
    expect(Lnil) {l.eval("(output-stream-p is)")}
    expect(Ltrue) {l.eval("(close is)")}
    expect(Lnil) {l.eval("(open-stream-p is)")}
  }

  if (false)
  test("socket") {
      val l = listok
      l.eval(
      """
      (def s (open-tcp-connection "lenta.ru" 80))
      (print s)
      (def out (get-output-stream s))
      (def in  (get-input-stream s))
      (write-line "HEAD / HTTP/1.0" out)
      (write-line "Host: lenta.ru" out)
      (newline out)
      (do
        ((x "^^^^^^^^^^^^^^" (read-line in)))
        ((not x))
        (write-line x))
      (close s)
      """
      )
  }

}
