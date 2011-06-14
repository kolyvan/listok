package ru.listok.test

import _root_.ru.listok._
import org.scalatest.FunSuite
import javax.sound.sampled.Line


class StreamTest extends FunSuite {

  def listok = new Listok

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
    expect(Lstring("\n:A")) {l.eval("(get-output-stream-string os)")} // \n:A -
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

  //  expect(Lnil)
  //    {listok.eval(
  //      """(open (string (current-directory) "/src/test/resources/no-such-file"))""")}
  }

  if (false)
  test("socket") {
      val l = listok
      l.eval(
      """
      (def s (open-socket "lenta.ru" 80))
      (write-line "HEAD / HTTP/1.0" s)
      (write-line "Host: lenta.ru" s)
      (terpri s)
      (do
        ((x "^^^^^^^^^^^^^^" (read-line s)))
        ((not x))
        (write-line x))
      (close s)
      """
      )
  }
}
