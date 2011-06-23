package ru.listok.test

import _root_.ru.listok._
import org.scalatest.FunSuite


class TempTest extends FunSuite with Helper {

  import Util.toLlist

  /*
  test("trace") {

    val l = listok

    l.backtrace = true

    l.eval(
    """
    (defun f1 (n)
      ;(declare :tco)
      (if (< n 5) (f1 (+ 1 n)) n))
    (+ 1 2)
    (trace f1)
    (f1 1)
    """
    )

  }

  test("backtrace") {
    val s = """
    (trace 'foo 'zoo 'bar)
    ;(untrace 'foo)
    (defun foo (x)
      (defun bar (x)
        (error "in bar")
        t)
      (progn
      (+ 1 1)
      (bar (+ 2 2))))
    (defun zoo(z)
     (let ((x 11))
        (foo x)))
    (zoo 21)
    """
    try {
      val l = listok
      l.backtrace = true
      l.eval(s)
    } catch {
      case ScriptError(msg, env) =>
        println("error: " + msg)
        println("backtrace:")
        Util.backtrace(env, true).foreach { p =>
          println(" in " + p.pp + "\t: " + p.backtrace)
        }
    }
  }
  */

  /*
  test("optimize") {

    val before = Listok.parse(
      """
        (+ 1
          (( lambda () (def x 2) (* x 5) ))
          2)
      """)
    // val before = Listok.parse("""(tail (list 1 2 3))""")

    val after = Optimize.run(before)
    println("before:" + Util.pp(before))
    println("after :" + Util.pp(after))

  }
  */

  /*
  test("process") {
    val l = listok
    l.eval(
      """
      (def p (process "ls" "-l"))
      (def s (process-input-stream p))
      (dowhile (print (read-line s)))
      (print (process-exit-value p))
      """)
  }
 */

  test("json") {
    val l = listok
    if (false)
    l.eval(
      """
      (def query "http://ajax.googleapis.com/ajax/services/language/translate?langpair=en|ru&q=hello&v=1.0")
      (def conn (open-url query))
      (def reply (read-text conn))
      (close conn)
      (def json (json-parse reply))
      (print (hm-get (hm-get json :responseData) :translatedText))
      """)

    expect(toLlist(1.0f, 2.0f, 3.0f, 4.0f))(l.eval("""
    (def s (open (string (current-directory) "/src/test/resources/test.file" )))
    (def text (read-text s))
    (close s)
    (hm-get (json-parse text) :data)
    """))
  }

}
