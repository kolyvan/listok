package ru.listok.test

import _root_.ru.listok._
import org.scalatest.FunSuite


class SeqTest extends FunSuite {

  import Util.toLlist

  def listok = new Listok

  test("builders") {
    expect(LL(Lint(1),Lfloat(2.0),Ltrue,Lchar('y'), Lstring("foo"), Lnil)) {
      listok.eval("(list 1 2.0 t #\\y \"foo\" nil)")
    }
    expect(Lnil) {listok.eval("(list)")}

    expect(LV(Lint(1),Lfloat(2.0),Ltrue,Lchar('y'), Lstring("foo"), Lnil)) {
      listok.eval("(vector 1 2.0 t #\\y \"foo\" nil)")
    }
    expect(LV()) {listok.eval("(vector)")}

    expect(Lstring("")) {listok.eval("(string)")}
    expect(Lstring("123abckn")) {listok.eval("""(string 1 2 3 #\a #\b #\c :k 'n)""")}

    expect(Lstring("ab c\n")) {listok.eval("""(string #\a #\b #\Space #\c #\Newline )""")}

  }

  test("tail") {
    expect(LL(Lint(2),Lint(0))) {listok.eval("(tail (list 1 2 0))")}
    expect(Lnil) {listok.eval("(tail nil)")}
    expect(Lnil) {listok.eval("(tail ())")}
    expect(Lnil) {listok.eval("(tail (list))")}
    expect(Lnil) {listok.eval("(tail (list 1))")}
    expect(Lnil) {listok.eval("(tail (quote ()))")}

    expect(LL(Lint(2))) {listok.eval("(tail (list 1 2))")}
    expect(LV(Lint(2))) {listok.eval("(tail (vector 1 2))")}

    expect(Lstring("")) {listok.eval("""(tail "")""")}
    expect(Lstring("oo")) {listok.eval("""(tail "foo")""")}
  }

  test("head") {
    expect(Lint(1)) {listok.eval("(head (list 1 2 0))")}
    expect(Lnil) {listok.eval("(head nil)")}
    expect(Lnil) {listok.eval("(head (list))")}
    expect(Lnil) {listok.eval("(head ())")}
    expect(Lint(1)) {listok.eval("(head (list 1))")}
    expect(Lnil) {listok.eval("(head (quote ()))")}

    expect(Lint(1)) {listok.eval("(head (list 1 2))")}
    expect(Lint(1)) {listok.eval("(head (vector 1 2))")}

    expect(Lnil) {listok.eval("""(head "")""")}
    expect(Lchar('f')) {listok.eval("""(head "foo")""")}
  }

  test("last") {
    expect(Lint(0)) {listok.eval("(last (list 1 2 0))")}
    expect(Lnil) {listok.eval("(last nil)")}
    expect(Lnil) {listok.eval("(last (list))")}
    expect(Lnil) {listok.eval("(last ())")}
    expect(Lint(1)) {listok.eval("(last (list 1))")}
    expect(Lnil) {listok.eval("(last (quote ()))")}
    expect(Lint(2)) {listok.eval("(last (list 1 2))")}
    expect(Lint(2)) {listok.eval("(last (vector 1 2))")}
    expect(Lnil) {listok.eval("""(last "")""")}
    expect(Lchar('c')) {listok.eval("""(last "abc")""")}
  }

  test ("cons") {
    expect(LL(Lint(1), Lint(2), Lint(3))) {listok.eval("(cons 1 (list 2 3))") }
    expect(LL(Lnil)) {listok.eval("(cons nil nil)") }
    expect(LL(Lint(1))) {listok.eval("(cons 1 nil)") }
    expect(LL(Lint(1))) {listok.eval("(cons 1 ())") }
    expect(LL(Lint(1))) {listok.eval("(cons 1 (list))") }
    expect(LV(Lint(1), Lint(2), Lint(3))) {listok.eval("(cons 1 (vector 2 3))") }

    expect(Lstring("foo")) {listok.eval("""(cons #\f "oo")""") }
  }

  test("foreach") {
    expect(Lnil){listok.eval("(foreach (lambda (x) x) '(1 2))")}
    val l = listok
    l.eval("(def n nil)(defun f (x) (setf n (cons x n)))")
    expect(LL(Lint(2), Lint(1))){l.eval("(foreach f '(1 2)) n")}
    expect(LL(Lint(2), Lint(1))){l.eval("(setf n nil)(foreach f (vector 1 2)) n")}
  }

  test("reduce") {
    expect(Lnil){listok.eval("(reduce (lambda (n x) (+ n x)) nil)")}
    expect(Lint(6)){listok.eval("(reduce (lambda (n x) (+ n x)) '(1 2 3))")}
    expect(Lnil){listok.eval("(reduce (lambda (n x) (+ n x)) nil)")}
    expect(Lint(6)){listok.eval("(reduce (lambda (n x) (+ n x)) (vector 1 2 3))")}
  }

  test("fold") {
    expect(Lint(1)){listok.eval("(fold (lambda (n x) (+ n x)) 1 nil )")}
    expect(Lint(7)){listok.eval("(fold (lambda (n x) (+ n x)) 1 '(1 2 3) )")}
    expect(Lint(7)){listok.eval("(fold (lambda (n x) (+ n x)) 1 (vector 1 2 3))")}

    expect(Lint(7)){listok.eval("(fold + 1 '(1 2 3) )")}
  }

  test("map") {
    expect(LL(Lint(2), Lint(4))){listok.eval("(map (lambda (x) (* x 2)) '(1 2))")}
    expect(LV(Lint(2), Lint(4))){listok.eval("(map (lambda (x) (* x 2)) (vector 1 2))")}
  }

  test("flatmap") {
    expect(toLlist(1, 2, 3, 4)){
      listok.eval("(flatmap (lambda (x) x) '(1 2 () nil (3) 4))")}

  }

  test("length") {
    expect(Lint(0))(listok.eval("(length nil)"))
    expect(Lint(0))(listok.eval("(length ())"))
    expect(Lint(0))(listok.eval("(length '())"))
    expect(Lint(1))(listok.eval("(length '(1))"))
    expect(Lint(2))(listok.eval("(length (list 1 2))"))
    expect(Lint(2))(listok.eval("(length (vector 1 2))"))
    expect(Lint(3))(listok.eval("""(length "foo")"""))
  }

  test("append") {
    expect(Lnil)(listok.eval("(append nil nil)"))
    expect(LL(Lint(1)))(listok.eval("(append nil 1)"))
    expect(LL(Lint(1)))(listok.eval("(append () 1)"))
    expect(LL(Lint(1), Lint(2)))(listok.eval("(append nil '(1 2))"))
    expect(LL(Lint(1), Lint(2)))(listok.eval("(append '(1) 2)"))
    expect(LL(Lint(1), Lint(2), Lint(3), Lint(4)))(listok.eval("(append '(1 2) '(3 4))"))
    expect(LL(Lint(1), Lint(2), Lint(3), Lint(4)))(listok.eval("(append '(1) 2 '(3 4))"))

    expect(LV())(listok.eval("(append (vector) nil)"))
    expect(LV(Lint(1)))(listok.eval("(append (vector) 1)"))
    expect(LV(Lint(1), Lint(2)))(listok.eval("(append nil (vector 1 2))"))
    expect(LV(Lint(1), Lint(2)))(listok.eval("(append (vector 1) 2)"))
    expect(LV(Lint(1), Lint(2), Lint(3), Lint(4)))(listok.eval("(append (vector 1 2) '(3 4))"))
    expect(LV(Lint(1), Lint(2), Lint(3), Lint(4)))(listok.eval("(append (vector 1 2) (vector 3 4))"))
    expect(LV(Lint(1), Lint(2), Lint(3), Lint(4)))(listok.eval("(append (vector 1) 2 3 4)"))

    expect(Lstring(""))(listok.eval("""(append "" "")"""))
    expect(Lstring("foo"))(listok.eval("""(append "" "foo")"""))
    expect(Lstring("foo"))(listok.eval("""(append "f" "oo")"""))
    expect(Lstring("foo"))(listok.eval("""(append "f" "o" #\o)"""))
  }

  test("reverse") {
    expect(Lnil)(listok.eval("(reverse nil)"))
    expect(LL(Lint(1), Lint(2)))(listok.eval("(reverse '(2 1))"))
    expect(LV())(listok.eval("(reverse (vector))"))
    expect(LV(Lint(1), Lint(2)))(listok.eval("(reverse (vector 2 1))"))

    expect(Lstring(""))(listok.eval("""(reverse "")"""))
    expect(Lstring("rab"))(listok.eval("""(reverse "bar")"""))

  }

  test("elt") {
    expect(Lint(2))(listok.eval("(elt (list 1 2 3) 1)"))
    expect(Lint(3))(listok.eval("(elt (vector 1 2 3) 2)"))
    expect(Lchar('a'))(listok.eval("""(elt "bar" 1)"""))

    intercept[SyntaxError](listok.eval("(elt nil 1)"))
    intercept[SyntaxError](listok.eval("(elt '(1) 1)"))
    intercept[SyntaxError](listok.eval("(elt (vector) 1)"))
  }

  test("set-elt") {
    expect(Lint(22))(listok.eval("(set-elt (vector 1 2 3) 1 22)"))
    expect(LV(Lkeyword('a), Lint(20)))(listok.eval("(def v (vector 10 20)) (set-elt v 0 :a) v"))
    intercept[TypeError](listok.eval("(set-elt (list 1 2 3) 1 22)"))
  }

  test("subseq") {
    expect(Lnil)(listok.eval("(subseq nil 0)"))
    expect(LL(Lint(1), Lint(2)))(listok.eval("(subseq '(1 2) 0)"))
    expect(LL(Lint(3)))(listok.eval("(subseq '(1 2 3) 2)"))
    expect(LL(Lint(2), Lint(3)))(listok.eval("(subseq '(1 2 3) 1)"))
    expect(LL(Lint(2), Lint(3)))(listok.eval("(subseq '(1 2 3 4) 1 3)"))

    expect(Lnil)(listok.eval("(subseq (vector) 0)"))
    expect(LV(Lint(1), Lint(2)))(listok.eval("(subseq (vector 1 2) 0)"))
    expect(LV(Lint(3)))(listok.eval("(subseq (vector 1 2 3) 2)"))
    expect(LV(Lint(2), Lint(3)))(listok.eval("(subseq (vector 1 2 3) 1)"))
    expect(LV(Lint(2), Lint(3)))(listok.eval("(subseq (vector 1 2 3 4) 1 3)"))

    expect(Lstring("foobar"))(listok.eval("""(subseq "foobar" 0)"""))
    expect(Lstring("ob"))(listok.eval("""(subseq "foobar" 2 4)"""))
  }

  test("find") {
    expect(Lnil)(listok.eval("(find 1 nil)"))
    expect(Lnil)(listok.eval("(find 1 '(2 3))"))
    expect(Lint(1))(listok.eval("(find 1 '(2 3 1))"))
    expect(Lint(1))(listok.eval("(find 1 (vector 2 3 1))"))
    expect(Lchar('1'))(listok.eval("""(find #\1 "231")"""))

    expect(Lnil)(listok.eval("(find-if (lambda (x) (eq x 1)) nil)"))
    expect(Lnil)(listok.eval("(find-if (lambda (x) (eq x 1)) '(2 3))"))
    expect(Lint(1))(listok.eval("(find-if (lambda (x) (eq x 1)) '(2 3 1))"))
    expect(Lint(1))(listok.eval("(find-if (lambda (x) (eq x 1)) (vector 2 3 1))"))
    expect(Lchar('1'))(listok.eval("""(find-if (lambda (x) (eq x #\1)) "231")"""))
  }

  test("position") {
    expect(Lnil)(listok.eval("(position 1 nil)"))
    expect(Lnil)(listok.eval("(position 1 '(2 3))"))
    expect(Lint(2))(listok.eval("(position 1 '(2 3 1))"))
    expect(Lint(2))(listok.eval("(position 1 (vector 2 3 1))"))
    expect(Lint(2))(listok.eval("""(position #\1 "231")"""))

    expect(Lnil)(listok.eval("(position-if (lambda (x) (eq x 1)) nil)"))
    expect(Lnil)(listok.eval("(position-if (lambda (x) (eq x 1)) '(2 3))"))
    expect(Lint(2))(listok.eval("(position-if (lambda (x) (eq x 1)) '(2 3 1))"))
    expect(Lint(2))(listok.eval("(position-if (lambda (x) (eq x 1)) (vector 2 3 1))"))
    expect(Lint(2))(listok.eval("""(position-if (lambda (x) (eq x #\1)) "231")"""))
  }

  test("count") {
    expect(Lint(0))(listok.eval("(count 1 nil)"))
    expect(Lint(0))(listok.eval("(count 1 '(2 3))"))
    expect(Lint(2))(listok.eval("(count 1 '(1 3 1))"))
    expect(Lint(2))(listok.eval("(count 1 (vector 1 3 1))"))
    expect(Lint(1))(listok.eval("""(count #\1 "231")"""))

    expect(Lint(0))(listok.eval("(count-if (lambda (x) (eq x 1)) nil)"))
    expect(Lint(0))(listok.eval("(count-if (lambda (x) (eq x 1)) '(2 3))"))
    expect(Lint(1))(listok.eval("(count-if (lambda (x) (eq x 1)) '(2 3 1))"))
    expect(Lint(3))(listok.eval("(count-if (lambda (x) (eq x 1)) (vector 1 1 1))"))
    expect(Lint(1))(listok.eval("""(count-if (lambda (x) (eq x #\1)) "231")"""))
  }

  test("remove") {
    expect(Lnil)(listok.eval("(remove 1 nil)"))
    expect(Lnil)(listok.eval("(remove 1 '(1 1))"))
    expect(LL(Lint(2), Lint(3)))(listok.eval("(remove 1 '(2 3))"))
    expect(LL(Lint(2), Lint(3)))(listok.eval("(remove 1 '(2 3 1))"))
    expect(LV(Lint(2), Lint(3)))(listok.eval("(remove 1 (vector 2 3 1))"))
    expect(Lstring("23"))(listok.eval("""(remove #\1 "231")"""))

    expect(Lnil)(listok.eval("(remove-if (lambda (x) (eq x 1)) nil)"))
    expect(LL(Lint(2), Lint(3)))(listok.eval("(remove-if (lambda (x) (eq x 1)) '(2 3))"))
    expect(LL(Lint(2), Lint(3)))(listok.eval("(remove-if (lambda (x) (eq x 1)) '(2 3 1))"))
    expect(LV(Lint(2), Lint(3)))(listok.eval("(remove-if (lambda (x) (eq x 1)) (vector 2 3 1))"))
    expect(Lstring("23"))(listok.eval("""(remove-if (lambda (x) (eq x #\1)) "231")"""))
  }

  test("sort") {
    expect(Lnil)(listok.eval("(sort nil <)"))
    expect(LL(Lint(1), Lint(1)))(listok.eval("(sort '(1 1) <)"))
    expect(LL(Lint(1), Lint(2), Lint(3)))(listok.eval("(sort '(3 2 1) <)"))
    expect(LV(Lint(1), Lint(2), Lint(3)))(listok.eval("(sort (vector 2 3 1) <)"))

    expect(Lnil)(listok.eval("(sort nil (lambda (a b) (< a b)))"))
    expect(LL(Lint(1), Lint(1)))(listok.eval("(sort '(1 1) (lambda (a b) (< a b)))"))
    expect(LL(Lint(1), Lint(2), Lint(3)))(listok.eval("(sort '(3 2 1) (lambda (a b) (< a b)))"))
    expect(LV(Lint(1), Lint(2), Lint(3)))(listok.eval("(sort (vector 2 3 1) (lambda (a b) (< a b)))"))

    //expect(Lstring("123"))(listok.eval("""(sort "231" (lambda (a b) (< a b)))"""))
  }

  test("hashtable") {
    import scala.collection.mutable.{Map => MMap}
    val l = listok
    expect(Lhashtable(MMap.empty))(l.eval("(def ht (hash-table)) ht"))
    expect(Lint(11))(l.eval("(sethash :a ht 11)"))
    expect(Lint(11))(l.eval("(gethash :a ht)"))
    expect(Lnil)(l.eval("(gethash :b ht)"))
    expect(Ltrue)(l.eval("(checkhash :a ht)"))
    expect(Ltrue)(l.eval("(remhash :a ht)"))
    expect(Lnil)(l.eval("(checkhash :a ht)"))
    expect(Lnil)(l.eval("(gethash :a ht)"))
    expect(Lint(22))(l.eval("(sethash :a ht 22)"))
    expect(Lint(23))(l.eval("(gethash :a (maphash (lambda (k v) (+ 1 v)) ht))"))
  }

  test("hashmap") {
    val l = listok
    l.eval("(def hm (hashmap :a 1 :c 3 (pair :b 2)))")
    expect(Lint(1))(l.eval("(hm-get hm :a)"))
    expect(Lint(2))(l.eval("(hm-get hm :b)"))
    expect(Lnil)(l.eval("(hm-get hm :x nil)"))
    l.eval("(setf hm (cons (pair :x 42) hm))")
    expect(Lint(42))(l.eval("(hm-get hm :x)"))
    l.eval("(setf hm (hm-add hm :x 43))")
    expect(Lint(43))(l.eval("(hm-get hm :x)"))
    l.eval("(setf hm (hm-del hm :x))")
    expect(Lkeyword('fail))(l.eval("(hm-get hm :x :fail)"))
  }

  test("take") {
    expect(Lnil)(listok.eval("(take nil 0)"))
    expect(Lnil)(listok.eval("(take nil 1)"))
    expect(Lnil)(listok.eval("(take '(2 3) 0)"))
    expect(LL(Lint(2)))(listok.eval("(take '(2 3) 1)"))
    expect(LL(Lint(2),Lint(3)))(listok.eval("(take '(2 3) 2)"))
    expect(LL(Lint(2),Lint(3)))(listok.eval("(take '(2 3) 3)"))
  }

  test("drop") {
    expect(Lnil)(listok.eval("(drop nil 0)"))
    expect(Lnil)(listok.eval("(drop nil 1)"))
    expect(LL(Lint(2),Lint(3)))(listok.eval("(drop '(2 3) 0)"))
    expect(LL(Lint(3)))(listok.eval("(drop '(2 3) 1)"))
    expect(Lnil)(listok.eval("(drop '(2 3) 2)"))
    expect(Lnil)(listok.eval("(drop '(2 3) 3)"))
  }

  test("filter") {
    expect(Lnil){listok.eval("(filter (lambda (x) t) nil)")}
    expect(Lnil){listok.eval("(filter (lambda (x) nil) '(0 1 2))")}
    expect(Lnil){listok.eval("(filter (lambda (x) (> x 1)) nil)")}
    expect(LL(Lint(2), Lint(3))){listok.eval("(filter (lambda (x) (> x 1)) '(0 1 2 3))")}
  }

  test("partition") {
    expect(Lpair(Lnil, Lnil)){listok.eval("(partition (lambda (x) t) nil)")}
    expect(Lpair(LL(Lint(2), Lint(3)),LL(Lint(0), Lint(1))))
      {listok.eval("(partition (lambda (x) (> x 1)) '(0 2 1 3))")}
  }

  test("search") {
    expect(Lint(0))(listok.eval("(search nil nil)"))
    expect(Lint(0))(listok.eval("(search nil '(1))"))
    expect(Lnil)(listok.eval("(search '(1) nil)"))
    expect(Lint(1))(listok.eval("(search '(2) '(1 2 3))"))
    expect(Lint(3))(listok.eval("(search \"bar\" \"foobar\" )"))
    expect(Lnil)(listok.eval("(search '(2 4) '(1 2 3))"))
  }
}


object LV {
  def apply(xs: Lcommon*) = Lvector(scala.collection.mutable.ArraySeq(xs:_*))
}
