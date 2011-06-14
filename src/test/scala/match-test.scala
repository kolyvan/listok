package ru.listok.test

import _root_.ru.listok._
import org.scalatest.FunSuite
import java.util.regex.Pattern


class MatchTest extends FunSuite {

  def listok = new Listok

  def testFound(v: Lcommon):Unit = testFound(v,v)

  def testFound(p:Lcommon, v: Lcommon):Unit = testFound(v, p, v)

  def testFound(r: Lcommon, p: Lcommon, v: Lcommon):Unit =
      expect(MatchValueResult(r)) {PatternMatching.runMatch(listok.root, p, v)}

  def testNotFound(p:Lcommon, v: Lcommon) =
      expect(MatchNotFound) {PatternMatching.runMatch(listok.root, p, v)}


  test("simple match") {

    testFound(Lnil)
    testFound(Ltrue)
    testFound(Lint(1))
    testFound(Lfloat(1.0f))
    testFound(Lquote(Lsymbol('A)), Lsymbol('A))
    testFound(Lkeyword('A))
    testFound(Lchar('A'))
    testFound(Lstring("ABC"))

    testNotFound(Lnil, Ltrue)
    testNotFound(Ltrue, Lnil)
    testNotFound(Lint(1), Lint(2))
    testNotFound(Lint(1), Lnil)
    testNotFound(Lfloat(1.0f), Lfloat(2.0f))
    testNotFound(Lfloat(1.0f), Lnil)
    testNotFound(Lquote(Lsymbol('A)), Lsymbol('B))
    testNotFound(Lquote(Lsymbol('A)), Lnil)
    testNotFound(Lkeyword('A), Lkeyword('B))
    testNotFound(Lkeyword('A), Lnil)
    testNotFound(Lchar('A'), Lchar('B'))
    testNotFound(Lchar('A'), Lnil)
    testNotFound(Lstring("ABC"), Lstring("ABCD"))
    testNotFound(Lstring("ABC"), Lnil)
  }

  test("func match") {

    testFound(Lsymbol('atom), Ltrue)
    testFound(Ltrue, Lsymbol('keywordp), Lkeyword('A))
    //testFound(LL(Lsymbol('vector), Lint(1), Lint(2)), LV(Lint(1),Lint(2)))

    testFound(Lint(1), Lsymbol('head), LL(Lint(1), Lint(2)))
    testFound(LL(Lint(2)), Lsymbol('tail), LL(Lint(1), Lint(2)))

    testNotFound(Lsymbol('atom), Llist(Nil))
    testNotFound(Lsymbol('keywordp), Lint(1))
   // testNotFound(Lsymbol('head), Lnil)

    expect(MatchSymbolResult('x,LL(Lint(1),Lint(2),Lint(3)))) {
      PatternMatching.runMatch(listok.root,
        LL(Lsymbol('head), Lsymbol('x)),
        LL(Lint(1),Lint(2),Lint(3)))
    }

    expect(MatchSymbolResult('xs,LL(Lint(1),Lint(2),Lint(3)))) {
      PatternMatching.runMatch(listok.root,
        LL(Lsymbol('tail), Lsymbol('xs)),
        LL(Lint(1),Lint(2),Lint(3)))
    }

    expect(MatchNotFound) {
      PatternMatching.runMatch(listok.root,
        LL(Lsymbol('head), Lsymbol('x)),
        Lnil)
    }

  }

  test("list match") {

    // (list xs)
    expect(MatchSeqResult(List(('xs->LL(Lint(1), Lint(2),Lint(3)))))) {
      PatternMatching.runMatch(listok.root,
        LL(Lsymbol('list), Lsymbol('xs)),
        LL(Lint(1),Lint(2),Lint(3)))
    }

    // (list x xs)
    expect(MatchSeqResult(List(('x->Lint(1)), ('xs->LL(Lint(2),Lint(3)))))) {
      PatternMatching.runMatch(listok.root,
        LL(Lsymbol('list), Lsymbol('x), Lsymbol('xs)),
        LL(Lint(1),Lint(2),Lint(3)))
    }

    // (list 1 xs)
    expect(MatchSeqResult(List(('xs->LL(Lint(2),Lint(3)))))) {
      PatternMatching.runMatch(listok.root,
        LL(Lsymbol('list), Lint(1), Lsymbol('xs)),
        LL(Lint(1),Lint(2),Lint(3)))
    }

    // (list x 2 xs)
    expect(MatchSeqResult(List(('x->Lint(1)), ('xs->LL(Lint(3)))))) {
      PatternMatching.runMatch(listok.root,
        LL(Lsymbol('list), Lsymbol('x), Lint(2), Lsymbol('xs)),
        LL(Lint(1),Lint(2),Lint(3)))
    }

    // (list 1 2 3)
    expect(MatchSeqResult(Nil)) {
      PatternMatching.runMatch(listok.root,
        LL(Lsymbol('list), Lint(1), Lint(2), Lint(3)),
        LL(Lint(1),Lint(2),Lint(3)))
    }

  // (list x xs)
    expect(MatchNotFound) {
      PatternMatching.runMatch(listok.root,
        LL(Lsymbol('list), Lint(3), Lsymbol('xs)),
        LL(Lint(1),Lint(2),Lint(3)))
    }

    // (list 1 2 3)
    expect(MatchNotFound) {
      PatternMatching.runMatch(listok.root,
        LL(Lsymbol('list), Lint(1), Lint(2), Lint(4)),
        LL(Lint(1),Lint(2),Lint(3)))
    }
  }

  test("pair match") {
    testFound(Lpair(Lint(1), Lint(2)))
    testNotFound(Lpair(Lint(1), Lint(2)), Lpair(Lint(1), Lint(3)))

    testFound(
      Lpair(Lint(1), Lint(2)),
      LL(Lsymbol('pair), Lint(1), Lint(2)),
      Lpair(Lint(1), Lint(2)))

    testNotFound(
      LL(Lsymbol('pair), Lint(1), Lint(2)),
      Lpair(Lint(1), Lint(3)))

    expect(MatchSymbolResult('a,Lint(1))) {
      PatternMatching.runMatch(listok.root,
        LL(Lsymbol('pair), Lsymbol('a), Lint(2)),
        Lpair(Lint(1), Lint(2)))
      }

    expect(MatchSymbolResult('b,Lint(2))) {
      PatternMatching.runMatch(listok.root,
        LL(Lsymbol('pair), Lint(1), Lsymbol('b)),
        Lpair(Lint(1), Lint(2)))
      }
    expect(MatchSeqResult(List('a -> Lint(1), 'b -> Lint(2)))) {
      PatternMatching.runMatch(listok.root,
        LL(Lsymbol('pair), Lsymbol('a), Lsymbol('b)),
        Lpair(Lint(1), Lint(2)))
      }

    expect(MatchNotFound) {
      PatternMatching.runMatch(listok.root,
        LL(Lsymbol('pair), Lsymbol('a), Lint(3)),
        Lpair(Lint(1), Lint(2)))
      }
  }

  test("regex") {


    expect(Lstring("foo")){builtin.Regex.find(Pattern.compile("(F|f)oo"), "foo and bar")}
    expect(Lnil){builtin.Regex.find(Pattern.compile("(F|f)oo"), "yoda yoda")}
    expect(LL(Lstring("yoda"), Lstring("yoda"))){builtin.Regex.findall(Pattern.compile("yoda"), "yoda yoda")}
    expect(LL(Lstring("abc"), Lstring("756"))){builtin.Regex.matches(Pattern.compile("(\\w+)\\s+(\\d+)"), "abc 756")}
    expect(LL(Lstring("yoda"), Lstring("yoda"))){builtin.Regex.split(Pattern.compile("\\s+"), "yoda yoda")}
    expect(Lstring("foo foo")){builtin.Regex.replace(Pattern.compile("\\w+"), "yoda yoda", "foo")}

    expect(Lstring("foo")){listok.eval("(regex-find #/(F|f)oo/ \"foo and bar\")")}
    expect(Lnil){listok.eval("(regex-find #/(F|f)oo/ \"yoda yoda\")")}
    expect(LL(Lstring("yoda"), Lstring("yoda")))
      {listok.eval("(regex-find-all #/yoda/ \"yoda yoda\")")}
    expect(LL(Lstring("abc"), Lstring("756")))
      {listok.eval("""(regex-matches #/(\w+)\s+(\d+)/ "abc 756")""")}
    expect(LL(Lstring("yoda"), Lstring("yoda")))
      {listok.eval("""(regex-split #/\s+/ "yoda yoda")""")}
    expect(Lstring("foo foo"))
      {listok.eval("""(regex-replace #/\w+/ "yoda yoda" "foo")""")}
  }

  test("match") {
    val p =
    """
    (def foo 112)
    (def bar 113)
    ;(def x :xxx)
    (defun f (abc)
     (match abc
      (42 :int)
      (`,foo :foo)
      (bar :bar)
      (3.14 :float)
      (:ok :keyword)
      ("abc" :string)
      ('A :symbol)
      ;((regex "756" n) n)
      (stringp :stringp)
      ((numberp n) (+ n 1))
      ((list 'A 'B) :listsym)
      ;((list `,foo `,bar 'A) :foobar)
      (`(list ,foo ,bar 'A) :foobar)
      ;((list ) :list0)
      ;((list xs) xs)
      ;((list x xs) x)
      ;((list 1 xs) xs)
      ((list 1 x xs) x)
      ((pair 1 2) :pair)
      ((pair 11 b) b)
      ((pair `,foo b) b)
      (`(pair a ,bar) a)
      ((pair a 'A) a)
      ((pair a b) ( + a b))
      ))
    """

    val l = listok
    l.eval(p)

    expect(Lnil) {l.eval("(f t)")}
    expect(Lkeyword('int)) {l.eval("(f 42)")}
    expect(Lkeyword('foo)) {l.eval("(f 112)")}
    expect(Lkeyword('bar)) {l.eval("(f 113)")}
    expect(Lkeyword('float)) {l.eval("(f 3.14f)")}
    expect(Lkeyword('keyword)) {l.eval("(f :ok)")}
    expect(Lkeyword('string)) {l.eval("(f \"abc\")")}
    expect(Lkeyword('symbol)) {l.eval("(f 'A)")}
    expect(Lkeyword('stringp)) {l.eval("(f \"foo\")")}
    expect(Lint(28)) {l.eval("(f 27)")}
    expect(Lkeyword('listsym)) {l.eval("(f (list 'A 'B))")}
    expect(Lkeyword('foobar)) {l.eval("(f (list 112 113 'A))")}
   // expect(Lkeyword('list0)) {l.eval("(f (list 1 2 3 4))")}
   // expect(LL(Lint(1), Lint(2))) {l.eval("(f (list 1 2))")}
   // expect(Lint(1)) {l.eval("(f (list 1 2))")}
   // expect(LL(Lint(2), Lint(3))) {l.eval("(f (list 1 2 3))")}
    expect(Lint(2)) {l.eval("(f (list 1 2 3))")}
    expect(Lkeyword('pair)) {l.eval("(f (pair 1 2))")}
    expect(Lint(2)) {l.eval("(f (pair 11 2))")}
    expect(Lint(3)) {l.eval("(f (pair 112 3))")}
    expect(Lint(4)) {l.eval("(f (pair 4 113))")}
    expect(Lint(5)) {l.eval("(f (pair 5 'A))")}
    expect(Lint(9)) {l.eval("(f (pair 4 5))")}
    //expect(Lstring("756")) {l.eval("(f \"756\")")}


  }


  test("match regex") {
    val p =
    """
    (def foo 112)
    (def bar 113)
    ;(def z #/([0-9]+)/)
    (def z #/(\d+)/)
    (defun f (x)
     (match x
      ;(z x)
      ;(#/(\d+)/ x)
      ((z x) x)
      ;((#/(\d+)/ x y) x)
      ))
    """

    val l = listok
    l.eval(p)
    expect(Lstring("756")) {l.eval("(f \"756\")")}
    expect(Lnil) {l.eval("(f \"abc\")")}
  }


  test("match regex 2") {
    val p =
    """
    (defun f (s)
      (match s
        ((#/(\w+):(\d+)/ name id)
          (pair name (to-int id)))))
    """

    val l = listok
    l.eval(p)
    expect(Lpair(Lstring("foo"), Lint(42))) {l.eval("(f \"foo:42\")")}
    expect(Lnil) {l.eval("(f \"foo:abc\")")}
  }

}
