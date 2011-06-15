package ru.listok.test

import _root_.ru.listok._
import org.scalatest.FunSuite


class CommonTest extends FunSuite {

  def listok = new Listok  {
    var debug_ = false
    override def debug = debug_
    override def onload(env: Env, source: String) = source match {
      case "list" => Listok.load(env, new java.io.ByteArrayInputStream("(list 1 2 3)".getBytes))
      case _ => Lnil
    }
  }

  test("equal") {
    expect(Ltrue) {listok.eval("(eq nil nil)")}
    expect(Ltrue) {listok.eval("(eq 1 1)")}
    expect(Ltrue) {listok.eval("(= 1 1)")}
    expect(Ltrue) {listok.eval("(= 1.0 1.0)")}
    expect(Ltrue) {listok.eval("(= #\\a #\\a)")}
    expect(Ltrue) {listok.eval("(= \"abc\" \"abc\")")}
    expect(Ltrue) {listok.eval("(= '(1) '(1))")}
    expect(Ltrue) {listok.eval("(= '(vector 1 2) '(vector 1 2))")}

    expect(Lnil) {listok.eval("(eq nil t)")}
    expect(Lnil) {listok.eval("(eq 1 2)")}
    expect(Lnil) {listok.eval("(= 1 2)")}
    expect(Lnil) {listok.eval("(= 1.0 2.0)")}
    expect(Lnil) {listok.eval("(= #\\a #\\b)")}
    expect(Lnil) {listok.eval("(= \"abc\" \"xyz\")")}
    expect(Lnil) {listok.eval("(= '(1) '(2))")}
    expect(Lnil) {listok.eval("(= '(vector 1 2) '(vector 1 3))")}

    expect(Lnil) {listok.eval("(/= nil nil)")}
    expect(Lnil) {listok.eval("(/= 1 1)")}
    expect(Lnil) {listok.eval("(/= 1 1)")}
    expect(Lnil) {listok.eval("(/= 1.0 1.0)")}
    expect(Lnil) {listok.eval("(/= #\\a #\\a)")}
    expect(Lnil) {listok.eval("(/= \"abc\" \"abc\")")}
    expect(Lnil) {listok.eval("(/= '(1) '(1))")}
    expect(Lnil) {listok.eval("(/= '(vector 1 2) '(vector 1 2))")}

    expect(Ltrue) {listok.eval("(/= nil t)")}
    expect(Ltrue) {listok.eval("(/= 1 2)")}
    expect(Ltrue) {listok.eval("(/= 1 2)")}
    expect(Ltrue) {listok.eval("(/= 1.0 2.0)")}
    expect(Ltrue) {listok.eval("(/= #\\a #\\b)")}
    expect(Ltrue) {listok.eval("(/= \"abc\" \"xyz\")")}
    expect(Ltrue) {listok.eval("(/= '(1) '(2))")}
    expect(Ltrue) {listok.eval("(/= '(vector 1 2) '(vector 1 3))")}

  }

  test("convert") {
    expect(Lstring("")) {listok.eval("(to-str nil)")}
    expect(Lstring("true")) {listok.eval("(to-str t)")}
    expect(Lstring("42")) {listok.eval("(to-str 42)")}
    expect(Lstring("36.6")) {listok.eval("(to-str 36.6)")}
    expect(Lstring("A")) {listok.eval("(to-str #\\A)")}
    expect(Lstring("abc")) {listok.eval("(to-str \"abc\")")}
    expect(Lstring("FOO")) {listok.eval("(to-str :FOO)")}
    expect(Lstring("a1")) {listok.eval("(to-str (list #\\a 1))")}

    expect(Lint(0)) {listok.eval("(to-int nil)")}
    expect(Lint(1)) {listok.eval("(to-int t)")}
    expect(Lint(42)) {listok.eval("(to-int 42)")}
    expect(Lint(36)) {listok.eval("(to-int 36.6)")}
    expect(Lint(65)) {listok.eval("(to-int #\\A)")}
    expect(Lint(42)) {listok.eval("(to-int \"42\")")}

    expect(Lfloat(42f)) {listok.eval("(to-float 42)")}
    expect(Lfloat(36.6f)) {listok.eval("(to-float 36.6)")}
    expect(Lfloat(42f)) {listok.eval("(to-float \"42.0\")")}

    expect(LL(Lchar('1'),Lchar('2'))) {listok.eval("(to-list \"12\")")}
    expect(LL(Lint(1),Lint(2))) {listok.eval("(to-list (vector 1 2))")}
    expect(LL(Lpair(Lstring("k"), Lint(1)))) {listok.eval("(to-list (hashmap :k 1))")}
  }

   test("predicat") {
    expect(Ltrue) {listok.eval("(atom nil)")}
    expect(Ltrue) {listok.eval("(atom t)")}
    expect(Ltrue) {listok.eval("(atom 1)")}
    expect(Lnil) {listok.eval("(atom '(1))")}

    expect(Ltrue) {listok.eval("(listp nil)")}
    expect(Lnil) {listok.eval("(listp t)")}
    expect(Lnil) {listok.eval("(listp 1)")}
    expect(Ltrue) {listok.eval("(listp '(1))")}

    expect(Ltrue) {listok.eval("(null nil)")}
    expect(Ltrue) {listok.eval("(null ())")}
    expect(Lnil) {listok.eval("(null t)")}
    expect(Lnil) {listok.eval("(null 1)")}
    expect(Lnil) {listok.eval("(null '(1))")}

    expect(Lnil) {listok.eval("(numberp nil)")}
    expect(Lnil) {listok.eval("(numberp t)")}
    expect(Ltrue) {listok.eval("(numberp 1)")}
    expect(Ltrue) {listok.eval("(numberp 1.0)")}

    expect(Lnil) {listok.eval("(keywordp nil)")}
    expect(Lnil) {listok.eval("(keywordp t)")}
    expect(Lnil) {listok.eval("(keywordp 1)")}
    expect(Ltrue) {listok.eval("(keywordp :A)")}

    expect(Lnil) {listok.eval("(symbolp nil)")}
    expect(Lnil) {listok.eval("(symbolp t)")}
    expect(Lnil) {listok.eval("(symbolp 1)")}
    expect(Ltrue) {listok.eval("(symbolp 'A)")}
  }

  test("def") {
    expect(Lint(42)) {listok.eval("(def xxx 42) xxx")}
    expect(Lint(49)) {
      listok.eval(
      """
      (def xxx (+ 40 2))
      (def zzz 7)
      (+ xxx zzz)
      """)
    }
  }

  test("setf") {
    expect(Lint(8)) {
      listok.eval(
      """
      (def xxx 3)
      (setf xxx (+ 1 7))
      """)
    }
  }

  test("lambda") {
    expect(Lint(19)) {
      listok.eval(
      """
      (def xxx 11)
      ((lambda (x) (setf x (+ 1 x)) (+ x xxx)) 7)
      """)
    }
    expect(Lint(1)){listok.eval("((lambda () 1))")}
    intercept[TypeError]{listok.eval("(((lambda () 1)))")}
  }

  test("IF") {
    expect(Lint(1)) {listok.eval("""(if t 1 0)""")}
    expect(Lint(0)) {listok.eval("""(if nil 1 0)""")}
    expect(Lint(3)) {listok.eval("""(if (eq 1 1) (+ 1 2) 0)""")}
    expect(Lint(4)) {listok.eval("""(if (eq 1 0) (+ 1 2) (* 2 2))""")}
    expect(Lint(1)) {listok.eval("""(if t 1)""")}
    expect(Lnil)    {listok.eval("""(if nil 1)""")}

    expect(Lint(2)) {listok.eval(
      """
      (def xnil nil)
      (if xnil 1 2)
      """)}

    expect(Lint(1)) {listok.eval(
      """
      (def xtrue t)
      (if xtrue 1 2)
      """)}
  }

  test("symbols") {
    expect(Lfloat(112f)) {
      listok.eval(
      """
      (def lambda1 111)
      (+ lambda1 1.0)
      """)
    }

    expect(Lfloat(112f)) {
      listok.eval(
      """
      (def quote1 111)
      (+ 1.0 quote1)
      """)
    }
  }

  test("recursion") {
    expect(Lint(4)) {
      listok.eval(
      """
      (def f (lambda (x z)
          (if (eq x 0)
            z
            (f (- x 1) (+ z 1)))))
      (f 4 0)
      """)
    }

    expect(Lint(1)) {listok.eval("(def fact (lambda (n)(if (< n 2) 1 (* n (fact (- n 1))))))(fact 0)")}
    expect(Lint(1)) {listok.eval("(def fact (lambda (n)(if (< n 2) 1 (* n (fact (- n 1))))))(fact 1)")}
    expect(Lint(2)) {listok.eval("(def fact (lambda (n) (if (< n 2) 1 (* n (fact (- n 1))))))(fact 2)")}
    expect(Lint(6)) {listok.eval("(def fact (lambda (n) (if (< n 2) 1 (* n (fact (- n 1))))))(fact 3)")}
  }

  test("closure") {
    val text = """
    (def make-adder (lambda (n) (lambda (x) (+ x n))))
    (def add3 (make-adder 3))
    (add3 10)
    """
    expect(Lint(13)) {listok.eval(text)}
  }

  test ("cond") {
    expect(Lint(2))(listok.eval("(cond (nil 1)(t 2))"))
    expect(Lint(1))(listok.eval("(cond (t 1)(nil 2))"))
    expect(Ltrue)(listok.eval("(cond (t))"))
    expect(Lnil)(listok.eval("(cond (nil))"))
    expect(Lint(55))(listok.eval("(def xxx 5)(cond ((eq xxx 3) 33)(t 55))"))
    expect(Lint(55))(listok.eval("(def xxx 5)(cond ((eq xxx 3) 33)((eq xxx 5) 11 22 55)(t 0))"))
  }

  test("AND") {
    expect(Ltrue)   {listok.eval("(and t)")}
    expect(Lnil)    {listok.eval("(and nil)")}
    expect(Lint(1)) {listok.eval("(and 1)")}
    expect(Lint(2)) {listok.eval("(and t 2)")}
    expect(Lnil)    {listok.eval("(and nil 2)")}
    expect(Lnil)    {listok.eval("(and 3 nil)")}
    expect(Lnil)    {listok.eval("(and nil 1 t)")}
    expect(Lint(1)) {listok.eval("(and 3 2 1)")}
  }

  test("OR") {
    expect(Ltrue)   {listok.eval("(or t)")}
    expect(Lnil)    {listok.eval("(or nil)")}
    expect(Lnil)    {listok.eval("(or () ())")}
    expect(Lint(1)) {listok.eval("(or 1)")}
    expect(Ltrue)   {listok.eval("(or t 2)")}
    expect(Lint(2)) {listok.eval("(or nil 2)")}
    expect(Lint(3)) {listok.eval("(or 3 nil)")}
    expect(Lint(1)) {listok.eval("(or nil 1 t)")}
    expect(Lint(3)) {listok.eval("(or 3 2 1)")}
  }

  test("NOT") {
    expect(Lnil)    {listok.eval("(not t)")}
    expect(Ltrue)   {listok.eval("(not nil)")}
    expect(Ltrue)   {listok.eval("(not ())")}
    expect(Lnil)    {listok.eval("(not 1)")}
  }

  test("defun") {
    expect(Lint(3)){listok.eval("(defun f (x y) (+ x y))(f 1 2)")}

    intercept[SyntaxError] { listok.eval("(defun f )") }
    intercept[SyntaxError] { listok.eval("(defun f x)") }
    intercept[SyntaxError] { listok.eval("(defun f x 1)") }
    intercept[SyntaxError] { listok.eval("(defun (f) x 1)") }

    expect(Lsymbol('f)){listok.eval("(defun f (x) 1)")}
    expect(Lint(5)){listok.eval("(defun f (x) (+ x 3)) (f 2)")}
  }

  test("progn"){
    expect(Lnil)(listok.eval("(progn)"))
    expect(Lint(1))(listok.eval("(progn 1)"))
    expect(Lint(2))(listok.eval("(progn 1 2)"))
    expect(Lint(4))(listok.eval("(progn 1 3 (+ 1 3))"))
  }

  test("let") {

    expect(Lnil){listok.eval("(let ())")}
    expect(Lnil){listok.eval("(let (x))")}
    expect(Lnil){listok.eval("(let (x) x)")}
    expect(Lint(1)){listok.eval("(let ((x 1)) x)")}
    expect(LL(Lint(1), Lnil)){listok.eval("(let ((x 1) z) (list x z))")}
    expect(Lint(3)){listok.eval("(let ((x 1) (z 2)) (+ x z))")}
    expect(Ltrue){listok.eval("(let () t)")}
    expect(Lint(9)){listok.eval("(let ((x 3) (y (+ 1 5))) (+ x y))")}

    intercept[SyntaxError]{listok.eval("(let (x 1) x)")}
    intercept[SyntaxError]{listok.eval("(let)")}
    intercept[SyntaxError]{listok.eval("(let x)")}
  }

  test("keyword") {
    expect(Llist(List(Lkeyword(Symbol("1")),Lkeyword('a),Lkeyword('b2)))){
      listok.eval("(list :1 :a :b2)")}
    expect(Ltrue){listok.eval("(eq :a :a :a)")}
    expect(Lnil){listok.eval("(eq :a :a :b)")}
  }

  test("quote '"){
    expect(Llist(List(Lint(1), Lint(2)))){
      listok.eval("'(1 2)")
    }
  }

  test("apply") {
    expect(Lint(3)){listok.eval("(apply '+ '(1 2))")}
    expect(Lint(12)){listok.eval("(apply (lambda (x) (+ x 10)) (list 2))")}
    expect(Lint(12)){listok.eval("(apply (lambda (x) (+ x 10)) '(2))")}
    expect(Lint(9)){listok.eval("""
      (def f (lambda (x y z) (+ x y z)))
      (def l (list 2 3 4))
      (apply f l)
      """
      )}
    expect(Lint(3)){listok.eval("(apply '+ '(1 2))")}
    expect(Lint(3)){listok.eval("(def f '+)(apply f '(1 2))")}
    expect(Lint(1)){listok.eval("(apply 'eval '(1))")}

    intercept[TypeError]{listok.eval("(apply '+ 1)")}
  }

  test("eval") {
    expect(Lnil){listok.eval("(eval nil)")}
    expect(Lnil){listok.eval("(eval ())")}
    expect(Ltrue){listok.eval("(eval t)")}
    expect(Lint(1)){listok.eval("(eval 1)")}
    expect(Lint(1)){listok.eval("(eval '1)")}
    expect(Lint(1)){listok.eval("(eval ''1)")}
    expect(Lquote(Lint(1))){listok.eval("(eval '''1)")}
    expect(Lkeyword('a)){listok.eval("(eval :a)")}
    expect(Lstring("a")){listok.eval("""(eval "a")""")}

    intercept[UnboundSymbolError] {listok.eval("""(eval (quote a))""")}
    intercept[UnboundSymbolError] {listok.eval("""(eval 'a)""")}
    intercept[UnboundSymbolError]{listok.eval("(eval a)")}
    intercept[TypeError] {listok.eval("(eval '(1 2))")}

    intercept[TypeError] {listok.eval("(eval (list 1 2))")}

    expect(LL(Lint(1), Lint(2))){listok.eval("(eval '(list 1 2))")}
    expect(Lint(3)){listok.eval("(eval (+ 1 2))")}

    expect(Lint(9)){listok.eval("""
      (def f (lambda (x y z) (+ x y z)))
      (eval (f 2 3 4))
      """
      )}

    listok.eval("(eval (lambda (x) x))") match {
      case f: Lfunction =>
      case err => fail("expected function but got: " + err.pp)
    }

    listok.eval("""
      (def f (lambda (x y z) (+ x y z)))
      (eval f)
      """) match {
      case f: Lfunction =>
      case err => fail("expected function but got: " + err.pp)
    }

    expect(Lint(1)){listok.eval("(def x 1) (eval x)")}
    expect(Lint(3)){listok.eval("(def x 1)(eval (setf x 3))")}
    expect(Lint(6)){listok.eval("(def x '(+ 1 2 3)) (eval x)")}
  }

  test("load") {
    expect(LL(Lint(1), Lint(2), Lint(3))){listok.eval("(load :list)")}
  }

  test("error") {
    intercept[ScriptError] {listok.eval("(error :test)")}
  }

  test("&rest arg") {
    val l = listok
    l.eval(
      "(defun f0 (&rest args) args)" +
      "(defun f1 (x &rest args) (cons x args))")

    expect(Lnil){l.eval("(f0)")}
    expect(LL(Lint(1))){l.eval("(f0 1)")}
    expect(LL(Lint(1), Lint(2))){l.eval("(f0 1 2)")}

    intercept[SyntaxError]{l.eval("(f1)")}
    expect(LL(Lint(1))){l.eval("(f1 1)")}
    expect(LL(Lint(1), Lint(2))){l.eval("(f1 1 2)")}
    expect(LL(Lint(1), Lint(2), Lint(3))){l.eval("(f1 1 2 3)")}
  }

  test("defstruct") {
    val l = listok
    expect(Lsymbol('yoda)){l.eval("(defstruct yoda foo bar)")}
    expect(Lsymbol('yoda1)){l.eval("(def yoda1 (make-yoda 3.14 42))")}
    expect(Lfloat(3.14f)){l.eval("(yoda-foo yoda1)")}
    expect(Lint(42)){l.eval("(yoda-bar yoda1)")}
    expect(Lint(37)){l.eval("(set-yoda-bar yoda1 37)")}
    expect(Lint(37)){l.eval("(yoda-bar yoda1)")}
    // l.eval("(print yoda1)")
  }

  test("curry") {
    expect(Lint(3)){listok.eval(
    """
    (def add1 (curry + 1))
    (add1 2)
    """)}
  }

  test("assert") {
    val l = listok
    l.debug_ = true
    intercept[ScriptAssert]{l.eval("""(assert nil "test assert")""")}
    l.debug_ = false
    expect(Lnil){l.eval("""(assert nil "test assert")""")}
  }
}

