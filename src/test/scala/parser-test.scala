package ru.listok.test

import ru.listok._
import org.scalatest.FunSuite


class ParserTest extends FunSuite {

  import Util.toLlist

  def parse1(text: String) = {
    Parser.read(text) match {
        case Right(s) =>
          if (s.isEmpty)
            Lnil
          else {
            if (s.length != 1)
              fail("a list with more than one elements: " + s)
            s.head
          }
        case Left(m) => fail(m)
      }
  }

  def parsefail(text: String) = {
    Parser.read(text) match {
      case Right(s) => fail("parse should fail = " + Util.pp(s))
      case Left(m) =>
      }
  }

  test ("nil") {
    expect(Lnil){parse1("nil")}
  }

  test ("t") {
    expect(Ltrue)(parse1("t"))
  }

  test ("char") {
    expect(Lchar('a'))  {parse1("""#\a""")}
    expect(Lchar('W'))  {parse1("""#\W""")}
    expect(Lchar('Ы'))  {parse1("""#\Ы""")}
    expect(Lchar('λ'))  {parse1("""#\λ""")}
    expect(Lchar('-'))  {parse1("""#\-""")}
    expect(Lchar('+'))  {parse1("""#\+""")}
    expect(Lchar('@'))  {parse1("""#\@""")}
    expect(Lchar('?'))  {parse1("""#\?""")}
    expect(Lchar('#'))  {parse1("""#\#""")}
    expect(Lchar('$'))  {parse1("""#\$""")}
    expect(Lchar('~'))  {parse1("""#\~""")}
    expect(Lchar('/'))  {parse1("""#\/""")}
    expect(Lchar('|'))  {parse1("""#\|""")}
    expect(Lchar(','))  {parse1("""#\,""")}
    expect(Lchar('\'')) {parse1("""#\'""")}
    expect(Lchar('`'))  {parse1("""#\`""")}
    expect(Lchar('('))  {parse1("""#\(""")}
    expect(Lchar(')'))  {parse1("""#\)""")}
    expect(Lchar('\\')) {parse1("""#\\""")}
    expect(Lchar('1'))  {parse1("""#\1""")}
    expect(Lchar('0'))  {parse1("""#\0""")}
    expect(Lchar('\n')) {parse1("""#\Newline""")}
    expect(Lchar(' '))  {parse1("""#\Space""")}
    expect(Lchar('\r')) {parse1("""#\Return""")}
    expect(Lchar('\t')) {parse1("""#\Tab""")}
    expect(Lchar('\f')) {parse1("""#\Page""")}
  }

  test ("int") {
    expect(Lint(0)) {parse1("0")}
    expect(Lint(42)) {parse1("42")}
    expect(Lint(-42)) {parse1("-42")}
    expect(Lint(2147483647)) {parse1("2147483647")}
    expect(Lint(-2147483648)) {parse1("-2147483648")}
  }

  test ("float") {
    expect(Lfloat(0.0)) {parse1("0.0")}
    expect(Lfloat(42.42)) {parse1("42.42")}
    expect(Lfloat(-42.42)) {parse1("-42.42")}
    expect(Lfloat(42.0)) {parse1("42.0")}
    expect(Lfloat(-42.0)) {parse1("-42.0")}
    expect(Lfloat(4200.0)) {parse1("42e2")}
    expect(Lfloat(-4200.0)) {parse1("-42e2")}
  }

  test ("long") {
    expect(Llong(2147483648l)) {parse1("2147483648")}
    expect(Llong(-2147483649l)) {parse1("-2147483649")}
    expect(Llong(Long.MaxValue)) {parse1("9223372036854775807")}
    expect(Llong(Long.MinValue)) {parse1("-9223372036854775808")}
  }

  test ("bignum") {
    expect(Lbignum(BigInt("9223372036854775808"))) {parse1("9223372036854775808")}
    expect(Lbignum(BigInt("-9223372036854775809"))) {parse1("-9223372036854775809")}
  }

  test ("string") {
    expect(Lstring("")) {parse1("\"\"")}
    expect(Lstring("hello")) {parse1("\"hello\"")}
    expect(Lstring("http://google.com")) {parse1(""""http://google.com"""")}
  }

  test ("symbol") {

    expect(Lsymbol('foo1)){parse1("foo1")}
    expect(Lsymbol(Symbol("-"))){parse1("-")}
    expect(Lsymbol(Symbol("*"))){parse1("*")}
    expect(Lsymbol(Symbol("+"))){parse1("+")}
    expect(Lsymbol(Symbol("/"))){parse1("/")}
    expect(Lsymbol(Symbol("="))){parse1("=")}
    expect(Lsymbol(Symbol("<"))){parse1("<")}
    expect(Lsymbol(Symbol(">"))){parse1(">")}
    expect(Lsymbol(Symbol("?"))){parse1("?")}
    expect(Lsymbol(Symbol("_"))){parse1("_")}
    expect(Lsymbol(Symbol("~"))){parse1("~")}
    expect(Lsymbol(Symbol("%"))){parse1("%")}
    expect(Lsymbol(Symbol("!"))){parse1("!")}
    expect(Lsymbol(Symbol("^"))){parse1("^")}
    expect(Lsymbol(Symbol("&"))){parse1("&")}
    expect(Lsymbol(Symbol("1foo"))){parse1("1foo")}
    expect(Lsymbol(Symbol("1foo:bar"))){parse1("1foo:bar")}
    expect(Lsymbol(Symbol("+bar+"))){parse1("+bar+")}
    expect(Lsymbol(Symbol("*bar*"))){parse1("*bar*")}
    expect(Lsymbol(Symbol("foo#"))){parse1("foo#")}
  }

  test ("list") {
    expect(Llist(Nil)) {parse1("()") }

    expect(toLlist(Lsymbol('foo), "bar", 1, Nil, true, '\n', Lkeyword('bar)) ) {
      parse1("(foo \"bar\" 1 nil t #\\Newline :bar)") }

    expect(toLlist(List(1, 2), List(3,4,List(true)))) {
      parse1("((1 2) (3 4 (t)))") }
  }

  test ("quote") {
    expect(Lquote(Lint(123))) { parse1("(quote 123)") }
    expect(Lquote(Lsymbol('abc))) { parse1("(quote abc)") }
    expect(Lquote(Lchar('a'))) { parse1("(quote #\\a)") }
    expect(Lquote(Ltrue)) { parse1("(quote t)") }

    expect(Lquote(Lint(123))) { parse1("'123") }
    expect(Lquote(Lsymbol('abc))) { parse1("'abc") }
    expect(Lquote(Lchar('a'))) { parse1("'#\\a") }
    expect(Lquote(Ltrue)) { parse1("'t") }

    expect(Lquote(LL(Lint(1), Lint(2)))) {parse1("'(1 2)") }
  }


  test ("lambda") {
      expect(
          Llambda(
            List(Lsymbol('x), Lsymbol('y)),
            List(LL(Lsymbol('+), Lint(1), Lsymbol('x), Lsymbol('y)))))
      { parse1("(lambda (x y) (+ 1 x y))") }
    }

  test ("greek lambda") {
      expect(
          Llambda(
            List(Lsymbol('x), Lsymbol('y)),
            List(LL(Lsymbol('+), Lint(1), Lsymbol('x), Lsymbol('y)))))
      { parse1("(λ (x y) (+ 1 x y))") }
    }

  test ("keyword") {
    expect(Lkeyword('abc)) {parse1(":abc")}
    expect(Lkeyword(Symbol("1"))) {parse1(":1")}
  }

  test("defmacro") {
    expect(Ldefmacro(
        'test,
        List(Lsymbol('b)),
        List(Lmacrobackquote(LL(Lsymbol('list), Lmacrocomma(Lsymbol('b)))))))
    {parse1("""(defmacro test (b) `(list ,b))""")}
  }


  test ("sform") {

    def expect(name: Symbol, args: List[Lcommon])(r: Lcommon) = r match {
      case Llist(l) =>
        l.head match {
          case sf: Lsform if sf.name == name =>
            if (l.tail != args)
              fail("fail parse sform (invalid args): " + args.toList)
           case _ => fail("fail parse sform (invalid sform): " + r.pp)
        }
      case _ => fail("fail parse sform: " + r.pp)
    }

    expect('def, List(Lsymbol('x))){parse1("(def x)")}
    expect('def, List(Lsymbol('x), Lint(1)))(parse1("(def x 1)"))
    expect('def, List(Lsymbol('def), Lint(1)))(parse1("(def def 1)"))
    expect('def, List(Lsymbol('defx), Lint(1)))(parse1("(def defx 1)"))
    expect('defconstant, List(Lsymbol('x)))(parse1("(defconstant x)"))
    expect('defconstant, List(Lsymbol('x), Lnil))(parse1("(defconstant x nil)"))
    expect('defun, List(Lsymbol('foo), LL(Lsymbol('x)), Lsymbol('x)))(parse1("(defun foo (x) x)"))
    expect('if, List(Lsymbol('x)))(parse1("(if x)"))
    expect('if, List(Lsymbol('x), Lnil))(parse1("(if x nil)"))
    expect('cond, List(Lsymbol('x)))(parse1("(cond x)"))
    expect('do, List(Lsymbol('x)))(parse1("(do x)"))
    expect('and, List(Lsymbol('x)))(parse1("(and x)"))
    expect('or, List(Lsymbol('x)))(parse1("(or x)"))
    expect('setf, List(Lsymbol('setf)))(parse1("(setf setf)"))
    expect('spawn, List(Lsymbol('worker), Llist(Nil)))(parse1("(spawn worker ())"))
    expect('match, List(Lsymbol('x)))(parse1("(match x)"))
  }

  test("regex") {

    expect("abc"){Parser.parseRegex("#/abc/").regex}
    expect("abc/"){Parser.parseRegex("""#/abc\//""").regex}
    expect("///"){Parser.parseRegex("""#/\/\/\//""").regex}
    expect("""(\b[0-9]+\.([0-9]+\b)?|\.[0-9]+\b)""")
      {Parser.parseRegex("""#/(\b[0-9]+\.([0-9]+\b)?|\.[0-9]+\b)/""").regex}
    expect("""\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,4}\b""")
      {Parser.parseRegex("""#/\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,4}\b/""").regex}

    expect(LL(
      Lsymbol('list), Lint(1), Lsymbol(Symbol("/")), Lkeyword(Symbol("x#")), Lstring("#//"),  Lregex("(abc)")))
      {parse1("""(list 1 / :x# "#//" #/(abc)/)""")}
  }

  test("spliced comma") {
    expect(Lmacrobackquote(LL(
        Lint(1),
        Lmacrocomma(Lsymbol('a), true),
        Lmacrocomma(Lsymbol('b), false),
        Lint(2))))
    {parse1("`(1 ,@a ,b 2)")}
  }

  test ("invalid char") {
    parsefail("#\\ab")
    parsefail("#\\ a")
  }

  test ("must fail") {
     parsefail(")")
  }

  test("escape") {
    expect(Lstring("\n\b\f\r\t"))  {parse1(""""\n\b\f\r\t"""")}
    expect(Lstring("\033"))  {parse1(""""\e"""")}
  }

  test("symbols and sforms") {
    expect(Lsymbol(Symbol("match-all")))  {parse1("match-all")}
    expect(Lsymbol(Symbol("match/all")))  {parse1("match/all")}
    expect(Lsymbol(Symbol("matchall")))  {parse1("matchall")}

    expect(Lsymbol(Symbol("do-1")))  {parse1("do-1")}
    expect(Lsymbol(Symbol("do/1")))  {parse1("do/1")}
    expect(Lsymbol(Symbol("do1")))  {parse1("do1")}

    expect(LL(Lsymbol(Symbol("match-all")))) {(parse1("(match-all)"))}
    expect(LL(Lsymbol(Symbol("match/all")))) {parse1("(match/all)")}
    expect(LL(Lsymbol(Symbol("matchall"))))  {parse1("(matchall)")}

    expect(LL(Lsymbol(Symbol("do-1"))))  {parse1("(do-1)")}
    expect(LL(Lsymbol(Symbol("do/1"))))  {parse1("(do/1)")}
    expect(LL(Lsymbol(Symbol("do1"))))  {parse1("(do1)")}
    expect(LL(Lsymbol(Symbol("all-do"))))  {parse1("(all-do)")}
    expect(LL(Lsymbol(Symbol("dodo"))))  {parse1("(dodo)")}

    expect(LL(Lsymbol(Symbol("defmarco-all")))) {parse1("(defmarco-all)")}
    expect(LL(Lsymbol(Symbol("lambda-all"))))   {parse1("(lambda-all)")}
    expect(LL(Lsymbol(Symbol("quote-all"))))    {parse1("(quote-all)")}
  }

  test("comment") {
    //try {
      expect(Lnil)(parse1(";\n"))
      expect(LL())(parse1("();\n"))
      expect(Lnil)(parse1(";()\n"))
      expect(LL(Lsymbol('+), Lint(1), Lint(2)))(parse1("(+  1 2);()\n"))
      expect(Lnil)(parse1(" ;()\n"))
      expect(Lnil)(parse1("\t;()\n"))
      expect(LL())(parse1("\t();\n"))
      expect(Lnil)(parse1(";()\n\t"))
      expect(Lnil)(parse1("\t;()\n\t"))
      expect(LL())(parse1("\t()\n;()\t\n\t"))
      expect(Lnil)(parse1(
        """
        ;1
        ;2
        """))
      expect(Lstring(";"))(parse1("\";\"\n"))
    expect(Lnil)(parse1(";\";\"\n"))
    //}catch {case e => println(e)}
  }
}

