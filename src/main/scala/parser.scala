/*
	Listok is a dialect of LISP 
	Copyright (C) 2011 Konstantin Boukreev
	ru.kolyvan@gmail.com

	This file is part of Listok.

	Listok is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public
	License as published by the Free Software Foundation; either
	version 3 of the License, or (at your option) any later version.

	Listok is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
	Lesser General Public License for more details.

	You should have received a copy of the GNU Lesser General Public License
	along with this library. If not, see <http://www.gnu.org/licenses/>.
 */

package ru.listok

import util.parsing.combinator.{JavaTokenParsers, RegexParsers}

object Parser extends JavaTokenParsers {
//object Parser extends RegexParsers {

  override def stringLiteral: Parser[String] =
    ("\""+"""([^"\p{Cntrl}\\]|\\[\\/bfnrte]|\\u[a-fA-F0-9]{4})*"""+"\"").r  // added \e

  lazy val regex_keyword = """:([a-zA-Z_~%!=#<>\-\+\*\?\^\&\d])*""".r
  lazy val regex_symbol = """[a-zA-Z_~%!=<>\-\+\*\?\^\&\/\d\.]([a-zA-Z_~%!=<>:@#\-\+\*\?\^\&\/\d\.])*""".r

  lazy val lparen: Parser[String] = "("
  lazy val rparen: Parser[String] = ")"
  lazy val str_quote: Parser[String] = """quote\s+""".r
  lazy val char_quote: Parser[String] = "'"
  lazy val str_lambda: Parser[String] = "lambda" | "\u03BB" //"λ" greek small letter lamda
  lazy val str_defmacro: Parser[String] = "defmacro"

  lazy val sform_name = """(def|defun|defconstant|setf|if|cond|do|and|or|spawn|match|defstruct|assert|collect)\s+""".r

  //symbol or number
  lazy val symbol: Parser[Lcommon] = regex_symbol ^^ {
    case "t" => Ltrue
    case "nil" => Lnil
    case s => isNumber(s) match {
      case Some(x) => x
      case None => Lsymbol(Symbol(s))
    }
  }

  lazy val keyword: Parser[Lkeyword] = regex_keyword ^^ { s =>
    Lkeyword(Symbol(s.substring(1))) }

  lazy val sform: Parser[Llist] = lparen ~> sform_name ~ (form+)  <~ rparen ^^ {
    case name  ~ forms => Llist(SpecialForms.make(Symbol(name.trim)) :: forms)
  }

  lazy val char = new ParserChar

  lazy val string: Parser[Lstring] =  stringLiteral ^^ {s =>
    Lstring(Util.unescape(s.substring(1, s.length - 1)))
  }

  lazy val lambda_list: Parser[List[Lcommon]] = lparen ~> (symbol*) <~ rparen

  lazy val lambda: Parser[Llambda] = lparen ~> str_lambda ~> lambda_list ~ (form*) <~ rparen ^^ {
    case ll ~ body => Llambda(ll, body)
  }

  lazy val list: Parser[Llist] = lparen ~> (form*) <~ rparen ^^ { Llist(_) }

  lazy val quote: Parser[Lquote] = lparen ~> str_quote ~> form <~ rparen ^^ { Lquote(_) }

  lazy val comma: Parser[Lmacrocomma] = "," ~> form ^^ { Lmacrocomma(_, false) }

  lazy val commasplice: Parser[Lmacrocomma] = ",@" ~> form ^^ { Lmacrocomma(_, true) }

  lazy val backquote: Parser[Lmacrobackquote] = "`" ~> form ^^ { Lmacrobackquote(_) }

  lazy val regex = new ParseRegex

  lazy val form: Parser[Lcommon] =
    char | keyword | symbol | string | quote | lambda |
    sform | list | qform | comma | backquote | commasplice | regex

  lazy val qform: Parser[Lquote] = char_quote ~> form ^^ { Lquote(_) }


  lazy val macro_comma: Parser[Lmacrocomma] = "," ~> macro_form ^^ { Lmacrocomma(_, false) }

  lazy val macro_commasplice: Parser[Lmacrocomma] = ",@" ~> macro_form ^^ { Lmacrocomma(_, true) }

  lazy val macro_backquote: Parser[Lmacrobackquote] = "`" ~> macro_form ^^ { Lmacrobackquote(_) }

  lazy val macro_list: Parser[Llist] = lparen ~> (macro_form*) <~ rparen ^^ { Llist(_) }

  lazy val macro_sform: Parser[Llist] = lparen ~> sform_name ~ (macro_form+)  <~ rparen ^^ {
    case name  ~ forms => Llist(SpecialForms.make(Symbol(name.trim)) :: forms)
  }

  lazy val macro_form: Parser[Lcommon] =
    char | keyword | symbol | string | quote |
    macro_sform | macro_list | macro_comma | macro_commasplice | macro_backquote

  lazy val defmacro: Parser[Ldefmacro] = lparen ~> str_defmacro ~> regex_symbol ~ lambda_list ~ (macro_form*) <~ rparen ^^ {
      case ident ~ ll ~ body => Ldefmacro(Symbol(ident), ll, body)
   }

  //override protected val whiteSpace = """(\s+|;.*\n)""".r

  lazy val prog: Parser[List[Lcommon]]   = (defmacro | form)*

  //def read(text: String): Either[String, Llist] = {
  def read(text: String): Either[String, List[Lcommon]] = {
    parseAll(prog, text) match {
      case Success(form, _) => Right(form)
      case NoSuccess(msg, in) => Left(ppError(msg, in.pos))
    }
  }

  def ppError(msg: String, pos: scala.util.parsing.input.Position) =
    pos.line +": " + msg + "\n" + pos.longString


  class ParseRegex extends Parser[Lregex] {
    def apply(in: Input): ParseResult[Lregex] = {
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      if (start + 2 < source.length) { // #// minimum regex
        val s = source.subSequence(start, start+2)
        if (s != "#/")
          return Failure("`#/' expected but "+s+" found", in.drop(start - offset))
        var prev = ' '
        var i = start + 2
        val sb = new StringBuilder
        while (i < source.length) {
          val ch = source.charAt(i)

          if (ch == '/') {
            if (prev != '\\') {
              return Success(Lregex(sb.toString), in.drop(i + 1 - offset))
            }
            else {
              sb.deleteCharAt(sb.length - 1) // drop escape
            }
          }

          sb.append(ch)
          prev = ch
          i += 1
        }
      }
      Failure("`/' expected but end of source found", in.drop(start - offset))
    }
  }

  def parseRegex(text: String)  = {
    parseAll(new ParseRegex, text)  match {
      case Success(r, _) => r
      case NoSuccess(msg, in) => throw new RuntimeException(ppError(msg, in.pos))
    }
  }

  class ParserChar extends Parser[Lchar] {
    def apply(in: Input): ParseResult[Lchar] = {
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      if (start + 2 >= source.length)  // #\a minimum char
        return Failure("char expected but end of source found", in.drop(start - offset))

      val s = source.subSequence(start, start+2)
      if (s != "#\\")
        return Failure("`#\\' expected but "+s+" found", in.drop(start - offset))

      var i = start + 2
      val sb = new StringBuilder
      var ok = true
      while (ok && (i < source.length)) {
        val ch = source.charAt(i)
        if (ch == ' ')
          ok = false
        else if (sb.length > 0 && ch == ')')
          ok = false
        else {
          sb.append(ch)
          i += 1
        }
      }

      val tin = in.drop(i - offset)
      if (sb.length == 1) {
        return Success(Lchar(sb.charAt(0)), tin)
      }
      else {
        sb.toString match {
          case "Space" =>
            return Success(Lchar(' '), tin)
          case "Newline" | "Linefeed" =>
            return Success(Lchar('\n'), tin)
          case "Return" =>
            return Success(Lchar('\r'), tin)
          case "Tab" =>
            return Success(Lchar('\t'), tin)
          case "Backspace" =>
            return Success(Lchar('\b'), tin)
          case "Page" =>
            return Success(Lchar('\f'), tin)
          case _ =>
            return Failure("char expected but "+s+" found", in.drop(start - offset))
        }
      }
    }
  }

  lazy val isnumber = """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r
  lazy val isinteger = """-?\d+""".r

  //def isKeyword(s: String) = s.head == ':' match {
  //  case false => None
  //  case true => Lkeyword(Symbol(s.substring(1)))
  //}

  def isNumber(s: String): Option[Lcommon] = {
    //ru.listok.log("isnumber " + s + " m=" + isnumber.pattern.matcher(s).matches())
    if (isnumber.pattern.matcher(s).matches()) {
      if (isinteger.pattern.matcher(s).matches()) {
        if (s.length < 10)    // Int.MaxLength = 2147483647, length = 10
          Some(Lint(s.toInt))
        else if (s.length < 19) // Long.MaxLength = 9223372036854775807, length = 19
          Some(builtin.Numbers.toLnumeric(s.toLong))
        else
          Some(builtin.Numbers.toLnumeric(BigInt(s)))
      }
      else
        Some(Lfloat(s.toDouble))
    }
    else
      None
  }

}
