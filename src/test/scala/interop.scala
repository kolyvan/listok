package ru.listok.test

import _root_.ru.listok._
import org.scalatest.FunSuite
import java.io.File
import java.net.InetAddress

class InteropTest extends FunSuite with Helper {

  test("method") {
    val l = listok

    expect(Lsymbol(Symbol("to-upper"))) {
      l.eval("(interop \"java.lang.String.toUpperCase\" 'to-upper)") }

    expect(Lstring("TEST")) {l.eval("(to-upper \"test\")")}
  }

  test("static") {
    val l = listok

    expect(Lsymbol(Symbol("sys/prop"))) {
      l.eval("(interop \"java.lang.System.getProperty\" 'sys/prop :static)") }

    expect(Lstring(System.getProperty("os.name"))) {
      l.eval("(sys/prop \"os.name\")")}
  }

  test("constant") {
    val l = listok

    expect(Lsymbol('pi)) {
      l.eval("(interop \"java.lang.Math.PI\" 'pi :constant)") }

    expect(Lint(314)) {
      l.eval("(to-int (* 100 pi))") }
  }

  test("constructor") {
    val l = listok

    expect(Lsymbol(Symbol("make-file"))) {
      l.eval("(interop \"java.io.File\" 'make-file :constructor)") }

    expect(Lsymbol(Symbol("file/path"))) {
      l.eval("(interop \"java.io.File.getAbsolutePath\" 'file/path)") }

    expect(Lstring(new File(".").getAbsolutePath)) {
      l.eval(""" (file/path (make-file ".")) """)}
  }

  test("more") {
    val l = listok

    expect(Lsymbol(Symbol("inetaddr/byname"))) {
      l.eval("(interop \"java.net.InetAddress.getByName\" 'inetaddr/byname :static)") }

    expect(Lstring(InetAddress.getByName("localhost").toString)) {
      l.eval(""" (to-str (inetaddr/byname "localhost")) """)}
  }

}