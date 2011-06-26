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


object Interop extends ru.listok.builtin.Helpers {

  def guard[R] (env: Env, name: String)(f: => R) =
    try { f } catch {
      case e: ClassNotFoundException => throw SyntaxError("Interop class "+ name +" not found", env)
      case e: NoSuchFieldException => throw SyntaxError("Interop field "+name+" not found", env)
      case e: NoSuchMethodException => throw SyntaxError("Interop method "+name+" not found", env)
      case e: IllegalAccessException => throw SyntaxError("Interop access not allowed to " + name, env)
    }

  def getClassAndName(env: Env, name: String, ctor: Boolean): (Class[_], String) = guard(env, name) {
    if (ctor)
      (Class.forName(name), "")
    else {
      val ss = name.split('.')
      val cname = ss.init.mkString(".") // class name
      val mname = ss.last               // method name
      (Class.forName(cname), mname)
    }
  }

  def checkFunc (env: Env, clazz: Class[_], name: String, mod: Symbol) = guard(env, name) {
    mod match {
      case 'method =>
        clazz.getMethods.exists { m =>
          m.getName == name &&
          0 == (m.getModifiers & java.lang.reflect.Modifier.STATIC) }

      case 'static =>
        clazz.getMethods.exists { m =>
          m.getName == name &&
          0 != (m.getModifiers & java.lang.reflect.Modifier.STATIC) }

      case _ => true
    }
  }


  def callCtor(clazz: Class[_], l: List[Lcommon]): Lcommon = {

    if (l.isEmpty) {

      val ctors = clazz.getConstructors.filter { ctor => ctor.getParameterTypes.isEmpty }
      fromJava(ctors.head.newInstance())

    } else {

      val params = l.map(p => toJava(p.getAny))
      val types = params.map(_.getClass).toArray
      val ctors = clazz.getConstructors.filter { ctor => matchingTypes(ctor.getParameterTypes, types) }
      fromJava(ctors.head.newInstance(params: _*))
    }
  }

  def callFunc (obj: AnyRef, clazz: Class[_], name: String, l: List[Lcommon]): Lcommon = {

    // log(" for " + obj + " call " + clazz.getName + "." + name + " " + Util.pp(l))

    if (l.isEmpty) {

      val mm = clazz.getMethods.filter { m =>
        m.getName == name && m.getParameterTypes.isEmpty }

      if (mm.isEmpty) {
        val ff = clazz.getField(name)
        fromJava(ff.get(obj))
      }
      else
        fromJava(mm.head.invoke(obj))
    }
    else {
      val params = l.map(p => toJava(p.getAny))
      val types = params.map(_.getClass).toArray

      val mm = clazz.getMethods.filter { m =>
        m.getName == name &&
        matchingTypes(m.getParameterTypes, types) }

      fromJava(mm.head.invoke(obj, params:_*))
    }


  }

  def getField(env: Env, clazz: Class[_], name: String) = guard(env, name) {
    val ff = clazz.getField(name)
    fromJava(ff.get(null))
  }

  // this function taken from http://www.familie-kneissl.org/Members/martin/blog/reflection-from-scala-heaven-and-hell
  def matchingTypes(declared: Array[Class[_]], actual: Array[Class[_]]): Boolean = {
    declared.length == actual.length && (
      (declared zip actual) forall {
        case (declared, actual) => declared.isAssignableFrom(actual)
      })
  }

 def toJava(x: Any): AnyRef = x match {
    case x: Byte => Byte.box(x)
    case x: Char => Char.box(x)
    case x: Int => Int.box(x)
    case x: Long => Long.box(x)
    case x: Float => Float.box(x)
    case x: Double => Double.box(x)
    case x: Boolean => Boolean.box(x)
  //  case x: Unit => ()
    case x: BigInt => x.bigInteger
    case _ => x.asInstanceOf[AnyRef]
  }

 def fromJava(x: Any) = Util.toLcommon(x match {
    case x: java.lang.Byte      => x.byteValue
    case x: java.lang.Character => x.charValue
    case x: java.lang.Integer   => x.intValue
    case x: java.lang.Long      => x.longValue
    case x: java.lang.Float     => x.floatValue
    case x: java.lang.Double    => x.doubleValue
    case x: java.lang.Boolean   => x.booleanValue
    case x: java.lang.Void      => ()
    case x: java.lang.String    => x
    case x: java.math.BigInteger=> new BigInt(x)
    case _ => x //.asInstanceOf[AnyRef]
    })



  def func_interop(env: Env, args: List[Lcommon]): Lcommon = {
    notLess(env, args, 2)

    val jname = args(0).getString(env)
    val lname = args(1).getSymbol(env)

    val mod = if (args.length > 2)
      args(2).castKeyword(env).sym match {
        case s if s == 'static || s == 'method || s == 'constructor  || s == 'constant => s
        case err =>
          env.host.onwarning(env, "unknown modifier "+err+" on interop")
          'method
      }
    else
      'method

    val (clazz, mname) = getClassAndName(env, jname, mod == 'constructor)

    if (mod == 'constant) {

      val value = getField(env, clazz, mname)
      env.defineconst(lname, value)

    } else {

      if (!checkFunc(env, clazz, mname, mod))
        throw SyntaxError("Interop method "+jname+" not found", env)

      val fn = mod match {
          case 'static =>
            (env: Env, l: List[Lcommon]) => { guard(env, jname){callFunc(null, clazz, mname, l)} }

          case 'method =>
            (env: Env, l: List[Lcommon]) => {
              notLess(env, l, 1)
              guard(env, jname){callFunc(toJava(l.head.getAny), clazz, mname, l.tail)}
            }

          case 'constructor =>
            (env: Env, l: List[Lcommon]) => { guard(env, jname){callCtor(clazz, l)} }
        }

      env.define(lname, Lfunction(fn, lname))
    }

    Lsymbol(lname)
  }

  val all = List(
    Lfunction(func_interop, 'interop)
  )

}
