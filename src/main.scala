// Copyright 2019 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

import chisel3.RawModule
import chisel3.experimental.RunFirrtlTransform
import chisel3.internal.firrtl.Circuit
import firrtl.{ChirrtlForm, CircuitState, CustomTransformException, LowFirrtlCompiler, MiddleFirrtlCompiler, Parser, Transform}
import examples._
import specimpl.NotEquivalentException

object main {

  def main(args: Array[String]): Unit = {
    check_examples()
  }

  def check_examples() = {
    for((gen, correct) <- simple_alu.get_examples ++ max3.get_examples ++ popcount.get_examples) {
      val got_exception = try {
        check(gen)
        false
      } catch {
        case CustomTransformException(e : NotEquivalentException) => true
      }
      assert(got_exception != correct)
    }
  }

  def check_serv_alu() = {
    check(() => new serv_alu)
  }

  def check[T <: RawModule](gen: () => T) = {
    println
    val ir = chisel3.Driver.elaborate(gen)
    compile(ir)
    println
  }

  // adapted from chisel3.Driver.execute and firrtl.Driver.execute
  def compile(c: Circuit) = {
    val firrtl = Parser.parseString(chisel3.Driver.emit(c), Parser.UseInfo)
    val transforms = c.annotations
        .collect { case anno: RunFirrtlTransform => anno.transformClass }
        .distinct
        .filterNot(_ == classOf[Transform])
        .map { transformClass: Class[_ <: Transform] =>
          transformClass.newInstance()
        }
    //val compiler = new LowFirrtlCompiler()
    val compiler = new MiddleFirrtlCompiler()
    val annos = c.annotations.map(_.toFirrtl)
    val res = compiler.compile(CircuitState(firrtl, ChirrtlForm, annos), transforms)
  }
}
