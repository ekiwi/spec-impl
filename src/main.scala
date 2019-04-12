// Copyright 2019 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

import chisel3._
import chisel3.experimental.{RawModule, RunFirrtlTransform}
import chisel3.util._
import chisel3.internal.firrtl.Circuit
import firrtl.{ChirrtlForm, CircuitState, LowFirrtlCompiler, Parser, Transform}


object main {

  def main(args: Array[String]): Unit = {
    check(() => new SimpleALU(correct = true))
    check(() => new SimpleALU(correct = false))
    //check(() => new mf8_alu())
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
    val compiler = new LowFirrtlCompiler()
    val annos = c.annotations.map(_.toFirrtl)
    val res = compiler.compile(CircuitState(firrtl, ChirrtlForm, annos), transforms)
  }
}
