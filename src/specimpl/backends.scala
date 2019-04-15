// Copyright 2019 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package specimpl


import scala.collection.mutable
import firrtl._
import firrtl.ir._
import firrtl.util.BackendCompilationUtilities
import firrtl.CompilerUtils.getLoweringTransforms
import firrtl.transforms.BlackBoxSourceHelper
import java.io._

import scala.sys.process.{ProcessBuilder, ProcessLogger, _}
import scala.util.matching._


abstract class BackendResult
case class IsEquivalent() extends BackendResult
case class IsNotEquivalent(model: Map[String,BigInt]) extends BackendResult

trait CombinatorialChecker {
  def checkCombinatorial(prefix: String, c: Circuit, trigger: String = "trigger") : BackendResult
}

class MinimumFirrtlToVerilogCompiler extends Compiler {
  def emitter = new VerilogEmitter
  def transforms: Seq[Transform] = getLoweringTransforms(HighForm, LowForm) ++
      Seq(new MinimumLowFirrtlOptimization, new BlackBoxSourceHelper)
}

class YosysChecker extends CombinatorialChecker with BackendCompilationUtilities {
  private val compiler = new MinimumFirrtlToVerilogCompiler

  private def makeVerilog(testDir: File, circuit: Circuit): String = {
    //println("About to compile the following FIRRTL:")
    //println(circuit.serialize)
    // TODO: preserve annotations somehow ...
    val state = CircuitState(circuit, HighForm, Seq())
    val verilog = compiler.compileAndEmit(state)
    val file = new PrintWriter(s"${testDir.getAbsolutePath}/${circuit.main}.v")
    file.write(verilog.getEmittedCircuit.value)
    file.close()
    circuit.main
  }

  private def yosysModel(yosysOut: Seq[String]) : Map[String, BigInt] = {
    val signal : Regex = raw"\s+(\d+)\s+\\([a-zA-Z][a-zA-Z0-9_\.]+)\s+(\d+)\s+.+".r
    yosysOut.collect{
      case signal(time, name, value) => assert(time == "1"); name -> BigInt(value)
    }.toMap
  }

  // based on yosysExpectFailure
  private def yosysCheck(testDir: File, module: String, trigger: String): Tuple2[Boolean, Seq[String]] = {
    val scriptFileName = s"${testDir.getAbsolutePath}/yosys_script"
    val yosysScriptWriter = new PrintWriter(scriptFileName)
    yosysScriptWriter.write(
      s"""read_verilog ${testDir.getAbsolutePath}/$module.v
         |prep; proc; opt; memory; flatten
         |hierarchy -top $module
         |sat -verify -show-all -prove $trigger 0 -seq 1 $module"""
          .stripMargin)
    yosysScriptWriter.close()

    val resultFileName = testDir.getAbsolutePath + "/yosys_results"
    val command = s"yosys -s $scriptFileName" // #> new File(resultFileName)
    val buf = mutable.ArrayBuffer.empty[String]
    val logger = ProcessLogger({a => buf += a}, _ => ())
    val ret = command.!(logger)
    //println(s"yopsys returned $ret")
    (ret == 0, buf.toSeq)
  }

  def checkCombinatorial(prefix: String, c: Circuit, trigger: String = "trigger") : BackendResult = {
    val testDir = createTestDirectory(prefix + "_equivalence_test")
    println(s"Results dir: ${testDir.getAbsolutePath}")
    makeVerilog(testDir, c)
    val (success, stdout) = yosysCheck(testDir, c.main, trigger)
    if(success) { IsEquivalent() } else {
      IsNotEquivalent(yosysModel(stdout))
    }
  }
}