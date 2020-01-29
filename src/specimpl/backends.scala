// Copyright 2019 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package specimpl


import firrtl._
import firrtl.ir._
import firrtl.util.BackendCompilationUtilities
import firrtl.CompilerUtils.getLoweringTransforms
import firrtl.transforms.{BlackBoxSourceHelper, NoDCEAnnotation}
import java.io._

import scala.sys.process._
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
    val state = CircuitState(circuit, HighForm, Seq(NoDCEAnnotation))
    val verilog = compiler.compileAndEmit(state)
    val file = new PrintWriter(s"${testDir.getAbsolutePath}/${circuit.main}.v")
    file.write(verilog.getEmittedCircuit.value)
    file.close()
    circuit.main
  }

  private def yosysModel(yosysJson: Seq[String]) : Map[String, BigInt] = {
    val bit_signal   : Regex = "\\s+\\{ \"name\": \"([a-zA-Z][a-zA-Z0-9_\\.]+)\", \"wave\": \"([01])\" \\},?".r
    val multi_signal : Regex = "\\s+\\{ \"name\": \"([a-zA-Z][a-zA-Z0-9_\\.]+)\", \"wave\": \"=\", \"data\": \\[\"([01]+)\"\\] \\},?".r

    yosysJson.collect {
      case bit_signal(name, value) => name -> BigInt(value)
      case multi_signal(name, value) => name -> BigInt(value, 2)
    }.toMap
  }

  // based on yosysExpectFailure
  private def yosysCheck(testDir: File, module: String, trigger: String): Tuple2[Boolean, Seq[String]] = {
    val scriptFileName = s"${testDir.getAbsolutePath}/yosys_script"
    val modelFileName = s"${testDir.getAbsolutePath}/$module.json"
    val yosysScriptWriter = new PrintWriter(scriptFileName)
    yosysScriptWriter.write(
      s"""read_verilog ${testDir.getAbsolutePath}/$module.v
         |prep; proc; opt; memory; flatten
         |hierarchy -top $module
         |sat -verify -show-all -dump_json $modelFileName -prove $trigger 0 -seq 1 $module"""
          .stripMargin)
    yosysScriptWriter.close()

    // execute yosys
    val resultFileName = testDir.getAbsolutePath + "/yosys_results"
    val ret = (s"yosys -s $scriptFileName" #> new File(resultFileName)).!
    val success = ret == 0
    val model = if(success) { Seq() } else { io.Source.fromFile(modelFileName).getLines.toSeq }

    (success, model)
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