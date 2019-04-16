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

//import scala.sys.process.{ProcessBuilder, ProcessLogger, _}
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

case class CompletedProcess(returncode: Int, stdout: Seq[String], stderr: Seq[String])
object runCommand {
  // there seemed to be a bug where sys.process losses some stdout information,
  // which is why we reimplement that functionality with Jav primitives
  def apply(cmd: String, args: Seq[String] = Seq()) : CompletedProcess = {
    val list = scala.collection.JavaConverters.seqAsJavaList(Seq(cmd) ++ args)
    val pb = new ProcessBuilder(list)
    pb.environment.put("PATH", System.getenv("PATH"))
    val stdout = mutable.ArrayStack[String]()
    val stderr = mutable.ArrayStack[String]()

    val proc = pb.start()
    val out_reader = new BufferedReader(new InputStreamReader(proc.getInputStream))
    val err_reader = new BufferedReader(new InputStreamReader(proc.getErrorStream))

    var s = ""
    while({ s = out_reader.readLine(); s != null}) { println(s"stdout: $s") ; stdout.push(s) }
    while({ s = err_reader.readLine(); s != null}) { println(s"stderr: $s") ; stderr.push(s) }

    proc.waitFor()
    val ret = proc.exitValue()

    CompletedProcess(ret, stdout, stderr)
  }
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
         |sat -show-all -prove $trigger 0 -seq 1 $module"""
          .stripMargin)
    yosysScriptWriter.close()

    /*
    val resultFileName = testDir.getAbsolutePath + "/yosys_results"
    val command = s"yosys -s $scriptFileName" // #> new File(resultFileName)
    val buf = mutable.ArrayBuffer.empty[String]
    val logger = ProcessLogger({a => buf += a}, _ => ())
    val ret = command.!(logger)


    val command2 = s"yosys -s $scriptFileName" #> new File(resultFileName)
    command2.!
    */

    Thread.sleep(100)
    val CompletedProcess(ret, stdout, stderr) = runCommand("yosys", Seq("-s", scriptFileName))
    (ret == 0, stdout)
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