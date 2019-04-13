// Copyright 2019 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package specimpl


import scala.collection.mutable
import firrtl._
import firrtl.ir._
import firrtl.annotations._
import firrtl.Mappers._
import firrtl.util.BackendCompilationUtilities
import firrtl.CompilerUtils.getLoweringTransforms
import firrtl.transforms.BlackBoxSourceHelper
import java.io._

import firrtl.passes.PassException

import scala.sys.process.{ProcessBuilder, ProcessLogger, _}
import scala.util.matching._



object LHS { def apply() : Gender = FEMALE }
object RHS { def apply() : Gender = MALE }

case class SpecImplAnnotation(target: ComponentName, is_spec: Boolean, other: Option[ComponentName]) extends SingleTargetAnnotation[ComponentName] {
  def duplicate(n: ComponentName): SpecImplAnnotation = this.copy(target = n)
}

case class SpecImplPair(m: String, spec_wire: String, impl_wire: String)

class MinimumFirrtlToVerilogCompiler extends Compiler {
  def emitter = new VerilogEmitter
  def transforms: Seq[Transform] = getLoweringTransforms(HighForm, LowForm) ++
      Seq(new MinimumLowFirrtlOptimization, new BlackBoxSourceHelper)
}

class NotEquivalentException(info: Info, msg: String) extends PassException(
  s"$info: $msg"
)

class EquivalenceChecker(info: Info, spec: Module, impl: Module) extends BackendCompilationUtilities {
  private def getModuleIO(m: Module): Tuple2[Map[String, Type], Map[String, Type]] = {
    assert(m.ports.length == 3)
    assert(m.ports.map(_.name) == Seq("clock", "reset", "io"))
    val io = m.ports.last
    assert(io.direction == Input)
    val fields = io.tpe match {
      case bundle: BundleType => bundle.fields
      case _ => throw new RuntimeException(s"Unexpected io type: ${io.tpe}")
    }
    val inputs = fields.collect{ case Field(name, Default, tpe) => name -> tpe }.toMap
    val outputs = fields.collect{ case Field(name, Flip, tpe) => name -> tpe }
    val outputs_without_en = outputs.filter{ case (name, _) => !name.endsWith("_en")}.toMap
    assert(outputs.size == outputs_without_en.size * 2)
    (inputs, outputs_without_en)
  }

  private def assertMatchingIO(dir: String, spec: Map[String, Type], impl: Map[String, Type]) = {
    for((name, tpe) <- spec) {
      assert(impl.contains(name), s"Implementation is missing $dir `$name` from spec.")
      assert(impl(name) == tpe, s"Type mismatch for $dir `$name`: $tpe vs ${impl(name)}")
    }
  }

  private def getIO() : Tuple2[Map[String, Type], Map[String, Type]] = {
    val (spec_in, spec_out) = getModuleIO(spec)
    val (impl_in, impl_out) = getModuleIO(impl)
    assertMatchingIO("input", spec_in, impl_in)
    assertMatchingIO("output", spec_out, impl_out)
    // IO is equivalent
    (spec_in, spec_out)
  }

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

  private def makeVerilog(testDir: File, m: Module): String = {
    val circuit = Circuit(m.info, Seq(m), m.name)
    makeVerilog(testDir, circuit)
  }

  private def makeMiter(in: Map[String, Type], out: Map[String, Type]) : Circuit = {
    val bool_t = UIntType(IntWidth(1))
    val io = WRef("io")
    val io_port = Port(NoInfo, "io", Input, BundleType(in.map { case (name, tpe) => Field(name, Default, tpe) }.toSeq))
    val ports = Seq(
      Port(NoInfo, "clock", Input, ClockType),
      Port(NoInfo, "reset", Input, bool_t),
      io_port,
      Port(NoInfo, "trigger", Output, bool_t)
    )
    val trigger = WRef("trigger")
    def specIO(name: String) = WSubField(WSubField(WRef("spec"), "io"), name)
    def implIO(name: String) = WSubField(WSubField(WRef("impl"), "io"), name)
    def updateTrigger(expr: Expression) = {
      assert(expr.tpe == bool_t)
      Connect(NoInfo, trigger, DoPrim(PrimOps.Or, Seq(trigger, expr), Seq(), bool_t))
    }
    def outNeq(name: String) : Expression = {
      DoPrim(PrimOps.Neq, Seq(specIO(name), implIO(name)), Seq(), bool_t)
    }
    def outEq(name: String) : Expression = {
      DoPrim(PrimOps.Eq, Seq(specIO(name), implIO(name)), Seq(), bool_t)
    }
    def makeInstance(name: String) = {
      Block(Seq(
        DefInstance(NoInfo, name, name),
        Connect(NoInfo, WSubField(WRef(name), "clock"), WRef("clock")),
        Connect(NoInfo, WSubField(WRef(name), "reset"), WRef("reset"))
      ))
    }
    def Ite(c: Expression, a: Expression, b: Expression) = {
      assert(a.tpe == b.tpe)
      Mux(c, a, b, a.tpe)
    }
    val triggers : Iterable[Expression] = out.flatMap{ case (name, _) => Seq(
      outNeq(s"${name}_en"),
      Ite(specIO(s"${name}_en"), outNeq(name), UIntLiteral(0))
    )}
    val trigger_signal : Expression = triggers.reduce( (a,b) => DoPrim(PrimOps.Or, Seq(a,b), Seq(), bool_t))
    val body = Block(Seq(
        makeInstance("spec"), makeInstance("impl"),
        // not triggered by default
        Connect(NoInfo, trigger, trigger_signal),
      ) ++
      // Inputs
      in.map { case (name, _) => Block(Seq(
        Connect(NoInfo, specIO(name), WSubField(io, name)),
        Connect(NoInfo, implIO(name), WSubField(io, name))
      ))}.toSeq)

    val miter = Module(NoInfo, "miter", ports, body)
    Circuit(NoInfo, Seq(spec, impl, miter), miter.name)
  }

  private def yosysModelToString(yosysOut: Seq[String], in: Map[String, Type], out: Map[String, Type]) : String = {
    val signal : Regex = raw"\s+(\d+)\s+\\([a-zA-Z][a-zA-Z0-9_\.]+)\s+(\d+)\s+.+".r
    val signals : Map[String,BigInt] = yosysOut.collect{
      case signal(time, name, value) => assert(time == "1"); name -> BigInt(value)
    }.toMap

    //println(yosysOut)

    val inputs = in.map{ case (name, _) => name -> signals(s"io_${name}") }
    val spec_out = out.map{ case (name, _) => name -> (signals(s"spec.io_${name}"), signals(s"spec.io_${name}_en")) }
    val impl_out = out.map{ case (name, _) => name -> (signals(s"impl.io_${name}"), signals(s"impl.io_${name}_en")) }
    val same = spec_out.filter{ case (name, value) => impl_out(name) == value }.keys.toSeq
    val diff = spec_out.filter{ case (name, value) => impl_out(name) != value }.keys.toSeq

    def n(name: String) : String = name.replace('_', '.')
    val out_str = new StringBuffer
    out_str.append("Inputs:\n")
    inputs.foreach{ case (name, value) => out_str.append(s"${n(name)}: $value\n") }
    out_str.append("Disagreeing Outputs:\n")
    for(name <- diff) {
      val (spec_val, spec_en) = spec_out(name)
      val (impl_val, impl_en) = impl_out(name)
      out_str.append(s"${n(name)}:\n")
      if(spec_en != impl_en) {
        out_str.append(s"\tspec: updates? $spec_en\n")
        out_str.append(s"\timpl: updates? $impl_en\n")
      } else {
        assert(spec_en == 1)
        out_str.append(s"\tspec: $spec_val\n")
        out_str.append(s"\timpl: $impl_val\n")
      }
    }
    out_str.append("Other Outputs:\n")
    same.foreach{ case name => out_str.append(s"${n(name)}: ${spec_out(name)}\n") }
    out_str.toString
  }

  // based on yosysExpectFailure
  private def yosysCheckCombinatorialEq(testDir: File): Tuple2[Boolean, Seq[String]] = {
    val scriptFileName = s"${testDir.getAbsolutePath}/yosys_script"
    val yosysScriptWriter = new PrintWriter(scriptFileName)
    yosysScriptWriter.write(
      s"""read_verilog ${testDir.getAbsolutePath}/miter.v
         |prep; proc; opt; memory; flatten
         |hierarchy -top miter
         |sat -verify -show-all -prove trigger 0 -seq 1 miter"""
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

  // inspired by firrtlEquivalenceTest from FirrtlSpec.scala
  def run(prefix: String) = {
    assert(spec.name == "spec")
    assert(impl.name == "impl")
    val (in, out) = getIO()

    val testDir = createTestDirectory(prefix + "_equivalence_test")
    println(s"Results dir: ${testDir.getAbsolutePath}")
    makeVerilog(testDir, makeMiter(in, out))
    val (success, stdout) = yosysCheckCombinatorialEq(testDir)
    if(success) {
      println("✔️ Implementation follows spec!")
    } else {
      println("❌ Implementation dows not follow the spec!")
      val model = yosysModelToString(stdout, in, out)
      println(model)
      val msg = "Implementation dows not follow the spec!\n" + model
      throw new NotEquivalentException(info, msg)
    }

    //val spec_verilog = makeVerilog(testDir, spec)
    //val impl_verilog = makeVerilog(testDir, impl)
    // val success = yosysExpectSuccess(impl_verilog, spec_verilog, testDir, Seq())
    //println(s"Equivalent? $success")
  }
}


class SpecImplCheck extends Transform {
  private val form = HighForm
  override def inputForm = form
  override def outputForm = form
  override def name = "Spec/Impl Block Verification"
  override def execute(state: CircuitState): CircuitState = {
    val annos = state.annotations.collect{ case a: SpecImplAnnotation => a}
    if(annos.length > 0) {
      // println("SpecImplCheck pass:")
      // println(state.circuit.serialize)
      val mod_index = state.circuit.modules.collect{ case m: firrtl.ir.Module => m}.map{ m => m.name -> m}.toMap
      val spec_impl_pairs = parseAnnotations(annos)
      for(sip <- spec_impl_pairs) {
        verify(mod_index, sip)
      }
    }
    state
  }

  def parseAnnotations(annos: Seq[SpecImplAnnotation]) : Seq[SpecImplPair] = {
    val target_to_anno = annos.map{ a => a.target -> a}.toMap
    val complete_annos = annos.filter(_.other.isDefined)
    for(a <- complete_annos) {
      assert(target_to_anno.get(a.other.get).isDefined, s"couldn't find matching spec/impl block for $a")
      val other = target_to_anno(a.other.get)
      assert(other.is_spec != a.is_spec, s"other block does not match $a/$other")
      assert(other.target.module == a.target.module, s"spec/impl scopes have to be in the same module! $a/$other")
    }
    complete_annos.collect {
      case SpecImplAnnotation(s, true, Some(i)) => SpecImplPair(s.module.name, s.name, i.name)
      case SpecImplAnnotation(i, false, Some(s)) => SpecImplPair(s.module.name, s.name, i.name)
    }
  }

  def findBlock(module: firrtl.ir.Module, wire_name: String) : Statement = {
    // we need to find the Statement inside the when statement after the spec/impl wire
    // the idea here is that it has to be in a block!
    def searchBlock(stmts: Seq[Statement]) : Option[Statement] = {
      var get_next_conditional = false
      for(s <- stmts) {
        if(get_next_conditional) {
          val when = s.asInstanceOf[Conditionally]
          //println(s"when.pred: ${when.pred}")
          //println(s"when.alt: ${when.alt}")
          return Some(when.conseq)
        }
        val is_needle = s match {
          case DefWire(_, name, _) => name == wire_name
          case _ => false
        }
        get_next_conditional = is_needle
      }
      None
    }
    def visitStatement(stmt: Statement) : Option[Statement] = {
      stmt match {
        case b: Block => searchBlock(b.stmts)
        case _ => None
      }
    }
    visitStatement(module.body).get
  }

  def makeModule(name: String, root: Statement) = {
    val bool_t = UIntType(IntWidth(1))
    // TODO: change to Map[String,Expression] and include original WRef/... nodes
    val internally_defined = mutable.Set[String]()
    val inputs = mutable.Map[String,Expression]()
    val outputs = mutable.Map[String,Type]()
    // TODO: more reliable name generation
    def toName(ref: Expression) : String = ref.serialize.replace('.', '_')
    def onUse(ref: Expression) : Expression = {
      val name = toName(ref)
      if(!internally_defined.contains(name)) {
        inputs += (name -> ref)
        WSubField(WRef("io"), name)
      } else { ref }
    }
    def onDef[T <: Statement with IsDeclaration](dd: T): Statement with IsDeclaration = {
      val name = dd.name
      dd match {
        case wire: DefWire => internally_defined += name; outputs += (name -> wire.tpe)
        case node: DefNode => internally_defined += name
      }
      dd
    }
    def onConnect(con: Connect): Statement = {
      // IMPORTANT: this runs before onExpr!
      val name = toName(con.loc)
      if(!internally_defined.contains(name)) {
        val out = WSubField(WRef("io"), s"${name}_out")
        val en = WSubField(WRef("io"), s"${name}_out_en")
        outputs += (name -> con.loc.tpe)
        val expr = con.expr.mapExpr(onExpr)
        Block(Seq(
          Connect(NoInfo, out, expr), Connect(NoInfo, en, UIntLiteral(1))
        ))
      } else {
        throw new RuntimeException("TODO: implement outputs for internally defined Wires, Regs etc....")
        con
      }
    }
    def onExpr(expr: Expression): Expression = {
      expr match {
        case field : WSubField => onUse(field)
        case field : WSubAccess => onUse(field)
        case field : WSubIndex => onUse(field)
        case ref : WRef => onUse(ref)
        case other => other.map(onExpr)
      }
    }
    def onStmt(stmt: Statement): Statement = {
      stmt match {
        case wire: DefWire => onDef(wire)
        case dd: DefNode => onDef(dd).mapExpr(onExpr)
        case con : Connect => onConnect(con)
        case inst: WDefInstance => throw new RuntimeException(s"Module instantiations are not supported yet! $inst")
        case reg: DefRegister => throw new RuntimeException(s"Internal registers are not supported yet! $reg")
        case mem: DefMemory => throw new RuntimeException(s"Internal memory is not supported yet! $mem")
        case _ : Attach => throw new RuntimeException("Attach not supported")
        case _ : PartialConnect => throw new RuntimeException("PartialConnect not supported")
        case other => other.mapStmt(onStmt).mapExpr(onExpr)
      }
    }
    val body = onStmt(root)


    val io_port = Port(NoInfo, "io", Input, BundleType(
      inputs.toMap.map { case (name, ref) => Field(name.replace('.', '_'), Default, ref.tpe) }.toSeq ++
      outputs.toMap.flatMap {case (name, tpe) => Seq(Field(s"${name}_out", Flip, tpe), Field(s"${name}_out_en", Flip, bool_t))}.toSeq
    ))

    val output_inits = Block(
      outputs.toMap.map { case (name, tpe) => Block(Seq(
        IsInvalid(NoInfo, WSubField(WRef("io"), s"${name}_out")),                   // outputs are invalid by default
        Connect(NoInfo, WSubField(WRef("io"), s"${name}_out_en"), UIntLiteral(0)),  // only if this is reconnected to 1 is the output valid
      )) }.toSeq
    )

    val clk = Port(NoInfo, "clock", Input, ClockType)
    val rst = Port(NoInfo, "reset", Input, bool_t)

    //println(s"Inputs: $inputs")
    //println(s"Outputs: $outputs")

    Module(NoInfo, name, Seq(clk, rst, io_port), Block(Seq(output_inits, body)))
  }

  def verify(modules: Map[String, firrtl.ir.Module], pair: SpecImplPair) = {
    // turn spec and impl scopes into modules
    val mod = modules(pair.m)
    val spec = makeModule("spec", findBlock(mod, pair.spec_wire))
    val impl = makeModule("impl", findBlock(mod, pair.impl_wire))

    // generate verilog from modules
    val eq = new EquivalenceChecker(mod.info, spec, impl)
    // TODO: make sure that prefix is unique
    eq.run(mod.name)
  }
}
