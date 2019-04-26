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

import firrtl.passes.PassException



object LHS { def apply() : Gender = FEMALE }
object RHS { def apply() : Gender = MALE }


case class SpecImplPair(id: Int, module: String, spec: Conditionally, impl: Conditionally)

class NotEquivalentException(info: Info, msg: String) extends PassException(
  s"$info: $msg"
)

class EquivalenceChecker(checker: CombinatorialChecker, info: Info, spec: Module, impl: Module) extends BackendCompilationUtilities {
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

  private def assertMatchingInput(spec: Map[String, Type], impl: Map[String, Type]) = {
    for((name, tpe) <- spec) {
      assert(impl.contains(name), s"Implementation is missing input `$name` from spec.")
      assert(impl(name) == tpe, s"Type mismatch for input `$name`: $tpe vs ${impl(name)}")
    }
  }

  private def getIO() : Tuple3[Map[String, Type], Map[String, Type], Map[String, Type]] = {
    val (spec_in, spec_out) = getModuleIO(spec)
    val (impl_in, impl_out) = getModuleIO(impl)
    assertMatchingInput(spec_in, impl_in)
    // outputs do not have to match since they have the `_en` port which could always be false
    (spec_in, spec_out, impl_out)
  }

  private def makeMiter(spec_name: String, impl_name: String, in: Map[String, Type], spec_out: Map[String, Type], impl_out: Map[String, Type]) : Circuit = {
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
    def specIO(name: String) = WSubField(WSubField(WRef(spec_name), "io"), name)
    def implIO(name: String) = WSubField(WSubField(WRef(impl_name), "io"), name)
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
    def make_trigger(output: String) : Seq[Expression] = {
      if(spec_out.contains(output) && impl_out.contains(output)) {
        Seq(
          outNeq(s"${output}_en"),
          Ite(specIO(s"${output}_en"), outNeq(output), UIntLiteral(0))
        )
      } else {
        val ref = if(spec_out.contains(output)) { specIO(output+"_en") } else { implIO(output+"_en") }
        Seq(DoPrim(PrimOps.Eq, Seq(ref, UIntLiteral(0)), Seq(), bool_t))
      }
    }
    val triggers = (spec_out.keys.toSet | impl_out.keys.toSet).flatMap(make_trigger)
    val trigger_signal : Expression = triggers.reduce( (a,b) => DoPrim(PrimOps.Or, Seq(a,b), Seq(), bool_t))
    val body = Block(Seq(
        makeInstance(spec_name), makeInstance(impl_name),
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

  private def parseModel(signals: Map[String, BigInt], in: Map[String, Type], spec_out: Map[String, Type], impl_out: Map[String, Type]) : String = {

    val inputs = in.map{ case (name, _) => name -> signals(s"io_${name}") }
    val spec_vals = spec_out.map{ case (name, _) => name -> (signals(s"spec.io_${name}"), signals(s"spec.io_${name}_en")) }
    val impl_vals = impl_out.map{ case (name, _) => name -> (signals(s"impl.io_${name}"), signals(s"impl.io_${name}_en")) }
    val names = (spec_out.keys.toSet | impl_out.keys.toSet).toSeq
    def compare_output(name: String) : Boolean = {
      if(spec_vals.contains(name) && impl_vals.contains(name)) {
        spec_vals(name) == impl_vals(name)
      } else {
        val en = spec_vals.getOrElse(name, impl_vals(name))._2
        en == 0
      }
    }
    val same = names.filter(compare_output)
    val diff = names.filter{!compare_output(_)}

    def n(name: String) : String = name.replace('_', '.')
    val out_str = new StringBuffer
    out_str.append("Inputs:\n")
    inputs.foreach{ case (name, value) => out_str.append(s"${n(name)}: $value\n") }
    out_str.append("Disagreeing Outputs:\n")
    for(name <- diff) {
      if(spec_vals.contains(name) && impl_vals.contains(name)) {
        val (spec_val, spec_en) = spec_vals(name)
        val (impl_val, impl_en) = impl_vals(name)
        out_str.append(s"${n(name)}:\n")
        if (spec_en != impl_en) {
          out_str.append(s"\tspec: updates? ${spec_en != 0}\n")
          out_str.append(s"\timpl: updates? ${impl_en != 0}\n")
        } else {
          assert(spec_en == 1)
          out_str.append(s"\tspec: $spec_val\n")
          out_str.append(s"\timpl: $impl_val\n")
        }
      } else {
        out_str.append(s"${n(name)}:\n")
        out_str.append(s"\tspec: updates? ${spec_vals.contains(name)}\n")
        out_str.append(s"\timpl: updates? ${impl_vals.contains(name)}\n")
      }
    }
    out_str.append("Other Outputs:\n")
    same.foreach{ case name => out_str.append(s"${n(name)}: ${spec_out(name)}\n") }
    out_str.toString
  }

  // inspired by firrtlEquivalenceTest from FirrtlSpec.scala
  def run(prefix: String) = {
    assert(spec.name.startsWith("spec"))
    assert(impl.name.startsWith("impl"))
    val (in, spec_out, impl_out) = getIO()

    val miter = makeMiter(spec.name, impl.name, in, spec_out, impl_out)
    println(miter.serialize)


    val res = checker.checkCombinatorial(prefix, miter, "trigger")
    res match {
      case _ : IsEquivalent => {
        println("✔️ Implementation follows spec!")
      }
      case IsNotEquivalent(model) => {
        println("❌ Implementation dows not follow the spec!")
        val model_str = parseModel(model, in, spec_out, impl_out)
        println(model_str)
        val msg = "Implementation dows not follow the spec!\n" + model_str
        throw new NotEquivalentException(info, msg)
      }
    }


    //val spec_verilog = makeVerilog(testDir, spec)
    //val impl_verilog = makeVerilog(testDir, impl)
    // val success = yosysExpectSuccess(impl_verilog, spec_verilog, testDir, Seq())
    //println(s"Equivalent? $success")
  }
}


class SpecImplCheck extends NamedBlockTransform {
  private val form = HighForm
  override def inputForm = form
  override def outputForm = form
  override def name = "Spec/Impl Block Verification"
  override def execute(state: CircuitState): CircuitState = {
    val mod_index = state.circuit.modules.collect{ case m: firrtl.ir.Module => m}.map{ m => m.name -> m}.toMap
    val spec_impl_pairs = pairBlocks()
    for(sip <- spec_impl_pairs) {
      verify(mod_index(sip.module), sip)
    }
    state
  }

  private def pairBlocks() : Seq[SpecImplPair] = {
    // find matching blocks
    val meta_blocks : Seq[(SpecImplMeta, NamedBlock)] = blocks.map(b => b.meta.get.asInstanceOf[SpecImplMeta] -> b)
    val id_to_block : Map[Int, Seq[NamedBlock]] = meta_blocks.map{
      case (SpecImplId(id), b) => id -> b
      case (SpecImplProblem(id), b) => id -> b
    }.groupBy(_._1).map(p => p._1 -> p._2.map(_._2))
    // generate spec impl pairs
    id_to_block.map { case (id, blocks) =>
      assert(blocks.length == 2, s"Expected spec/impl pair, got: $blocks")
      val spec = blocks.filter(_.visible).head
      assert(spec.name.startsWith(s"spec_$id"), s"Unexpected name: ${spec.name}")
      val impl = blocks.filter(!_.visible).head
      assert(impl.name.startsWith(s"impl_$id"), s"Unexpected name: ${impl.name}")
      assert(spec.module == impl.module, s"Expected spec/impl pair to be in the same module ($spec/$impl)")
      SpecImplPair(id, spec.module, spec=spec.when, impl=impl.when)
    }.toSeq
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

    println(s"Inputs: $inputs")
    println(s"Outputs: $outputs")

    Module(NoInfo, name, Seq(clk, rst, io_port), Block(Seq(output_inits, body)))
  }


  def verify(mod: firrtl.ir.Module, pair: SpecImplPair) = {
    // turn spec and impl scopes into modules
    val spec = makeModule(s"spec_${pair.id}", pair.spec.conseq)
    val impl = makeModule(s"impl_${pair.id}", pair.impl.conseq)

    // generate verilog from modules
    val backend = new YosysChecker
    val eq = new EquivalenceChecker(backend, mod.info, spec, impl)
    // TODO: make sure that prefix is unique
    eq.run(mod.name)
  }

}
