// spec / impl scope implementation
// author: Kevin Laeufer <laeufer@eecs.berkeley.edu>

package specimpl

import java.util.EmptyStackException

import scala.collection.mutable
import firrtl._
import firrtl.ir._
import firrtl.annotations._
import firrtl.Mappers._
import firrtl.util.BackendCompilationUtilities

case class SpecImplAnnotation(target: ComponentName, is_spec: Boolean, other: Option[ComponentName]) extends SingleTargetAnnotation[ComponentName] {
  def duplicate(n: ComponentName): SpecImplAnnotation = this.copy(target = n)
}

case class SpecImplPair(m: String, spec_wire: String, impl_wire: String)

class EquivalenceChecker(spec: Module, impl: Module) extends BackendCompilationUtilities {
  private def getIO(m: Module): Tuple2[Map[String, Type], Map[String, Type]] = {
    assert(m.ports.length == 3)
    assert(m.ports.map(_.name) == Seq("clock", "reset", "io"))
    val io = m.ports.last
    assert(io.direction == Input)
    val fields = io.tpe match {
      case bundle: BundleType => bundle.fields
      case _ => throw new RuntimeException(s"Unexpected io type: ${io.tpe}")
    }
    (fields.collect{ case Field(name, Default, tpe) => name -> tpe }.toMap,
        fields.collect{ case Field(name, Flip, tpe) => name -> tpe }.toMap)
  }

  private def assertMatchingIO(dir: String, spec: Map[String, Type], impl: Map[String, Type]) = {
    for((name, tpe) <- spec) {
      assert(impl.contains(name), s"Implementation is missing $dir `$name` from spec.")
      assert(impl(name) == tpe, s"Type mismatch for $dir `$name`: $tpe vs ${impl(name)}")
    }
  }

  // inspired by firrtlEquivalenceTest from FirrtlSpec.scala
  def run() = {
    val (spec_in, spec_out) = getIO(spec)
    val (impl_in, impl_out) = getIO(spec)
    assertMatchingIO("input", spec_in, impl_in)
    assertMatchingIO("output", spec_out, impl_out)
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
      println("SpecImplCheck pass:")
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
        WRef(s"io.${name}", ref.tpe, WireKind, UNKNOWNGENDER)
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
        val out = WRef(s"io.${name}_out", con.loc.tpe, WireKind, UNKNOWNGENDER)
        val en = WRef(s"io.${name}_out_en", bool_t, WireKind, UNKNOWNGENDER)
        outputs += (name -> out.tpe)
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
    val clk = Port(NoInfo, "clock", Input, ClockType)
    val rst = Port(NoInfo, "reset", Input, bool_t)

    //println(s"Inputs: $inputs")
    //println(s"Outputs: $outputs")

    Module(NoInfo, name, Seq(clk, rst, io_port), body)
  }

  def verify(modules: Map[String, firrtl.ir.Module], pair: SpecImplPair) = {
    // turn spec and impl scopes into modules
    val mod = modules(pair.m)
    val spec = makeModule("spec", findBlock(mod, pair.spec_wire))
    val impl = makeModule("impl", findBlock(mod, pair.impl_wire))

    // generate verilog from modules
    val eq = new EquivalenceChecker(spec, impl)
    eq.run()
  }
}
