// spec / impl scope implementation
// author: Kevin Laeufer <laeufer@eecs.berkeley.edu>

package specimpl

import chisel3._
import chisel3.core.{ChiselAnnotation, CompileOptions, RunFirrtlTransform, annotate}
import chisel3.internal.InstanceId
import chisel3.internal.sourceinfo.SourceInfo
import firrtl._
import firrtl.ir._
import firrtl.passes.Pass
import firrtl.annotations._
import firrtl.Utils._
// import firrtl.traversals.Foreachers._

// frontend

object specimpl {
  def make_block(block: => Unit, is_spec: Boolean, other: Option[InstanceId] = None): InstanceId = {
    // TODO: the dummy wire below is a hack to work around the fact that `when` statements cannot be annotated
    val wire_mark = Wire(Bool())
    wire_mark.suggestName(if(is_spec) { "spec" } else { "impl" })
    annotate(SpecImplChiselAnnotation(wire_mark, is_spec, other))
    when((!is_spec).B) {
      block
    }
    // avoid not fully initialized error
    wire_mark := is_spec.B
    wire_mark
  }
}

object spec {
  def apply(block: => Unit)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions) = new SpecContext(block)
}

final class SpecContext(block: => Unit) {
  val other = specimpl.make_block(block, is_spec = true)
  def impl(block: => Unit)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Unit = {
    specimpl.make_block(block, is_spec = false, other = Some(other))
  }
}

object impl {
  def apply(block: => Unit)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions) = new ImplContext(block)
}

final class ImplContext(block: => Unit) {
  val other = specimpl.make_block(block, is_spec = false)
  def spec(block: => Unit)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Unit = {
    specimpl.make_block(block, is_spec = true, other = Some(other))
  }
}

// backend
case class SpecImplAnnotation(target: ComponentName, is_spec: Boolean, other: Option[ComponentName]) extends SingleTargetAnnotation[ComponentName] {
  def duplicate(n: ComponentName): SpecImplAnnotation = this.copy(target = n)
}
case class SpecImplChiselAnnotation(target: InstanceId, is_spec: Boolean, other: Option[InstanceId])
    extends ChiselAnnotation with RunFirrtlTransform {
  def toFirrtl: SpecImplAnnotation = SpecImplAnnotation(target.toNamed.asInstanceOf[ComponentName],
                                                        is_spec,
                                                        other.map(_.toNamed.asInstanceOf[ComponentName]))
  def transformClass: Class[SpecImplCheck] = classOf[SpecImplCheck]
}

case class SpecImplPair(m: String, spec_wire: String, impl_wire: String)

class SpecImplCheck extends Transform {
  private val form = HighForm
  override def inputForm = form
  override def outputForm = form
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

  def verify(modules: Map[String, firrtl.ir.Module], pair: SpecImplPair) = {
    val mod = modules(pair.m)
    val spec = findBlock(mod, pair.spec_wire)
    val impl = findBlock(mod, pair.impl_wire)


    println("Spec")
    println(spec.serialize)
    println()
    println("Impl")
    println(impl.serialize)

  }
}
