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
case class SpecImplAnnotation(target: Named, is_spec: Boolean, other: Option[Named]) extends SingleTargetAnnotation[Named] {
  def duplicate(n: Named): SpecImplAnnotation = this.copy(target = n)
}
case class SpecImplChiselAnnotation(target: InstanceId, is_spec: Boolean, other: Option[InstanceId])
    extends ChiselAnnotation with RunFirrtlTransform {
  def toFirrtl: SpecImplAnnotation = SpecImplAnnotation(target.toNamed, is_spec, other.map(_.toNamed))
  def transformClass: Class[SpecImplCheck] = classOf[SpecImplCheck]
}

class SpecImplCheck extends Pass {
  override def inputForm = ChirrtlForm
  override def outputForm = ChirrtlForm
  override def run(c: Circuit): Circuit = {
    println("TODO: implement SpicImplCheck pass")
    c
  }
}
