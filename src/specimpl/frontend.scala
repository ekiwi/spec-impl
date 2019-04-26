// Copyright 2019 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package specimpl

import chisel3._
import chisel3.core.{ChiselAnnotation, CompileOptions, RunFirrtlTransform, annotate}
import chisel3.internal.InstanceId
import chisel3.internal.sourceinfo.SourceInfo
import firrtl.annotations.ComponentName


object specimpl {
  private var nextId : Int = 0
  def getUID() : Int = {
    val ii = nextId
    nextId = nextId + 1
    ii
  }
}

object spec {
  def apply(block: => Unit)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions) = new SpecContext(block)
}

final class SpecContext(block: => Unit) {
  val id = specimpl.getUID()
  named_block(s"spec_$id", block, meta = Some(SpecImplId(id)))
  def impl(block: => Unit)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Unit = {
    named_block(s"impl_$id", block, do_include = false, meta = Some(SpecImplProblem(id)))
  }
}

object impl {
  def apply(block: => Unit)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions) = new ImplContext(block)
}

final class ImplContext(block: => Unit) {
  val id = specimpl.getUID()
  named_block(s"impl_$id", block, do_include = false, meta = Some(SpecImplId(id)))
  def spec(block: => Unit)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Unit = {
    named_block(s"spec_$id", block, meta = Some(SpecImplProblem(id)))
  }
}

class SpecImplMeta extends NamedBlockMetaData {
  def subTransformClass: Class[SpecImplCheck] = classOf[SpecImplCheck]
}

case class SpecImplId(id: Int) extends SpecImplMeta

// TODO: add any additional information to this struct
case class SpecImplProblem(id: Int) extends SpecImplMeta

case class SpecImplChiselAnnotation(target: InstanceId, is_spec: Boolean, other: Option[InstanceId])
    extends ChiselAnnotation with RunFirrtlTransform {
  def toFirrtl: SpecImplAnnotation = SpecImplAnnotation(target.toNamed.asInstanceOf[ComponentName],
    is_spec,
    other.map(_.toNamed.asInstanceOf[ComponentName]))
  def transformClass: Class[SpecImplCheck] = classOf[SpecImplCheck]
}
