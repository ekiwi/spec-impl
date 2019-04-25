// Copyright 2019 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package specimpl

import chisel3._
import chisel3.core.{ChiselAnnotation, CompileOptions, RunFirrtlTransform, annotate}
import chisel3.internal.InstanceId
import chisel3.internal.sourceinfo.SourceInfo
import firrtl.annotations.ComponentName


object spec {
  def apply(block: => Unit)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions) = new SpecContext(block)
}

final class SpecContext(block: => Unit) {
  named_block("spec", block)
  //val other = specimpl.make_block(block, is_spec = true)
  def impl(block: => Unit)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Unit = {
    named_block("impl", block, do_include = false)
    // specimpl.make_block(block, is_spec = false, other = Some(other))
  }
}

object impl {
  def apply(block: => Unit)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions) = new ImplContext(block)
}

final class ImplContext(block: => Unit) {
  //val other = specimpl.make_block(block, is_spec = false)
  named_block("impl", block, do_include = false)
  def spec(block: => Unit)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Unit = {
    named_block("spec", block)
    //specimpl.make_block(block, is_spec = true, other = Some(other))
  }
}

case class SpecImplChiselAnnotation(target: InstanceId, is_spec: Boolean, other: Option[InstanceId])
    extends ChiselAnnotation with RunFirrtlTransform {
  def toFirrtl: SpecImplAnnotation = SpecImplAnnotation(target.toNamed.asInstanceOf[ComponentName],
    is_spec,
    other.map(_.toNamed.asInstanceOf[ComponentName]))
  def transformClass: Class[SpecImplCheck] = classOf[SpecImplCheck]
}
