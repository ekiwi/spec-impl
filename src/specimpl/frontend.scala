// Copyright 2019 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package specimpl

import chisel3._
import chisel3.internal.sourceinfo.SourceInfo

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