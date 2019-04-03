// spec / impl scope implementation
// author: Kevin Laeufer <laeufer@eecs.berkeley.edu>

import chisel3._
import chisel3.core.CompileOptions
import chisel3.internal.sourceinfo.SourceInfo

// frontend

object spec {  // scalastyle:ignore object.name
  def apply(block: => Unit)(implicit sourceInfo: SourceInfo,
                            compileOptions: CompileOptions): SpecContext = {
    new SpecContext(sourceInfo, block)
  }
}

final class SpecContext(sourceInfo: SourceInfo, block: => Unit, firrtlDepth: Int = 0) {
  when(false.B) {
    block
  }

  def impl(block: => Unit)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Unit = {
    when(true.B) {
      block
    }
  }
}