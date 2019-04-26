// Copyright 2019 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package specimpl

import chisel3.core.{ChiselAnnotation, CompileOptions, RunFirrtlTransform, annotate}
import chisel3.internal.InstanceId
import chisel3.internal.sourceinfo.SourceInfo
import chisel3._
import firrtl.annotations.{ComponentName, SingleTargetAnnotation}
import firrtl.ir._
import firrtl._


object named_block {
  def apply(name: String, block: => Unit, do_include: Boolean = true, meta: Option[NamedBlockMetaData] = None)
           (implicit sourceInfo: SourceInfo, compileOptions: CompileOptions) : Unit = {
    // TODO: the dummy wire below is a hack to work around the fact that `when` statements cannot be annotated
    val wire_mark = Wire(Bool())
    wire_mark.suggestName(name)
    wire_mark := do_include.B
    annotate(NamedBlockChiselAnnotation(wire_mark, meta))
    when(wire_mark) {
      block
    }
  }
}

abstract class NamedBlockTransform extends Transform {
  private var blocks_p : Seq[NamedBlock] = Seq()
  protected def blocks : Seq[NamedBlock] = blocks_p
  def run(bbs: Seq[NamedBlock], state: CircuitState): CircuitState = {
    blocks_p = bbs
    runTransform(state)
  }
}


trait NamedBlockMetaData {
  def subTransformClass: Class[_ <: NamedBlockTransform]
}

case class NamedBlockChiselAnnotation(target: InstanceId, meta: Option[NamedBlockMetaData])
    extends ChiselAnnotation with RunFirrtlTransform {
  def toFirrtl: NamedBlockAnnotation = NamedBlockAnnotation(target.toNamed.asInstanceOf[ComponentName], meta)
  def transformClass: Class[NamedBlockFinder] = classOf[NamedBlockFinder]
}

case class NamedBlockAnnotation(target: ComponentName, meta: Option[NamedBlockMetaData]) extends SingleTargetAnnotation[ComponentName] {
  def duplicate(n: ComponentName): NamedBlockAnnotation = this.copy(target = n)
}

case class NamedBlock(name: String, when: Conditionally, meta: Option[NamedBlockMetaData] = None, subBlocks: Seq[String] = Seq())

class NamedBlockFinder extends Transform {
  private val form = HighForm
  override def inputForm = form
  override def outputForm = form
  override def name = "Named Block Finder"

  override def execute(state: CircuitState): CircuitState = {
    val annos = state.annotations.collect { case a: NamedBlockAnnotation => a }
    if (annos.nonEmpty) {
      val modules = state.circuit.modules.collect{ case m: firrtl.ir.Module => m}
      val mod_to_block_inst = modules.map{mod => mod.name -> scanModule(mod, annos)}.toMap
      val blocks = mod_to_block_inst.values.flatten.map{bi => {
       val add_sub_blocks = bi.instances.flatMap(mod_to_block_inst(_)).map(_.block.name)
        bi.block.copy(subBlocks = bi.block.subBlocks ++ add_sub_blocks)
      }}

      // TODO: this is ugly, there has to be a better way to call the next transform...
      val meta_blocks = blocks.map(b => (b.meta, b)).collect{case (Some(meta), b) => meta.subTransformClass-> b}
      // collect passes
      val passes = meta_blocks.map(_._1).toSet
      for(pass <- passes) {
        val bbs = meta_blocks.filter(_._1 == pass).map(_._2).toSeq
        val inst = pass.newInstance()
        // TODO: support transforms that emit a changed circuit
        inst.run(bbs, state)
      }

      /*
      for(bb <- blocks) {
        println(s"${bb.name}(${bb.subBlocks}): ${bb.meta} ${bb.when.info.serialize}")
      }
       */
    }
    state
  }

  private def scanModule(mod: firrtl.ir.Module, annos: Seq[NamedBlockAnnotation]) : Seq[BlockInst] = {
    val mod_annos = annos.filter(_.target.module.name == mod.name)
    if(mod_annos.isEmpty) { Seq() } else {
      val wires = mod_annos.map{ a => a.target.name -> a}.toMap
      //println(s"Searching in ${mod.name}")
      val (blocks, instances) = find(mod.name, mod.body, wires)
      //println(s"Blocks: ${blocks}")
      //println(s"Instances: ${instances}")
      blocks
    }
  }

  private case class BlockInst(block: NamedBlock, instances: Seq[String])

  private abstract class SearchState
  private case class Start() extends SearchState
  private case class FoundWire(name: String) extends SearchState
  private case class FoundAssignment(name: String, do_include: Boolean) extends SearchState

  private def find(module: String, stmt: Statement, wires: Map[String, NamedBlockAnnotation])
  : (Seq[BlockInst], Set[String]) = {
    stmt match {
      case Block(sub_stmts) => find(module, sub_stmts, wires)
      case other => find(module, Seq(other), wires)
    }
  }

  private def find(module: String, stmts: Seq[Statement], wires: Map[String, NamedBlockAnnotation])
    : (Seq[BlockInst], Set[String]) = {
    // the pattern that we are searching for is:
    //
    //  wire ${name} : UInt<1>
    //  ${name} <= UInt<1>("h${do_include}")
    //  when ${name}
    //
    // in addition to that we want the names of all modules instantiated in this block

    var submods : Set[String] = Set()
    var blocks  : Seq[BlockInst] = Seq()
    var state : SearchState = Start()

    def subFind(stmt: Statement) : (Seq[BlockInst], Set[String])  = {
      val (bbs, mods) = find(module, stmt, wires)
      blocks = blocks ++ bbs
      submods = submods ++ mods
      (bbs, mods)
    }

    for(s <- stmts) { state = state match {
      case Start() => s match {
        case DefWire(_, name, _) => if(wires contains name) { FoundWire(name) } else { Start() }
        case DefInstance(_, _, module) => submods = submods + module; Start()
        case other => other.mapStmt{s => subFind(s); s} ; Start()
      }
      case FoundWire(name) => {
        assert(s.isInstanceOf[Connect])
        val con = s.asInstanceOf[Connect]
        assert(con.loc.isInstanceOf[Reference], s"${con.loc} (${con.serialize})")
        assert(con.expr.isInstanceOf[UIntLiteral], s"${con.expr} (${con.serialize})")
        val lhs_name = con.loc.asInstanceOf[Reference].name
        assert(lhs_name == name)
        val do_include = con.expr.asInstanceOf[UIntLiteral].value == 1
        FoundAssignment(name, do_include)
      }
      case FoundAssignment(name, do_include) => {
        assert(s.isInstanceOf[Conditionally])
        val when = s.asInstanceOf[Conditionally]
        assert(when.pred.isInstanceOf[Reference])
        assert(when.alt.serialize == "skip")
        val cond_name = when.pred.asInstanceOf[Reference].name
        assert(cond_name == name)
        // we found a block! => find inner blocks and instances
        val (bbs, mods) = subFind(when.conseq)
        val subblocks = bbs.map{ bb => bb.block.name}
        val bb = NamedBlock(s"$module.$name", when, meta=wires(name).meta, subBlocks=subblocks)
        blocks = blocks ++ Seq(BlockInst(bb, mods.toSeq))
        Start()
      }
    }
    }
    (blocks, submods)
  }

}