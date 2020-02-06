// Copyright 2019 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package specimpl


import scala.collection.mutable
import firrtl._
import firrtl.ir._
import firrtl.Mappers._

object blockToModule {
  def apply(name: String, root: Statement) : Module = {
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
        // println(s"New Input: $name -> $ref (${ref.serialize}")
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
        val expr = onExpr(con.expr)
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
}