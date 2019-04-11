import chisel3._
import chisel3.experimental.{RawModule, RunFirrtlTransform}
import chisel3.util._
import chisel3.internal.firrtl.Circuit
import firrtl.{ChirrtlForm, CircuitState, LowFirrtlCompiler, Parser, Transform}
import firrtl.annotations.JsonProtocol
import specimpl._

abstract class TestClass extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(8.W))
    val b = Input(UInt(8.W))
    val c = Output(UInt(8.W))
    val ctrl = Input(UInt(3.W))
    val status = Output(UInt(2.W))
  })
}

class A(correct: Boolean) extends  TestClass {
  println(s"A(correct = $correct)")
  io.c := 0.U
  io.status := 0.U


  spec {
    switch(io.ctrl) {
      is(0.U) {
        io.c := io.a + io.b
      }
      is(1.U) {
        io.c := io.a - io.b
      }
    }
  } .impl {
    val is_add = io.ctrl === 0.U
    val is_sub = io.ctrl === 1.U
    val b = if(correct) {
      Mux(is_sub, (~io.b) + 1.U, io.b)
    } else {
      Mux(is_sub, ~io.b, io.b)
    }
    when(is_add || is_sub) {
      io.c := io.a + b
    }
  }

}




object main {

  def main(args: Array[String]): Unit = {
    check(() => new A(correct = true))
    check(() => new A(correct = false))
  }

  def check[T <: RawModule](gen: () => T) = {
    println
    val ir = chisel3.Driver.elaborate(gen)
    //val firrtl = chisel3.Driver.emit(ir)
    //println(firrtl)
    //val annos = ir.annotations.map(_.toFirrtl)
    //println(JsonProtocol.serialize(annos))
    compile(ir)
    println
  }

  // adapted from chisel3.Driver.execute and firrtl.Driver.execute
  def compile(c: Circuit) = {
    val firrtl = Parser.parseString(chisel3.Driver.emit(c), Parser.UseInfo)
    val transforms = c.annotations
        .collect { case anno: RunFirrtlTransform => anno.transformClass }
        .distinct
        .filterNot(_ == classOf[Transform])
        .map { transformClass: Class[_ <: Transform] =>
          transformClass.newInstance()
        }
    val compiler = new LowFirrtlCompiler()
    val annos = c.annotations.map(_.toFirrtl)
    val res = compiler.compile(CircuitState(firrtl, ChirrtlForm, annos), transforms)
  }
}
