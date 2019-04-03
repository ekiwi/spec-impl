import chisel3._
import chisel3.util._

abstract class TestClass extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(8.W))
    val b = Input(UInt(8.W))
    val c = Output(UInt(8.W))
    val ctrl = Input(UInt(3.W))
    val status = Output(UInt(2.W))
  })
}

class A extends  TestClass {
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
    val b = Mux(is_sub, ~io.b, io.b)
    when(is_add || is_sub) {
      io.c := io.a + b
    }
  }

}




object main {

  def main(args: Array[String]): Unit = {
    println("Hello World")
    // print firrtl to command line
    val ir = chisel3.Driver.elaborate(() => new A)
    val firrtl = chisel3.Driver.emit(ir)
    println(firrtl)

    // generate verilog and save to file
    //chisel3.Driver.execute(args, () => new ReferenceGcd(8))
    //chisel3.Driver.execute(args, () => new GuardedAtomicActionGcd(8))
  }
}
