// Copyright 2019 The Regents of the University of California
// Copyright 2019 Olof Kindgren
// released under BSD 2-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>
// based on Olof Kindgren's implementation in Verilog

package serv

import chisel3._
import chisel3.util._

class ctrl_io extends Bundle {
  val en = Output(Bool())
  val pc_en = Output(Bool())
  val jmp = Output(Bool())
  val jalr = Output(Bool())
  val jal_or_jalr = Output(Bool())
  val utype = Output(Bool())
  val lui = Output(Bool())
  val trap = Output(Bool())
  val mret = Output(Bool())
  val misalign = Input(Bool())
}

class bufreg_io extends Bundle {
  val hold = Output(Bool())
  val imm_en = Output(Bool())
  val loop = Output(Bool())
}

class rf_io extends Bundle {

}

class serv_decode extends Module {

}