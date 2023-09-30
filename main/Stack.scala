// See LICENSE.txt for license details.
package aias_lab6.Hw2

import chisel3._
import chisel3.util.log2Ceil

class Stack(val depth: Int, val bit : Int = 150) extends Module {
    val io = IO(new Bundle {
    val push    = Input(Bool())
    val pop     = Input(Bool())
    val en      = Input(Bool())
    val dataIn  = Input(UInt(bit.W))
    val dataOut = Output(UInt(bit.W))
    val empty   = Output(Bool())
    val full    = Output(Bool())
    })
    val stack_mem = Mem(depth, UInt(bit.W))
    val sp        = RegInit(0.U(log2Ceil(depth+1).W))
    val out       = RegInit(0.U(bit.W))

    when (io.en) {
        when(io.push && (sp < depth.asUInt)) {
        stack_mem(sp) := io.dataIn
        sp := sp + 1.U
        } .elsewhen(io.pop && (sp > 0.U)) {
        sp := sp - 1.U
        }
        when (sp > 0.U) {
        out := stack_mem(sp - 1.U)
        }.elsewhen(sp === 0.U){
            out := 0.U 
        }
    }
    io.empty := Mux(sp === 0.U, true.B, false.B)
    io.full:= Mux(sp === depth.asUInt , true.B, false.B)
    io.dataOut := out
}
