# Possible Benchmarks for Spec/Impl

TODO: create benchmark repository!

## Refactoring:

### BOOM
* [Use Rocket's PipelinedMultiplier instead of our own ](https://github.com/riscv-boom/riscv-boom/commit/e0cf354b726da08eb53e376c0c0f0722be3e1edc)
* [refactor execution unit generation and wiring](https://github.com/riscv-boom/riscv-boom/commit/a6ce80ce477ed8d3f07ffb6a05c57de3077de0c1)
* [Unify MemExeUnit and ALUExeUnit generators. ](https://github.com/riscv-boom/riscv-boom/commit/46fd0e109d37738f270da305a55dbf7a2d2a1502)

## Documentation / BugFinding

### Chisel IP Contributions

For all of these we would like to verify against a simple combinatorial version
as spec.

* [AES](https://github.com/hplp/aes_chisel)
* [BitonicSorter](https://github.com/freechipsproject/ip-contributions/blob/master/src/main/scala/chisel/lib/bitonicsorter/BitonicSorter.scala)
* [IterativeCordic](https://github.com/freechipsproject/ip-contributions/blob/master/src/main/scala/chisel/lib/cordic/iterative/Cordic.scala)


### RISCV Softcore Contest Entries

* [MF8 ALU](https://github.com/ekiwi/riscv-softcore-2018/blob/master/engine-v/chisel/src/addsub8.scala)
* [MF8 Register File](https://github.com/ekiwi/riscv-softcore-2018/blob/master/engine-v/hdl/mf8_reg.v)


### Rocket Chip
* [PMP](https://github.com/freechipsproject/rocket-chip/blob/master/src/main/scala/rocket/PMP.scala)
* [Decoder](https://github.com/freechipsproject/rocket-chip/blob/master/src/main/scala/rocket/Decode.scala)

