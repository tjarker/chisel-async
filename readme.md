Requires an installed JDK and SBT to be run.


- `src/main/scala/async`: contains general asynchronous handshake channel definitions and dataflow blocks.
- `src/main/scala/async/examples`: contains a Fibonacci, FIFO and GCD example
- `src/main/scala/noc`: contains the NoC library with demux, arb-merge, router and NoC generator
- `src/main/scala/noc/examples`: contains a simple NoC connecting one sender and multiple receivers on a 4x4 grid
- `src/test/scala`: contains all tests


- In order to generate the verilog for a 4x4 NoC run `sbt "runMain noc.NOC"`.
- In order to run all tests execute `sbt test`.


