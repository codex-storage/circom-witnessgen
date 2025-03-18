
Circom witness generators
-------------------------

The original idea behind this small project was to take the "computation graph" 
files generated by [`circom-witnesscalc`](https://github.com/iden3/circom-witnesscalc),
and either interpret or compile them to various algebra backends.

While this is quite straightforward in principle, and seems to work on particular
examples, it turns out that `circom-witnesscalc` itself has some serious limitations.

The biggest one is that it doesn't support any kind of dynamic computation (which should 
be obvious from the fact that it produces a static graph). But even if one uses only 
static computations, there are further very annoying things which just don't work.

However at the end this is still a useful thing, as many circuits still work.

### Compiler

Not sure how useful the compiler idea is after all, instead of interpreting the graph
directly. For large(r) circuits you will have millions of function calls, which
may strain the target language's compiler.

The main advantage seems to be that you don't have to re-implement the parser, and
you won't need to ship a binary graph file.

### Implementation status

Haskell witness generator:

- [x] parsing the graph file
- [x] parsing json input
- [x] naive interpreter
- [x] exporting the witness
- [ ] Cabalize
- [ ] refactor the parser to be nicer
- [ ] use high-performance algebra

Haskell compiler:

- [ ] constantine backend
- [ ] zikkurat backend
- [ ] arkworks backend

Nim witness generator (to be used with [`nim-groth16`](https://github.com/codex-storage/nim-groth16))

- [x] parsing the graph file
- [x] parsing json input
- [x] generating the witness
- [x] exporting the witness
- [ ] support the complete set of operations
- [ ] proper error handling

### Testing & correctness

I haven't yet done any proper testing, apart from "works for our purposes".

Known bugs:

 - comparison ignores the "signed" semantics of circom
 - integer division and modulo is not implemented

### Circuit optimizations

NOTE: you _have to_ run `circom` with the `--O2` options, otherwise the 
witness format will be most probably incompatible with the the one generated
by `circom-witnesscalc`.

### Graph file format

`circom-witnesscalc` produces binary files encoding a computation graph.

This has the following format:

- magic header: `"wtns.graph.001"` (14 bytes)
- number of nodes (8 bytes little endian)
- list of nodes, in protobuf serialized format. Each one is prefixed by a `varint` length
- after that, `GraphMetaData`, encoded with protobuf 
- finally, 8 bytes little-endian  offset (yes, _at the very end_...), pointing to the start of `GraphMetaData`

Node format:

- varint length prefix
- protobuf tag-byte (lower 3 bits are `0x02`, upper bits are node type `1..5`)
- varint length 
- several record fields
    - protobuf tag-byte (lower 3 bits are `0x00`, upper bits are field index `1,2,3,4`)
    - value is varint word32, except for `ConstantNode` when it's a sequence of little-endian bytes,
      encoding a field element (wrapped a few times, because protobuf is being protobuf...)
