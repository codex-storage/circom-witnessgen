
import std/tables
import std/strformat

import ./field
import ./graph
import ./semantics

#-------------------------------------------------------------------------------

proc expandInputs*(circuitInputs: seq[(string, SignalDescription)] , inputs: Inputs): Table[int, F] =
  var table: Table[int, F]
  table[0] = oneF
  for (key, desc) in circuitInputs:
    let k: int = int(desc.length)
    let o: int = int(desc.offset)
    assert( inputs.hasKey(key) , "input signal `" & key & "` not present" )
    let list: seq[F] = inputs[key]
    assert( list.len == k , "input signal `" & key & "` has unexpected size" )
    for i in 0..<k:
      table[o + i] = list[i]
      # echo "input value " & (fToDecimal(list[i])) & " at offset " & ($(o+i))
  return table

# note: this contains temporary values which are not present in the actual witness
proc generateFullComputation*(graph: Graph, inputs: Inputs): seq[F] {.gcsafe.} =

  let sequence      : seq[Node[uint32]]                = graph.nodes
  let graphMeta     : GraphMetaData                    = graph.meta
  let circuitInputs : seq[(string, SignalDescription)] = graphMeta.inputSignals

  let inpTable = expandInputs(circuitInputs, inputs)
  var output: seq[F] = newSeq[F]( sequence.len )

  for (i, node_orig) in sequence.pairs():
    let node: Node[F] = fmap[uint32,F]( proc (idx: uint32): F = output[int(idx)] , node_orig )
    output[i] = evalNode( inpTable , node )
    # echo "index = " & ($i) & " -> " & (showNodeUint32(node_orig))

  return output

#[
    let node_orig = sequence[i]
    let o = 32*i
    let hexo = fmt"{o=:x}"
    if (i == 11838):    # o == 0x4fcc:
      echo " i = " & ($i) & " | ofs = " & hexo & " | node = " & ($node_orig)
      case node_orig.kind:
        of Duo:
          echo node_orig.duo.arg1
          echo node_orig.duo.arg2
          echo fToDecimal(output[int(node_orig.duo.arg1)])
          echo fToDecimal(output[int(node_orig.duo.arg2)])
        else:
          discard
      echo "result = " & fToDecimal(output[i])
      echo " "
]#

proc generateWitness*(graph: Graph, inputs: Inputs): seq[F] {.gcsafe.} =
  let mapping: seq[uint32] = graph.meta.witnessMapping.mapping
  let pre_witness = generateFullComputation(graph, inputs)
  var output: seq[F] = newSeq[F](mapping.len)
  for (j, idx) in mapping.pairs():
    output[j] = pre_witness[int(idx)]
    # echo " - " & ($j) & " -> " & fToDecimal(output[j]) & " | from " & ($idx)
  return output


#-------------------------------------------------------------------------------
