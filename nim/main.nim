
import std/sequtils

import circom_witnessgen/field
import circom_witnessgen/graph
import circom_witnessgen/load
import circom_witnessgen/input_json
import circom_witnessgen/export_wtns

#-------------------------------------------------------------------------------

const graph_file: string = "../tmp/graph4.bin"
const input_file: string = "../tmp/input4.json"
const wtns_file:  string = "../tmp/nim4.wtns"

#-------------------------------------------------------------------------------

when isMainModule:

  echo "\nloading in " & input_file
  let inp = loadInputJSON(input_file) 
  # printInputs(inp)

  echo "\nloading in " & graph_file
  let gr = loadGraph(graph_file)
  # echo $gr

  let us: seq[int] = @[1,2,3,4,5,6,7]
  let wtns: seq[F] = us.map(intToF);
  exportWitness(wtns_file, wtns)