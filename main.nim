
import circom_witnessgen/load
import circom_witnessgen/input_json
import circom_witnessgen/witness
import circom_witnessgen/export_wtns

#-------------------------------------------------------------------------------

const graph_file: string = "../tmp/graph3.bin"
const input_file: string = "../tmp/input3.json"
const wtns_file:  string = "../tmp/nim3.wtns"

#-------------------------------------------------------------------------------

when isMainModule:

  echo "loading in " & input_file
  let inp = loadInputJSON(input_file) 
  # printInputs(inp)

  echo "loading in " & graph_file
  let gr = loadGraph(graph_file)
  # echo $gr

  echo "generating witness"
  let wtns = generateWitness( gr, inp )
  exportWitness(wtns_file, wtns)