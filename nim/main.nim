
import circom_witnessgen/load
import circom_witnessgen/input_json
import circom_witnessgen/witness
import circom_witnessgen/export_wtns

#-------------------------------------------------------------------------------

const graph_file: string = "../tmp/graph4.bin"
const input_file: string = "../tmp/input4.json"
const wtns_file:  string = "../tmp/nim4.wtns"
const comp_file:  string = "../tmp/nim4_full.bin"

#-------------------------------------------------------------------------------

when isMainModule:

  echo "loading in " & input_file
  let inp = loadInputJSON(input_file) 
  # printInputs(inp)

  echo "loading in " & graph_file
  let gr = loadGraph(graph_file)
  # echo $gr

  # echo "generating full computation"
  # let comp = generateFullComputation( gr, inp )
  # exportFeltSequence(comp_file, comp)

  echo "generating witness"
  let wtns = generateWitness( gr, inp )
  exportWitness(wtns_file, wtns)