
import circom_witnessgen/graph
import circom_witnessgen/load
import circom_witnessgen/input_json

const graph_file: string = "../tmp/graph4.bin"
const input_file: string = "../tmp/input4.json"

when isMainModule:
  echo "\nloading in " & input_file
  let inp = loadInputJSON(input_file) 
  printInputs(inp)

  echo "\nloading in " & graph_file
  let gr = loadGraph(graph_file)
  # echo $gr
