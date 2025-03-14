
import circom_witnessgen/graph
import circom_witnessgen/load

when isMainModule:
  let fn = "/Users/bkomuves/zk/codex/circom-witnessgen-compiler/tmp/graph2.bin"
  echo "loading in " & fn
  let g = loadGraph(fn)
  echo $g
