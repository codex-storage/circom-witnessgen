
import std/bitops
import strutils

import ./graph

#-------------------------------------------------------------------------------

proc parseVarUint64(buf: openArray[byte], p: var int): uint64 =
  let x = buf[p]
  p += 1
  if x < 128:
    return uint64(x)
  else:
    let y = buf.parseVarUint64(p)
    return uint64(x - 128) + (y shl 7)

proc parseVarUint32(buf: openArray[byte], p: var int): uint32 =
  return uint32( parseVarUint64(buf,p) )

proc parseVarInt(buf: openArray[byte], p: var int): int =
  return int( parseVarUint64(buf,p) )

proc parseUint64(buf: openArray[byte], p: var int): uint64 = 
  var x: uint64 = 0
  for i in 0..<8:
    x += uint64(buf[p+i]) shl (i*8)
  p += 8
  return x

const VARINT : byte = 0
const I64    : byte = 1
const LEN    : byte = 2
const SGROUP : byte = 3
const EGROUP : byte = 4
const I32    : byte = 5

proc parseProtoField(buf: openArray[byte], p: var int, expected: byte): int = 
  let b = buf[p]
  p = p+1
  assert expected == bitand(b,7)
  return int(b shr 3)

proc leBytesToHex(bytes: openArray[byte]): string =
  var s: string = ""
  let l = bytes.len
  for i in 0..<l:
    s = s & bytes[l-1-i].toHex;
  return s

#-------------------------------------------------------------------------------

proc parseGenericNode(buf: openArray[byte]): seq[uint32] = 
  let l = buf.len
  var p = 0
  var values: seq[uint32] = newSeq[uint32](5)
  while( p < l ):
    let i = buf.parseProtoField(p, VARINT)
    let y = buf.parseVarUint32(p)
    values[i] = y
  return values

proc parseInputNode(buf: openArray[byte]): Node[uint32] =
  # echo "InputNode"
  let values = parseGenericNode(buf)
  let node: InputNode[uint32] = InputNode[uint32](idx: values[1])
  return Node[uint32](kind: Input, inp: node)

proc parseConstantNode(buf: openArray[byte]): Node[uint32] =
  # echo "ConstantNode"
  var p = 0

  let fld = buf.parseProtoField(p, LEN)
  assert fld == 1
  let l = buf.parseVarInt(p)

  # protobuf is stupid, it's like triple wrapped
  let fld2 = buf.parseProtoField(p, LEN)
  assert fld2 == 1
  let l2 = buf.parseVarInt(p)

  var bytes: seq[byte] = newSeq[byte](l2)
  for i in 0..<l2: bytes[i] = buf[p+i]
  # echo leBytesToHex(bytes)

  let node: ConstantNode = ConstantNode(BigUInt(bytes))
  return Node[uint32](kind: Const, kst: node)

proc parseUnoOpNode(buf: openArray[byte]): Node[uint32] =
  # echo "UnoOpNode"
  let values = parseGenericNode(buf)
  let node: UnoOpNode[uint32] = UnoOpNode[uint32](op: UnoOp(values[1]), arg1: values[2])
  return Node[uint32](kind: Uno, uno: node)

proc parseDuoOpNode(buf: openArray[byte]): Node[uint32] =
  # echo "DuoOpNode"
  let values = parseGenericNode(buf)
  let node: DuoOpNode[uint32] = DuoOpNode[uint32](op: DuoOp(values[1]), arg1: values[2], arg2: values[3])
  return Node[uint32](kind: Duo, duo: node)

proc parseTresOpNode(buf: openArray[byte]): Node[uint32] =
  # echo "TresOpNode"
  let values = parseGenericNode(buf)
  let node: TresOpNode[uint32] = TresOpNode[uint32](op: TresOp(values[1]), arg1: values[2], arg2: values[3], arg3: values[4])
  return Node[uint32](kind: Tres, tres: node)

proc parseNode(buf: openArray[byte], p: var int): Node[uint32] =
  let len = buf.parseVarInt(p)
  # echo "node length = " & ($len)
  var nextp = p + len
  var fld = buf.parseProtoField(p, LEN)
  var len1 = buf.parseVarInt(p)
  var bytes = buf[p..<p+len1]
  var node: Node[uint32]
  case fld:
    of 1: node = bytes.parseInputNode()
    of 2: node = bytes.parseConstantNode()
    of 3: node = bytes.parseUnoOpNode()
    of 4: node = bytes.parseDuoOpNode()
    of 5: node = bytes.parseTresOpNode()
    else: assert false
  # echo ($node)
  p = nextp
  return node

#-------------------------------------------------------------------------------

proc parseWitnessMapping(buf: openArray[byte], p: var int): seq[uint32] = 

  let fld = buf.parseProtoField(p, LEN)
  assert fld == 1
  let l = buf.parseVarInt(p)
  let nextp = p + l

  var list: seq[uint32] = newSeq[uint32](0)
  while(p < nextp):
    let j = buf.parseVarUint32(p)
    list.add(j)

  p = nextp;
  return list

proc parseSignalDescription(buf: openArray[byte], p: var int): SignalDescription = 

  let fld = buf.parseProtoField(p, LEN)
  assert fld == 2
  let ln  = buf.parseVarInt(p)
  let nextp = p + ln
 
  var xofs: uint32 = 0
  var xlen: uint32 = 0

  while (p < nextp):
    let fld = buf.parseProtoField(p, VARINT)
    let val = buf.parseVarUint32(p)
    case fld:
      of 1: xofs = val
      of 2: xlen = val
      else: assert false

  p = nextp
  return SignalDescription(offset: xofs, length: xlen)

proc bytesToString(bytes: openarray[byte]): string =
  result = newString(bytes.len)
  copyMem(result[0].addr, bytes[0].unsafeAddr, bytes.len)  

proc parseSignalName(buf: openArray[byte], p: var int): string=

  let fld1 = buf.parseProtoField(p, LEN)
  assert fld1 == 1
  let len1 = buf.parseVarInt(p)
  let nextp1 = p + len1

  let bs = buf[p..<p+len1]
  let name = bytesToString(bs)

  p = nextp1
  return name
    
proc parseCircuitInput(buf: openArray[byte], p: var int): (string, SignalDescription) =

  let fld = buf.parseProtoField(p, LEN)
  assert fld == 2
  let l = buf.parseVarInt(p)
  let nextp = p + l

  # name
  let name = buf.parseSignalName(p)

  # (ofs,length)
  let desc = buf.parseSignalDescription(p)

  p = nextp
  return (name,desc)

proc parseMeta(buf: openArray[byte]): GraphMetaData = 
  var p: int = 0

  let mapping = buf.parseWitnessMapping(p)

  var entries: seq[(string, SignalDescription)] = newSeq[(string, SignalDescription)](0)
  while(p < buf.len):
    let entry = buf.parseCircuitInput(p)
    entries.add(entry)

  return GraphMetaData(witnessMapping: WitnessMapping(mapping), inputSignals: entries)

#-------------------------------------------------------------------------------

proc parseGraph*(buf: openArray[byte]): Graph = 
  var p: int = 0

  let magic  = "wtns.graph.001" 
  for i in 0..<magic.len:
    assert ord(magic[i]) == int(buf[i])
  p += magic.len

  var nnodes: uint64 = buf.parseUint64(p)

  # echo "magic  = " & ($magic)
  # echo "nnodes = " & ($nnodes)

  var nodes: seq[Node[uint32]] = newSeq[Node[uint32]](0)
  for k in 0..<nnodes:
    let node = buf.parseNode(p)
    nodes.add(node)

  let meta_len = buf.parseVarInt(p)
  let meta = parseMeta(buf[p..<p+meta_len])

  return Graph(nodes: nodes, meta: meta)

proc loadGraph*(fname: string): Graph=
  let f = fname.open(fmRead)
  let fileSize = f.getFileSize()
  var bytes: seq[byte] = newSeq[byte](fileSize)
  let amountRead = f.readBytes(bytes, 0, filesize)
  assert amountRead == fileSize
  f.close()
  let graph = parseGraph(bytes)
  return graph

#-------------------------------------------------------------------------------
