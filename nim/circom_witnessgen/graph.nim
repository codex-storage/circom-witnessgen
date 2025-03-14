
type

  UnoOp* = enum
    Neg,
    Id 

  DuoOp* = enum
    Mul,
    Div,
    Add,
    Sub,
    Pow,
    Idiv,
    Mod,
    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq,
    Land,
    Lor,
    Shl,
    Shr,
    Bor,
    Band,
    Bxor

  TresOp* = enum
    TresCond

  BigUInt* = distinct seq[uint8]

  InputNode*[T] = object
    idx*: T

  ConstantNode* = distinct BigUInt

  UnoOpNode*[T]  = object
    op*: UnoOp
    arg1*: T

  DuoOpNode*[T]  = object
    op*: DuoOp
    arg1*: T
    arg2*: T

  TresOpNode*[T] = object 
    op*: TresOp
    arg1*: T
    arg2*: T
    arg3*: T

  NodeKind* = enum Input, Const, Uno, Duo, Tres

  Node*[T] = object
    case kind*: NodeKind
      of Input: inp*:  InputNode[T]
      of Const: kst*:  ConstantNode
      of Uno:   uno*:  UnoOpNode[T]
      of Duo:   duo*:  DuoOpNode[T]
      of Tres:  tres*: TresOpNode[T]

  SignalDescription* = object
    offset*: uint32
    length*: uint32

  WitnessMapping* = distinct seq[uint32]

  CircuitInputs* = seq[(string, SignalDescription)]

  GraphMetaData* = object
    witnessMapping*: WitnessMapping
    inputSignals*:   CircuitInputs  

  Graph* = object
    nodes*: seq[Node[uint32]]
    meta*:  GraphMetaData

