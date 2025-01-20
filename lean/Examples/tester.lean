import Examples.Bool
import Lean

namespace Examples.tester
open Lean

deriving instance ToJson for IR.VarId
deriving instance ToJson for IR.IRType
deriving instance ToJson for IR.CtorInfo
deriving instance ToJson for IR.Arg
deriving instance ToJson for IR.LitVal
deriving instance ToJson for IR.Expr
deriving instance ToJson for IR.JoinPointId
deriving instance ToJson for IR.Param
deriving instance ToJson for String.Pos
deriving instance ToJson for Substring
deriving instance ToJson for SourceInfo
deriving instance ToJson for Syntax.Preresolved
deriving instance ToJson for Syntax
deriving instance ToJson for DataValue
deriving instance ToJson for KVMap
deriving instance ToJson for IR.AltCore
deriving instance ToJson for IR.FnBody
deriving instance ToJson for IR.DeclInfo
deriving instance ToJson for ExternEntry
deriving instance ToJson for ExternAttrData
deriving instance ToJson for IR.Decl

inductive Tree
  | leaf : Nat → Tree
  | node : Tree → Tree → Tree

set_option trace.compiler.ir.result true in
def sum : Tree → Tree
  | .leaf x => .leaf x
  | .node a b => .node (sum b) (sum a)

def a : Nat → Nat
    | 0 => 0
    | n+1 => n

def b : Nat → Nat
    | n => n + a n

#eval show MetaM _ from do
  let env ← getEnv
  let .some x := IR.findEnvDecl env ``b | failure
  println! toJson x
