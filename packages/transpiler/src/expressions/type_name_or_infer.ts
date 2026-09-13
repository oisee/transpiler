import {AbstractType, BasicTypes, Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TranspileTypes} from "../transpile_types";

// The built-in types that name themselves completely: CONV f( ) says
// everything about the type it wants, where CONV c( ) would not.
//
// Why they are here at all: the type normally comes from the InferredType
// reference the syntax check recorded, and for a second CONV in the same
// expression there is none. "DATA(lv) = CONV f( '1' ) + CONV f( 2 )."
// records one reference and needs two, so the second one used to end the
// transpile with "type not found: f" on code the syntax check passed. A
// name that resolves to exactly one type does not need a reference to be
// understood.
const BUILT_IN: {[name: string]: () => AbstractType} = {
  "i": () => BasicTypes.IntegerType.get(),
  "int8": () => new BasicTypes.Integer8Type(),
  "f": () => new BasicTypes.FloatType(),
  "string": () => BasicTypes.StringType.get(),
  "xstring": () => BasicTypes.XStringType.get(),
  "d": () => new BasicTypes.DateType(),
  "t": () => new BasicTypes.TimeType(),
  "utclong": () => new BasicTypes.UTCLongType(),
  "decfloat16": () => new BasicTypes.DecFloat16Type(),
  "decfloat34": () => new BasicTypes.DecFloat34Type(),
};

export class TypeNameOrInfer implements IExpressionTranspiler {

  public findType(node: Nodes.ExpressionNode, traversal: Traversal): AbstractType {
    const scope = traversal.findCurrentScopeByToken(node.getFirstToken());
    const type = traversal.lookupInferred(node, scope);

    if (type === undefined) {
        if (node.concatTokens() === "#") {
      const sqlType = traversal.getSQLInferredType();
      if (sqlType !== undefined) {
        return sqlType;
      }
    }
      const builtIn = BUILT_IN[node.concatTokens().toLowerCase()];
      if (builtIn !== undefined) {
        return builtIn();
      }
      throw new Error("TypeNameOrInfer, type not found: " + node.concatTokens() + ", " + traversal.getCurrentObject().getName() + ", " + traversal.getFilename() + " line " + node.getFirstToken().getStart().getRow());
    }

    return type;
  }

  public findTypeOrUndefined(node: Nodes.ExpressionNode, traversal: Traversal): AbstractType | undefined {
    const scope = traversal.findCurrentScopeByToken(node.getFirstToken());
    const type = traversal.lookupInferred(node, scope);
    return type;
  }

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const type = this.findType(node, traversal);

    const ret = new Chunk();
    ret.appendString(TranspileTypes.toType(type));
    return ret;
  }

}
