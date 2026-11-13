import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {SelectTranspiler as SelectStatementTranspiler} from "../statements/select";
import {Chunk} from "../chunk";
import {UniqueIdentifier} from "../unique_identifier";
import {SQLSourceTranspiler, SQLTargetTranspiler} from "../expressions";

export class SelectTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    const ret = new Chunk();
    const selectStatement = node.findFirstStatement(abaplint.Statements.SelectLoop);
    if (selectStatement === undefined) {
      throw "Structure, select loop not found";
    }
    const concat = selectStatement.concatTokens().toUpperCase();
    const sources = selectStatement.findAllExpressions(abaplint.Expressions.SQLFromSource);
    const from = sources[0]?.concatTokens().toUpperCase();
    const sTarget = selectStatement.findFirstExpression(abaplint.Expressions.SQLTarget);
    const intoList = selectStatement.findFirstExpression(abaplint.Expressions.SQLIntoList);

    let intoName: string = "";
    if (sTarget === undefined) {
      intoName = from!.toLowerCase();
    } else {
      intoName = new SQLTargetTranspiler().transpile(sTarget, traversal).getCode();
    }

    // note: this implementation SELECTs everything into memory, which might be bad, and sometimes not correct
    const targetName = UniqueIdentifier.get();
    const loopName = UniqueIdentifier.get();
    const countName = UniqueIdentifier.get();
    const loopOptions: string[] = [];
    let assign: string;
    if (intoList) {
      // INTO (a, b, c): the buffer row has one component per target, filled
      // from the columns left to right, and every target is assigned per row
      const targets = intoList.findDirectExpressions(abaplint.Expressions.SQLTarget)
        .map(t => new SQLTargetTranspiler().transpile(t, traversal).getCode());
      const components = targets.map((t, i) => `"c${i}": ${this.cloneOf(t)}`);
      ret.appendString(`let ${targetName} = new abap.types.Table(new abap.types.Structure({${components.join(", ")}}));\n`);
      loopOptions.push("positional: true");
      assign = targets.map((t, i) => `${t}.set(${loopName}.get().c${i});\n`).join("");
    } else {
      // one table, with or without an alias, has its row type in the DDIC
      // under its own name; a join has no single row type, so the buffer
      // takes the target's
      const table = sources.length === 1
        ? sources[0].findDirectExpression(abaplint.Expressions.DatabaseTable)?.concatTokens().toUpperCase()
        : undefined;
      if (table !== undefined) {
        ret.appendString(`let ${targetName} = new abap.types.Table(abap.DDIC["${table}"].type());\n`);
      } else if (sources.length > 1 && sTarget !== undefined) {
        ret.appendString(`let ${targetName} = new abap.types.Table(${this.cloneOf(intoName)});\n`);
      } else {
        ret.appendString(`let ${targetName} = new abap.types.Table(abap.DDIC["${from}"].type());\n`);
      }
      if (concat.includes(" INTO CORRESPONDING FIELDS OF ")) {
        assign = `abap.statements.moveCorresponding(${loopName}, ${intoName});\n`;
      } else {
        assign = `${intoName}.set(${loopName});\n`;
      }
    }
    const selectHead = new SelectStatementTranspiler().transpile(selectStatement, traversal, targetName, loopOptions);
    ret.appendChunk(selectHead.ensureStartMapping(selectStatement, traversal));

    // todo: optimize, it should do real streaming?
    const packageSize = selectStatement.findFirstExpression(abaplint.Expressions.SQLPackageSize)
      ?.findFirstExpression(abaplint.Expressions.SQLSource);
    let counted = false;
    if (packageSize) {
      const getSize = new SQLSourceTranspiler().transpile(packageSize, traversal).getCode() + ".get()";
      ret.appendString(`if (${targetName}.array().length > ${getSize}) {
  throw new Error("PACKAGE SIZED loop larger than package size not supported");
};
abap.statements.append({source: ${targetName}, target: ${intoName}, lines: true});
{\n`);
    } else {
      // sy-dbcnt counts the rows read so far, 1, 2, 3 ... in the body, and
      // stays at the last count after ENDSELECT, also when EXIT left early
      counted = true;
      ret.appendString(`let ${countName} = 0;\n`);
      ret.appendString(`for (const ${loopName} of ${targetName}.array()) {\n`);
      ret.appendString(`abap.builtin.sy.get().dbcnt.set(++${countName});\n`);
      ret.appendString(`abap.builtin.sy.get().subrc.set(0);\n`);
      ret.appendString(assign);
    }

    const body = node.findDirectStructure(abaplint.Structures.Body);
    if (body) {
      ret.appendChunk(traversal.traverse(body));
    }

    ret.appendString("}\n");
    if (counted) {
      ret.appendString(`abap.builtin.sy.get().dbcnt.set(${countName});\n`);
      ret.appendString(`abap.builtin.sy.get().subrc.set(${countName} > 0 ? 0 : 4);\n`);
    }

    return ret;
  }

  // a fresh value of the target's type, also when the target is a field symbol
  private cloneOf(code: string): string {
    return `(${code} instanceof abap.types.FieldSymbol ? ${code}.getPointer() : ${code}).clone()`;
  }

}
