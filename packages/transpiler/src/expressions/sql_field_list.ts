import {Nodes} from "@abaplint/core";
import * as abaplint from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {SQLFieldTranspiler} from "./sql_field";
import {SQLFieldNameTranspiler} from "./sql_field_name";

export class SQLFieldListTranspiler implements IExpressionTranspiler {

  /**
   * positional: give every column without an alias of its own the alias p0, p1, ...
   * by its position, so two columns of the same name (a~id b~id) stay two
   * columns in the name-keyed rows the database clients return
   */
  public transpile(node: Nodes.ExpressionNode, traversal: Traversal, positional = false): Chunk {

    const fields: string[] = [];
    const aliased: boolean[] = [];
    let aliasable = true;
    let pendingAs = false;
    for (const f of node.getChildren() || []) {
      if (f instanceof abaplint.Nodes.ExpressionNode && f.get() instanceof abaplint.Expressions.SQLField) {
        const code = new SQLFieldTranspiler().transpile(f, traversal).getCode();
        fields.push(code);
        aliased.push(f.findDirectExpression(abaplint.Expressions.SQLAsName) !== undefined);
      } else if (f instanceof abaplint.Nodes.ExpressionNode && f.get() instanceof abaplint.Expressions.SQLFieldName) {
        const code = new SQLFieldNameTranspiler().transpile(f, traversal).getCode();
        fields.push(code);
        aliased.push(false);
      } else if (f instanceof abaplint.Nodes.ExpressionNode && f.get() instanceof abaplint.Expressions.SQLFieldList) {
        // todo, I have no idea why its nested like this when there is just one field
        return new SQLFieldListTranspiler().transpile(f, traversal, positional);
      } else if (positional && fields.length > 0
          && f instanceof abaplint.Nodes.TokenNode && f.concatTokens().toUpperCase() === "AS") {
        // the loop field lists carry "AS name" beside the entry it names
        pendingAs = true;
      } else if (positional && pendingAs
          && f instanceof abaplint.Nodes.ExpressionNode && f.get() instanceof abaplint.Expressions.SQLAsName) {
        fields[fields.length - 1] = fields[fields.length - 1].trimEnd() + " AS " + f.concatTokens();
        aliased[aliased.length - 1] = true;
        pendingAs = false;
      } else {
        const concat = f.concatTokens();
        if (concat !== ",") {
          fields.push(concat);
          aliased.push(true);
          // "*", a dynamic list, a host variable, a constant: not aliased; the
          // runtime refuses a row whose column count does not fit the targets
          aliasable = false;
        }
      }
    }

    if (positional && aliasable) {
      for (let index = 0; index < fields.length; index++) {
        if (aliased[index] === false) {
          fields[index] = fields[index].trimEnd() + ` AS \\"p${index}\\"`;
        }
      }
    }

    return new Chunk().append(fields.join(", "), node, traversal);
  }

}
