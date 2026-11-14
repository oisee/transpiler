import {expandIN} from "./expand_in";
import {sqlEscape} from "./sql_escape";
import {splitLiterals} from "./sql_literals";
import {throwError} from "./throw_error";
import {FieldSymbol, HashedTable, Table} from "./types";

type DynamicCondition = string | Table | HashedTable | {get(): string};
type Evaluate = (name: string) => FieldSymbol | undefined;

// A dynamic condition is Open SQL text. Its code is rewritten (EQ, field
// symbols); its literals are values and are never read: a field symbol
// named inside 'q <fs> r' is three words of a value, not a host variable,
// and putting the value of <fs> there split the literal open. Literals are
// found once (splitLiterals), and a backtick literal, which Open SQL allows
// and SQL reads as an identifier, goes on single-quoted.
export function expandDynamic(input: DynamicCondition, evaluate: Evaluate) {
  let code: string;
  if (input instanceof Table || input instanceof HashedTable) {
    code = input.array().map(row => row.get()).join(" ");
  } else if (typeof input === "string") {
    code = input;
  } else {
    code = input.get();
  }

  if (code === "") {
    return "1 = 1";
  }

  let out = "";
  for (const segment of splitLiterals(code, "'`")) {
    if (segment.literal === false) {
      // yea, this is also wrong,
      // todo more here, these are just the simple cases.
      out += substitute(segment.text.replace(/ EQ /g, " = "), evaluate);
    } else if (segment.text.startsWith("`")) {
      out += backtickToSql(segment.text);
    } else {
      out += segment.text;
    }
  }
  return out;
}

function backtickToSql(literal: string): string {
  const closed = literal.length > 1 && literal.endsWith("`");
  const inner = literal.slice(1, closed ? -1 : undefined).replace(/``/g, "`");
  return "'" + sqlEscape(inner) + (closed ? "'" : "");
}

function isWord(c: string | undefined): boolean {
  return c !== undefined && /\w/.test(c);
}

// Field symbols in code: ` <fs>`, ` @<fs>`, ` <fs>-comp`, and `field IN <fs>-comp`
// for a range. One pass: a value put in is never scanned again, so a value
// holding " <x>" is data, and the result is built by concatenation so that
// "$'", "$&" and the like in a value are not read as replacement patterns.
// The optional `field IN` before a match is looked up backwards from it
// rather than tried at every position, which made a long word quadratic.
function substitute(code: string, evaluate: Evaluate): string {
  const regex = / @?<(\w+)>(?:-(\w+))?/g;
  let out = "";
  let from = 0;
  let match: RegExpExecArray | null;
  while ((match = regex.exec(code)) !== null) {
    const fs = match[1];
    const component = match[2];
    let start = match.index;
    let field: string | undefined = undefined;
    if (start - 4 >= from && code.startsWith(" IN", start - 3) && isWord(code[start - 4])) {
      let begin = start - 4;
      while (begin > from && isWord(code[begin - 1])) {
        begin--;
      }
      field = code.slice(begin, start - 3);
      start = begin;
    }

    const name = ("fs_" + fs + "_").toLowerCase();
    const found = evaluate(name);
    if (!(found instanceof FieldSymbol)) {
      continue;
    }
    if (found.isAssigned() === false) {
      // a system cannot read an unassigned field symbol either; it is a
      // semantic error of the dynamic condition, and catchable as one
      throwError("CX_SY_DYNAMIC_OSQL_SEMANTICS");
    }

    let value: any = found;
    if (component !== undefined) {
      value = found.get()?.[component.toLowerCase()];
      if (value === undefined) {
        throwError("CX_SY_DYNAMIC_OSQL_SEMANTICS");
      }
    }
    let replacement: string;
    if (component !== undefined && field !== undefined) {
      replacement = expandIN(field.toLowerCase(), value);
    } else {
      replacement = (field !== undefined ? field + " IN" : "") + " '" + sqlEscape(value.get()) + "'";
    }
    out += code.slice(from, start) + replacement;
    from = match.index + match[0].length;
  }
  return out + code.slice(from);
}
