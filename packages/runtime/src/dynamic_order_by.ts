// The text of a dynamic ORDER BY, as SQL takes it.
//
// Open SQL separates the columns of an ORDER BY with blanks or with commas:
// `ORDER BY (lv)` with lv = `s~so_id i~so_item_pos` or `a DESCENDING b` is
// legal, and so is `a, b`. SQL wants the commas. The static clause gets them
// from the transpiler (SQLOrderByTranspiler); a dynamic one is only known at
// runtime, so it is normalised here: every column is one item, an
// ASCENDING / DESCENDING (or ASC / DESC) belongs to the column before it,
// and the items are joined with ", ". PRIMARY KEY is left as it is, the
// database clients expand it. A text holding a parenthesis or a quote is
// not a plain column list and is passed on untouched.

const DIRECTIONS = new Set(["ASCENDING", "DESCENDING", "ASC", "DESC"]);

export function dynamicOrderBy(text: string): string {
  const trimmed = text.trim();
  if (trimmed === "" || /^PRIMARY\s+KEY$/i.test(trimmed) || /[()'"`]/.test(trimmed)) {
    return text;
  }
  const items: string[] = [];
  for (const group of trimmed.split(",")) {
    for (const token of group.trim().split(/\s+/)) {
      if (token === "") {
        continue;
      }
      if (DIRECTIONS.has(token.toUpperCase()) && items.length > 0) {
        items[items.length - 1] += " " + token;
      } else {
        items.push(token);
      }
    }
  }
  return items.join(", ");
}
