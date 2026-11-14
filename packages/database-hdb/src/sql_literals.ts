// String literals in SQL text, found once, so that a rewrite of the text
// cannot read them.
//
// The runtime and the database clients rewrite statement text with regular
// expressions: EQ becomes =, a field symbol becomes its value, ~ becomes .,
// UP TO n ROWS becomes LIMIT n. Run over the whole statement, those
// rewrites also reach into the values: 'A EQ B' became 'A = B', 'a~b'
// became 'a.b', and 'x UP TO 9 ROWS' became 'x' with a LIMIT 9 the value
// chose. A value is data, so every rewrite runs on the statement with its
// literals masked, and the literals go back exactly as they came.
//
// A literal is single-quoted with '' for a quote inside it. Open SQL text
// (a dynamic WHERE) also has backtick literals with `` for a backtick;
// splitLiterals finds those when asked to. An unterminated literal runs to
// the end of the text and is data as well: nothing after its quote is read.
//
// The scan is one pass with indexOf, never a regular expression over the
// literal: /'((?:[^']|'')*)'/ backtracks per character and overflows the
// stack on a long value.
//
// The database packages build against the published runtime, so each
// carries a copy of this file; test/sql_literals_copies.ts holds the copies
// to this one, byte for byte.

export type SqlSegment = {
  /** true for a literal, including its quotes */
  literal: boolean,
  text: string,
};

export function splitLiterals(sql: string, quotes: string = "'"): SqlSegment[] {
  const segments: SqlSegment[] = [];
  let from = 0;
  let i = from;
  while (i < sql.length) {
    if (quotes.indexOf(sql[i]) < 0) {
      i++;
      continue;
    }
    const quote = sql[i];
    let j = i + 1;
    for (;;) {
      j = sql.indexOf(quote, j);
      if (j < 0 || sql[j + 1] !== quote) {
        break;
      }
      j += 2;
    }
    const end = j < 0 ? sql.length : j + 1;
    if (i > from) {
      segments.push({literal: false, text: sql.slice(from, i)});
    }
    segments.push({literal: true, text: sql.slice(i, end)});
    from = end;
    i = end;
  }
  if (from < sql.length) {
    segments.push({literal: false, text: sql.slice(from)});
  }
  return segments;
}

// A placeholder holds no character a rewrite looks for: no blank, no
// keyword, no ~, and it is quoted so that a rewrite still sees a value
// where the literal was. The text outside literals is code, never data, so
// what the rewrite returns holds no placeholder that masking did not make.
const PLACEHOLDER = /'\u0000(\d+)\u0000'/g;

/** Runs `rewrite` over the code of `sql` only; every single-quoted literal
 *  comes back verbatim. */
export function outsideLiterals(sql: string, rewrite: (code: string) => string): string {
  if (sql.indexOf("'") < 0) {
    return rewrite(sql);
  }
  const kept: string[] = [];
  let masked = "";
  for (const segment of splitLiterals(sql)) {
    if (segment.literal) {
      masked += "'\u0000" + kept.length + "\u0000'";
      kept.push(segment.text);
    } else {
      masked += segment.text;
    }
  }
  return rewrite(masked).replace(PLACEHOLDER, (_whole: string, n: string) => kept[Number(n)]);
}
