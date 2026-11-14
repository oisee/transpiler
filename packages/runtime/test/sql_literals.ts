import {expect} from "chai";
import {outsideLiterals, splitLiterals} from "../src/sql_literals";

describe("sql literals, found once and kept verbatim", () => {
  const rewrite = (s: string) => s.replace(/ UP TO (\d+) ROWS(.*)/i, "$2 LIMIT $1")
    .replace(/ DESCENDING/ig, " DESC").replace(/~/g, ".").replace(/ LIMIT 0/g, "");

  it("a rewrite reaches the code and not the values", () => {
    const sql = "SELECT b~name FROM t AS b UP TO 5 ROWS WHERE ( b~NAME = 'a~b' OR b~NAME = 'x DESCENDING'"
      + " OR b~NAME = 'O''Hara LIMIT 0' OR b~NAME = 'x UP TO 9 ROWS' ) ORDER BY b~NAME DESCENDING";
    expect(outsideLiterals(sql, rewrite)).to.equal("SELECT b.name FROM t AS b WHERE ( b.NAME = 'a~b' OR b.NAME = 'x DESCENDING'"
      + " OR b.NAME = 'O''Hara LIMIT 0' OR b.NAME = 'x UP TO 9 ROWS' ) ORDER BY b.NAME DESC LIMIT 5");
  });

  it("an unterminated literal is data to the end", () => {
    expect(outsideLiterals("a~b = 'x~y", rewrite)).to.equal("a.b = 'x~y");
  });

  it("$ in a literal is not a replacement pattern", () => {
    expect(outsideLiterals("a~b = '$1 $& $''' AND c~d = ''", rewrite)).to.equal("a.b = '$1 $& $''' AND c.d = ''");
  });

  it("backtick literals only when asked", () => {
    expect(splitLiterals("a = `x'y` AND b = 'p``q'", "'`")).to.deep.equal([
      {literal: false, text: "a = "}, {literal: true, text: "`x'y`"},
      {literal: false, text: " AND b = "}, {literal: true, text: "'p``q'"}]);
    expect(splitLiterals("a = `x`").map(s => s.literal)).to.deep.equal([false]);
  });

  it("linear in the length of a literal", () => {
    const sql = "SELECT * FROM t WHERE a~b = '" + "a''".repeat(100000) + "'";
    const start = Date.now();
    expect(outsideLiterals(sql, rewrite)).to.equal(sql.replace("a~b", "a.b"));
    expect(Date.now() - start).to.be.below(1000);
  });
});
