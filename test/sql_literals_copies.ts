import {expect} from "chai";
import * as fs from "node:fs";
import * as path from "node:path";

// The database packages build against the published runtime and cannot
// import its sql_literals.ts yet, so each carries a copy. One function, one
// text: a copy that drifts is a client that reads values again.
describe("sql_literals.ts, the copies in the database packages", () => {
  const root = path.join(__dirname, "..", "..");
  const original = fs.readFileSync(path.join(root, "packages", "runtime", "src", "sql_literals.ts"), "utf8");

  for (const name of ["database-sqlite", "database-pg", "database-hdb", "database-snowflake"]) {
    it(name + " carries the runtime's file unchanged", () => {
      const copy = fs.readFileSync(path.join(root, "packages", name, "src", "sql_literals.ts"), "utf8");
      expect(copy).to.equal(original);
    });
  }
});
