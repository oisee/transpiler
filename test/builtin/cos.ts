import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - cos", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("cos()", async () => {
    const code = `ASSERT cos( 0 ) = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("cos and sin take their argument by name, also when unrecorded", async () => {
    // A built-in takes its argument as {val: x}. Inside an expression that
    // carries more than one constructor expression the syntax check records
    // the first name and not the rest, and an unrecorded built-in used to be
    // emitted positionally: sin(x) rather than sin({val: x}), so the runtime
    // read .val of undefined. The call next to it, in the same statement, was
    // correct, which is what made it hard to see.
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run RETURNING VALUE(rv) TYPE f.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD run.
    DATA lv_t TYPE f.
    DATA lv_speed TYPE f.
    lv_t = 0.
    lv_speed = 1.
    rv = CONV f( '0.5' ) + sin( lv_t * lv_speed ) * CONV f( '0.3' ) + cos( lv_t * lv_speed ).
  ENDMETHOD.
ENDCLASS.

ASSERT lcl=>run( ) = '1.5'.`;
    const js = await run(code);
    expect(js).to.not.match(/builtin\.(sin|cos)\((?!\{)/);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
