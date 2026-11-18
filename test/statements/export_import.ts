import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

async function runAndGet(code: string) {
  const js = await run(code);
  const f = new AsyncFunction("abap", js);
  await f(abap);
  return abap.console.get();
}

describe("Running statements - EXPORT/IMPORT, SHARED MEMORY and SHARED BUFFER", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("IMPORT of a key nothing was exported to, sy-subrc 4 and the target unchanged", async () => {
    const code = `
    DATA lv TYPE i VALUE 7.
    IMPORT val = lv FROM SHARED MEMORY indx(zz) ID 'NOTHING_HERE_1'.
    WRITE / sy-subrc.
    WRITE / lv.`;
    expect(await runAndGet(code)).to.equal("4\n7");
  });

  it("EXPORT then IMPORT, a table of structures", async () => {
    const code = `
    TYPES: BEGIN OF ty_row,
             id   TYPE c LENGTH 10,
             text TYPE string,
           END OF ty_row.
    TYPES ty_rows TYPE SORTED TABLE OF ty_row WITH UNIQUE KEY id.
    DATA lt_out TYPE ty_rows.
    DATA lt_in TYPE ty_rows.
    DATA ls TYPE ty_row.
    ls-id = 'B'. ls-text = |second|. INSERT ls INTO TABLE lt_out.
    ls-id = 'A'. ls-text = |first|. INSERT ls INTO TABLE lt_out.
    EXPORT rows = lt_out TO SHARED MEMORY indx(zk) ID 'ROUND_TRIP_1'.
    CLEAR lt_out.
    IMPORT rows = lt_in FROM SHARED MEMORY indx(zk) ID 'ROUND_TRIP_1'.
    WRITE / sy-subrc.
    WRITE / lines( lt_in ).
    LOOP AT lt_in INTO ls.
      WRITE / ls-text.
    ENDLOOP.`;
    expect(await runAndGet(code)).to.equal("0\n2\nfirst\nsecond");
  });

  it("the stored value is a copy, on both sides", async () => {
    const code = `
    DATA lv TYPE string.
    DATA lv_in TYPE string.
    lv = |before|.
    EXPORT v = lv TO SHARED MEMORY indx(zk) ID 'COPY_1'.
    lv = |after|.
    IMPORT v = lv_in FROM SHARED MEMORY indx(zk) ID 'COPY_1'.
    WRITE / lv_in.
    lv_in = |changed|.
    IMPORT v = lv_in FROM SHARED MEMORY indx(zk) ID 'COPY_1'.
    WRITE / lv_in.`;
    expect(await runAndGet(code)).to.equal("before\nbefore");
  });

  it("several parameters, short form and FROM / TO forms, a parameter the cluster does not hold", async () => {
    const code = `
    DATA a TYPE i VALUE 1.
    DATA b TYPE i VALUE 2.
    DATA c TYPE i VALUE 3.
    DATA x TYPE i.
    DATA y TYPE i.
    DATA z TYPE i VALUE 99.
    EXPORT a p_b FROM b TO SHARED BUFFER indx(zk) ID 'FORMS_1'.
    CLEAR a.
    IMPORT p_b TO x c INTO z FROM SHARED BUFFER indx(zk) ID 'FORMS_1'.
    WRITE / sy-subrc.
    IMPORT a FROM SHARED BUFFER indx(zk) ID 'FORMS_1'.
    WRITE / a.
    WRITE / x.
    WRITE / z.`;
    expect(await runAndGet(code)).to.equal("0\n1\n2\n99");
  });

  it("a later EXPORT replaces the whole cluster", async () => {
    const code = `
    DATA a TYPE i VALUE 1.
    DATA b TYPE i VALUE 2.
    DATA x TYPE i VALUE 5.
    EXPORT a = a b = b TO SHARED MEMORY indx(zk) ID 'REPLACE_1'.
    EXPORT a = b TO SHARED MEMORY indx(zk) ID 'REPLACE_1'.
    IMPORT a = a b = x FROM SHARED MEMORY indx(zk) ID 'REPLACE_1'.
    WRITE / a.
    WRITE / x.`;
    expect(await runAndGet(code)).to.equal("2\n5");
  });

  it("SHARED MEMORY and SHARED BUFFER are two stores, and the area and the ID are part of the key", async () => {
    const code = `
    DATA a TYPE i VALUE 1.
    EXPORT a = a TO SHARED MEMORY indx(zk) ID 'KEY_1'.
    IMPORT a = a FROM SHARED BUFFER indx(zk) ID 'KEY_1'.
    WRITE / sy-subrc.
    IMPORT a = a FROM SHARED MEMORY indx(zz) ID 'KEY_1'.
    WRITE / sy-subrc.
    IMPORT a = a FROM SHARED MEMORY indx(zk) ID 'KEY_2'.
    WRITE / sy-subrc.
    IMPORT a = a FROM SHARED MEMORY indx(zk) ID 'KEY_1'.
    WRITE / sy-subrc.`;
    expect(await runAndGet(code)).to.equal("4\n4\n4\n0");
  });

  it("CLIENT is part of the key, the default is sy-mandt; a work area is accepted", async () => {
    const code = `
    DATA a TYPE i VALUE 1.
    DATA wa TYPE c LENGTH 10.
    EXPORT a = a TO SHARED MEMORY indx(zk) FROM wa CLIENT '777' ID 'CLIENT_1'.
    IMPORT a = a FROM SHARED MEMORY indx(zk) ID 'CLIENT_1'.
    WRITE / sy-subrc.
    IMPORT a = a FROM SHARED MEMORY indx(zk) TO wa CLIENT '777' ID 'CLIENT_1'.
    WRITE / sy-subrc.`;
    expect(await runAndGet(code)).to.equal("4\n0");
  });

  it("the ID from a variable, trailing blanks do not count", async () => {
    const code = `
    CONSTANTS c_id TYPE c LENGTH 32 VALUE 'VARIABLE_ID_1'.
    DATA lv_id TYPE string VALUE 'VARIABLE_ID_1'.
    DATA a TYPE i VALUE 42.
    DATA b TYPE i.
    EXPORT a = a TO SHARED MEMORY indx(zk) ID c_id.
    IMPORT a = b FROM SHARED MEMORY indx(zk) ID lv_id.
    WRITE / b.`;
    expect(await runAndGet(code)).to.equal("42");
  });

  it("from a class: a static method exports, another imports", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CONSTANTS c_id TYPE c LENGTH 32 VALUE 'CLASS_1'.
    CLASS-METHODS put IMPORTING iv TYPE string.
    CLASS-METHODS get RETURNING VALUE(rv) TYPE string.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD put.
    EXPORT text = iv TO SHARED MEMORY indx(zk) ID c_id.
  ENDMETHOD.
  METHOD get.
    IMPORT text = rv FROM SHARED MEMORY indx(zk) ID c_id.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>put( |hello| ).
  WRITE / lcl=>get( ).`;
    expect(await runAndGet(code)).to.equal("hello");
  });

  it("other media still throw at runtime", async () => {
    const code = `
    DATA a TYPE i.
    EXPORT a = a TO MEMORY ID 'X'.`;
    let message = "";
    try {
      await runAndGet(code);
    } catch (e) {
      message = String((e as Error).message);
    }
    expect(message).to.contain("transpiler todo");
  });

});
