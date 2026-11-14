import {expect} from "chai";
import {ABAP} from "../src";
import {Character, FieldSymbol, Structure} from "../src/types";

// A dynamic WHERE arrives as text. What expandDynamic rewrites in it (EQ,
// host variables, field symbols) is code; a quoted literal is data and comes
// out exactly as it went in.
describe("expandDynamic, literals are data", () => {
  const abap = new ABAP();

  // @ts-ignore
  global.abap = abap;

  const chars = (value: string) => {
    const c = new Character(value.length);
    c.set(value);
    return c;
  };
  const assigned = (value: string) => {
    const fs = new FieldSymbol(new Character(value.length));
    fs.assign(chars(value));
    return fs;
  };
  const scope = (symbols: {[name: string]: FieldSymbol}) => (name: string) => symbols[name];

  it("an assigned field symbol named inside a literal is not substituted", () => {
    const evaluate = scope({fs_ls_key_: assigned("OR 1 = 1 OR NAME =")});
    expect(abap.expandDynamic("NAME = 'q <ls_key> r'", evaluate)).to.equal("NAME = 'q <ls_key> r'");
  });

  it("a field symbol outside a literal still is", () => {
    const evaluate = scope({fs_ls_key_: assigned("it's")});
    expect(abap.expandDynamic("NAME = <ls_key> OR NAME = '<ls_key>'", evaluate))
      .to.equal("NAME = 'it''s' OR NAME = '<ls_key>'");
  });

  it("EQ inside a literal is a word of the value", () => {
    expect(abap.expandDynamic("NAME EQ 'A EQ B'", scope({}))).to.equal("NAME = 'A EQ B'");
  });

  it("a doubled quote does not end the literal", () => {
    const evaluate = scope({fs_x_: assigned("boom")});
    expect(abap.expandDynamic("NAME = 'O'' <x> EQ '' r'", evaluate)).to.equal("NAME = 'O'' <x> EQ '' r'");
  });

  it("a backtick literal is a literal too, and reaches SQL single-quoted", () => {
    const evaluate = scope({fs_x_: assigned("boom")});
    expect(abap.expandDynamic("NAME = `a <x> EQ ``b'` AND X EQ <x>", evaluate))
      .to.equal("NAME = 'a <x> EQ `b''' AND X = 'boom'");
  });

  it("an unassigned field symbol outside a literal is an ABAP exception, not a TypeError", () => {
    const evaluate = scope({fs_ls_order_: new FieldSymbol(new Structure({order_id: new Character(10)}))});
    for (const text of ["ORDER_ID = <ls_order>", "ORDER_ID = <ls_order>-order_id"]) {
      let caught: any = undefined;
      try {
        abap.expandDynamic(text, evaluate);
      } catch (e) {
        caught = e;
      }
      expect(caught, text).to.not.equal(undefined);
      expect(caught, text).to.not.be.instanceOf(TypeError);
      expect(String(caught?.message ?? caught), text).to.contain("CX_SY_DYNAMIC_OSQL_SEMANTICS");
    }
  });

  it("an unassigned field symbol named inside a literal is not touched", () => {
    const evaluate = scope({fs_ls_order_: new FieldSymbol(new Structure({order_id: new Character(10)}))});
    expect(abap.expandDynamic("BUYER_ID = 'q <ls_order>-order_id r'", evaluate))
      .to.equal("BUYER_ID = 'q <ls_order>-order_id r'");
  });

  it("a long literal and a long word take linear time", () => {
    for (const text of [
      "NAME = '" + "a".repeat(200000) + "'",
      "NAME = " + "a".repeat(200000),
      "NAME IN " + "a ".repeat(100000),
    ]) {
      const start = Date.now();
      abap.expandDynamic(text, scope({}));
      expect(Date.now() - start, text.slice(0, 12)).to.be.below(1000);
    }
  });
});
