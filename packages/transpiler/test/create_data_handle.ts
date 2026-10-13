import {expect} from "chai";
import * as abaplint from "@abaplint/core";
import {Transpiler} from "../src";
import {UnknownTypesEnum} from "../src/types";

describe("CREATE DATA TYPE HANDLE parser compatibility", () => {
  it("traverses the handle expression and emits the kernel call", async () => {
    const reg = new abaplint.Registry().addFile(new abaplint.MemoryFile("zhandle.prog.abap", `
DATA ref TYPE REF TO data.
DATA handle TYPE REF TO object.
CREATE DATA ref TYPE HANDLE handle.`));
    const output = await new Transpiler({unknownTypes: UnknownTypesEnum.runtimeError}).run(reg);
    expect(output.objects[0].chunk.getCode()).to.contain("call({handle: handle, dref: ref})");
  });
});
