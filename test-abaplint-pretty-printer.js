#!/usr/bin/env node

const abaplint = require("@abaplint/core");

// Test cases
const TEST_CASES = [
  {
    name: "Simple statements",
    abap: `DATA lv_test TYPE string.
lv_test = 'Hello'.
WRITE lv_test.`
  },
  {
    name: "DATA with colons",
    abap: `DATA: lv_first TYPE string,
      lv_second TYPE i,
      lv_third TYPE c LENGTH 10.`
  },
  {
    name: "Inline DATA declaration",
    abap: `DATA(lv_result) = calculate( ).
DATA(lv_msg) = |Hello World|.`
  },
  {
    name: "Method calls",
    abap: `lo_obj->method( ).
cl_class=>static( param ).`
  }
];

function testAbaplintPrettyPrinter() {
  console.log("🔍 Testing abaplint's Built-in Pretty Printer");
  console.log("=" .repeat(70));
  
  for (const testCase of TEST_CASES) {
    console.log(`\nTest: ${testCase.name}`);
    console.log("-".repeat(40));
    console.log("Original:");
    console.log(testCase.abap);
    
    try {
      // Create registry and add file
      const reg = new abaplint.Registry();
      const file = new abaplint.MemoryFile("test.prog.abap", testCase.abap);
      reg.addFile(file);
      reg.parse();
      
      // Get the parsed file
      const obj = reg.getFirstObject();
      const abapFile = obj?.getABAPFiles()[0];
      
      if (abapFile) {
        // Try to use the PrettyPrinter
        const config = new abaplint.Config();
        const printer = new abaplint.PrettyPrinter(abapFile, config);
        const formatted = printer.run();
        
        console.log("\nFormatted by abaplint:");
        console.log(formatted);
      }
    } catch (error) {
      console.log(`Error: ${error.message}`);
    }
  }
}

testAbaplintPrettyPrinter();
