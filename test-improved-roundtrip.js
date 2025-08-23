#!/usr/bin/env node

/**
 * Test Improved Round-Trip with Smart Pretty Printer
 */

const abaplint = require("@abaplint/core");
const { SmartABAPPrettyPrinter } = require("./smart-pretty-printer.js");

// Test cases
const TEST_CASES = [
  {
    name: "Simple Working Case",
    abap: `DATA lv_test TYPE string.
lv_test = 'Hello'.
WRITE lv_test.`
  },
  {
    name: "DATA with colons (Previously Failed)",
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
  },
  {
    name: "Control Flow",
    abap: `IF lv_value > 0.
  WRITE 'Positive'.
ELSE.
  WRITE 'Non-positive'.
ENDIF.`
  }
];

function parseABAP(source, filename = "test.prog.abap") {
  const file = new abaplint.MemoryFile(filename, source);
  const reg = new abaplint.Registry();
  reg.addFile(file);
  reg.parse();
  
  const obj = reg.getFirstObject();
  if (!obj) throw new Error("No object found");
  
  const abapFile = obj.getABAPFiles()[0];
  if (!abapFile) throw new Error("No ABAP file found");
  
  return {
    file: abapFile,
    statements: abapFile.getStatements(),
    structure: abapFile.getStructure()
  };
}

function compareASTs(ast1, ast2) {
  const stmts1 = ast1.statements.map(s => ({
    type: s.get().constructor.name,
    tokens: s.getTokens().map(t => t.getStr().toUpperCase())
  }));
  
  const stmts2 = ast2.statements.map(s => ({
    type: s.get().constructor.name,
    tokens: s.getTokens().map(t => t.getStr().toUpperCase())
  }));
  
  if (stmts1.length !== stmts2.length) {
    return {
      equal: false,
      reason: `Statement count: ${stmts1.length} vs ${stmts2.length}`
    };
  }
  
  for (let i = 0; i < stmts1.length; i++) {
    if (stmts1[i].type !== stmts2[i].type) {
      return {
        equal: false,
        reason: `Statement ${i+1} type: ${stmts1[i].type} vs ${stmts2[i].type}`
      };
    }
    
    const tokens1 = stmts1[i].tokens.join(" ");
    const tokens2 = stmts2[i].tokens.join(" ");
    
    if (tokens1 !== tokens2) {
      return {
        equal: false,
        reason: `Statement ${i+1} tokens differ`,
        details: { tokens1, tokens2 }
      };
    }
  }
  
  return { equal: true };
}

function testRoundTrip(testCase) {
  console.log(`\n${"=".repeat(60)}`);
  console.log(`Test: ${testCase.name}`);
  console.log("=".repeat(60));
  
  try {
    // Step 1: Parse original
    console.log("\n📝 Original ABAP:");
    console.log(testCase.abap);
    
    const ast1 = parseABAP(testCase.abap);
    console.log(`✓ Parsed: ${ast1.statements.length} statements`);
    
    // Step 2: Pretty print with SMART printer
    const smartPrinter = new SmartABAPPrettyPrinter();
    const regenerated = smartPrinter.print(ast1);
    
    console.log("\n🔄 Regenerated ABAP (Smart Printer):");
    console.log(regenerated);
    
    // Step 3: Re-parse
    const ast2 = parseABAP(regenerated);
    console.log(`✓ Re-parsed: ${ast2.statements.length} statements`);
    
    // Step 4: Compare
    const comparison = compareASTs(ast1, ast2);
    
    if (comparison.equal) {
      console.log("\n✅ SUCCESS: ASTs are identical!");
      return { success: true };
    } else {
      console.log(`\n❌ FAILED: ${comparison.reason}`);
      if (comparison.details) {
        console.log(`  Original tokens: ${comparison.details.tokens1}`);
        console.log(`  Regenerated tokens: ${comparison.details.tokens2}`);
      }
      return { success: false, reason: comparison.reason };
    }
    
  } catch (error) {
    console.log(`\n❌ ERROR: ${error.message}`);
    return { success: false, error: error.message };
  }
}

// Main
function main() {
  console.log("🚀 IMPROVED ROUND-TRIP TEST WITH SMART PRETTY PRINTER");
  console.log("Testing fixes for spacing and structure preservation");
  
  const results = [];
  
  for (const testCase of TEST_CASES) {
    const result = testRoundTrip(testCase);
    results.push({
      name: testCase.name,
      ...result
    });
  }
  
  // Summary
  const passed = results.filter(r => r.success).length;
  const failed = results.filter(r => !r.success).length;
  
  console.log("\n" + "#".repeat(60));
  console.log("SUMMARY");
  console.log("#".repeat(60));
  console.log(`Total: ${results.length}`);
  console.log(`✅ Passed: ${passed}`);
  console.log(`❌ Failed: ${failed}`);
  console.log(`Success Rate: ${(passed / results.length * 100).toFixed(1)}%`);
  
  console.log("\n📊 IMPROVEMENTS FROM NAIVE PRINTER:");
  console.log("• DATA(var) preserved (no extra spaces)");
  console.log("• Method calls formatted correctly");
  console.log("• Operators like -> stay together");
  console.log("• Comma handling improved");
  console.log("• No space before period");
  
  if (failed > 0) {
    console.log("\n⚠️  Remaining Issues:");
    results.filter(r => !r.success).forEach(r => {
      console.log(`• ${r.name}: ${r.reason || r.error}`);
    });
  }
}

main();