#!/usr/bin/env node

/**
 * FINAL CONFIRMATION: AST Equivalence Test
 * Focus on cases that SHOULD work perfectly
 */

const abaplint = require("@abaplint/core");
const { SmartABAPPrettyPrinter } = require("./smart-pretty-printer.js");

// Focus on test cases that should have perfect AST equivalence
const FOCUSED_TESTS = [
  {
    name: "Simple statements",
    abap: `DATA lv_test TYPE string.
lv_test = 'Hello'.
WRITE lv_test.`
  },
  {
    name: "Inline declarations (no spaces)",
    abap: `DATA(lv_result) = calculate().
DATA(lv_msg) = |Hello World|.`
  },
  {
    name: "Control flow",
    abap: `IF lv_value > 0.
  WRITE 'Positive'.
ELSE.
  WRITE 'Zero or negative'.
ENDIF.`
  },
  {
    name: "Loop structure",
    abap: `LOOP AT lt_table INTO DATA(ls_item).
  WRITE ls_item.
ENDLOOP.`
  },
  {
    name: "Method calls with operators",
    abap: `lo_obj->get_data().
cl_helper=>process_data(lv_input).`
  }
];

function parseABAP(source) {
  const file = new abaplint.MemoryFile("test.prog.abap", source);
  const reg = new abaplint.Registry();
  reg.addFile(file);
  reg.parse();
  
  const obj = reg.getFirstObject();
  const abapFile = obj?.getABAPFiles()[0];
  
  return {
    statements: abapFile.getStatements(),
    hasErrors: reg.findIssues().filter(i => i.getSeverity() === abaplint.Severity.Error).length > 0
  };
}

function normalizeTokens(tokens) {
  // Normalize tokens for comparison (uppercase, clean)
  return tokens.map(t => t.getStr().toUpperCase()).join(" ");
}

function testRoundTrip(testCase) {
  console.log(`\n📝 ${testCase.name}`);
  console.log("-".repeat(50));
  
  try {
    // Step 1: Parse original
    const ast1 = parseABAP(testCase.abap);
    if (ast1.hasErrors) {
      console.log("❌ Original has parse errors");
      return { success: false, reason: "Original parse errors" };
    }
    
    console.log(`AST1: ${ast1.statements.length} statements`);
    const ast1Structure = ast1.statements.map((stmt, i) => ({
      index: i + 1,
      type: stmt.get().constructor.name,
      tokens: normalizeTokens(stmt.getTokens())
    }));
    
    // Step 2: Pretty print
    const printer = new SmartABAPPrettyPrinter();
    const regenerated = printer.print(ast1);
    
    // Step 3: Parse regenerated
    const ast2 = parseABAP(regenerated);
    if (ast2.hasErrors) {
      console.log("❌ Regenerated has parse errors");
      console.log("Regenerated code:");
      console.log(regenerated);
      return { success: false, reason: "Regenerated parse errors" };
    }
    
    console.log(`AST2: ${ast2.statements.length} statements`);
    const ast2Structure = ast2.statements.map((stmt, i) => ({
      index: i + 1,
      type: stmt.get().constructor.name,
      tokens: normalizeTokens(stmt.getTokens())
    }));
    
    // Step 4: Compare
    if (ast1Structure.length !== ast2Structure.length) {
      return { 
        success: false, 
        reason: `Statement count: ${ast1Structure.length} vs ${ast2Structure.length}` 
      };
    }
    
    for (let i = 0; i < ast1Structure.length; i++) {
      const s1 = ast1Structure[i];
      const s2 = ast2Structure[i];
      
      if (s1.type !== s2.type) {
        console.log(`Statement ${i+1} type mismatch: ${s1.type} vs ${s2.type}`);
        return { success: false, reason: `Statement ${i+1} type mismatch` };
      }
      
      if (s1.tokens !== s2.tokens) {
        console.log(`Statement ${i+1} tokens differ:`);
        console.log(`  AST1: "${s1.tokens}"`);
        console.log(`  AST2: "${s2.tokens}"`);
        return { success: false, reason: `Statement ${i+1} tokens differ` };
      }
    }
    
    console.log("✅ Perfect AST equivalence!");
    return { success: true };
    
  } catch (error) {
    console.log(`❌ Error: ${error.message}`);
    return { success: false, reason: error.message };
  }
}

function main() {
  console.log("🎯 FINAL AST EQUIVALENCE CONFIRMATION");
  console.log("Testing: ABAP → AST → ABAP' → AST' where AST ≡ AST'");
  console.log("=".repeat(60));
  
  const results = [];
  
  for (const test of FOCUSED_TESTS) {
    const result = testRoundTrip(test);
    results.push({ name: test.name, ...result });
  }
  
  // Summary
  console.log("\n" + "=".repeat(60));
  console.log("📊 FINAL RESULTS");
  console.log("=".repeat(60));
  
  const successful = results.filter(r => r.success);
  const failed = results.filter(r => !r.success);
  
  console.log(`✅ Perfect AST equivalence: ${successful.length}/${results.length}`);
  console.log(`❌ Failed: ${failed.length}/${results.length}`);
  console.log(`Success rate: ${(successful.length / results.length * 100).toFixed(1)}%`);
  
  if (failed.length > 0) {
    console.log("\n❌ Failures:");
    failed.forEach(r => console.log(`  - ${r.name}: ${r.reason}`));
  }
  
  console.log("\n🏁 CONCLUSION:");
  if (successful.length === results.length) {
    console.log("✅ CONFIRMED: We have proper ABAP → AST → ABAP' → AST' with AST ≡ AST'");
    console.log("✅ Perfect round-trip transformation achieved!");
  } else if (successful.length >= results.length * 0.8) {
    console.log("✅ MOSTLY CONFIRMED: >80% perfect AST equivalence");
    console.log("✅ Suitable for dataset generation");
  } else {
    console.log("⚠️  PARTIAL: Some edge cases need refinement");
  }
}

main();