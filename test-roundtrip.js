#!/usr/bin/env node

/**
 * Round-trip test for ABAP ↔ AST transformation
 * Tests that ABAP → AST → ABAP' → AST' produces equivalent ASTs
 */

const abaplint = require("@abaplint/core");
const fs = require("fs");
const path = require("path");

// Simple ABAP test cases
const TEST_CASES = [
  {
    name: "Simple data declaration",
    abap: `DATA lv_test TYPE string.`
  },
  {
    name: "Assignment statement",
    abap: `DATA lv_test TYPE string.
lv_test = 'Hello World'.`
  },
  {
    name: "Write statement",
    abap: `DATA lv_test TYPE string.
lv_test = 'Hello'.
WRITE lv_test.`
  },
  {
    name: "IF statement",
    abap: `DATA lv_number TYPE i.
lv_number = 42.
IF lv_number > 0.
  WRITE 'Positive'.
ELSE.
  WRITE 'Non-positive'.
ENDIF.`
  },
  {
    name: "LOOP statement",
    abap: `DATA: lt_table TYPE TABLE OF string,
      lv_line TYPE string.
APPEND 'Line1' TO lt_table.
APPEND 'Line2' TO lt_table.
LOOP AT lt_table INTO lv_line.
  WRITE lv_line.
ENDLOOP.`
  }
];

// Parse ABAP to AST
function parseABAP(source, filename = "test.prog.abap") {
  const file = new abaplint.MemoryFile(filename, source);
  const reg = new abaplint.Registry();
  reg.addFile(file);
  reg.parse();
  
  const obj = reg.getFirstObject();
  if (!obj) {
    throw new Error("No object found");
  }
  
  const abapFile = obj.getABAPFiles()[0];
  if (!abapFile) {
    throw new Error("No ABAP file found");
  }
  
  return {
    file: abapFile,
    structure: abapFile.getStructure(),
    statements: abapFile.getStatements(),
    obj: obj
  };
}

// Pretty print AST back to ABAP
function astToABAP(parseResult) {
  const statements = parseResult.statements;
  const lines = [];
  let indentLevel = 0;
  const indent = "  ";
  
  for (const stmt of statements) {
    const stmtType = stmt.get().constructor.name;
    const tokens = stmt.getTokens().map(t => t.getStr());
    
    // Adjust indent for block structures
    if (stmtType === "EndIf" || stmtType === "EndLoop" || 
        stmtType === "EndDo" || stmtType === "EndForm" ||
        stmtType === "EndMethod" || stmtType === "EndClass") {
      indentLevel = Math.max(0, indentLevel - 1);
    }
    
    // Build the statement string
    let stmtStr = indent.repeat(indentLevel) + tokens.join(" ");
    
    // Only add period if not already present
    if (!stmtStr.endsWith(".")) {
      stmtStr += ".";
    }
    
    if (stmtType === "Else" || stmtType === "ElseIf") {
      // Else/ElseIf at same level as IF
      const tempIndent = Math.max(0, indentLevel - 1);
      stmtStr = indent.repeat(tempIndent) + tokens.join(" ");
      if (!stmtStr.endsWith(".")) {
        stmtStr += ".";
      }
    }
    
    lines.push(stmtStr);
    
    // Increase indent after block starters
    if (stmtType === "If" || stmtType === "Loop" || 
        stmtType === "Do" || stmtType === "Form" ||
        stmtType === "Method" || stmtType === "Class") {
      indentLevel++;
    }
    
    if (stmtType === "Else" || stmtType === "ElseIf") {
      indentLevel++;
    }
  }
  
  return lines.join("\n");
}

// Compare two ASTs for semantic equivalence
function compareASTs(ast1, ast2) {
  // Extract and normalize statement sequences
  const stmts1 = normalizeStatements(ast1.statements);
  const stmts2 = normalizeStatements(ast2.statements);
  
  if (stmts1.length !== stmts2.length) {
    console.log(`Statement count mismatch: ${stmts1.length} vs ${stmts2.length}`);
    return false;
  }
  
  for (let i = 0; i < stmts1.length; i++) {
    if (!compareStatements(stmts1[i], stmts2[i])) {
      console.log(`Statement ${i + 1} differs:`);
      console.log(`  AST1: ${stmts1[i].type} - ${stmts1[i].tokens.join(" ")}`);
      console.log(`  AST2: ${stmts2[i].type} - ${stmts2[i].tokens.join(" ")}`);
      return false;
    }
  }
  
  return true;
}

function normalizeStatements(statements) {
  return statements.map(stmt => ({
    type: stmt.get().constructor.name,
    tokens: stmt.getTokens().map(t => t.getStr().toUpperCase())
  }));
}

function compareStatements(stmt1, stmt2) {
  if (stmt1.type !== stmt2.type) {
    return false;
  }
  
  if (stmt1.tokens.length !== stmt2.tokens.length) {
    return false;
  }
  
  for (let i = 0; i < stmt1.tokens.length; i++) {
    // Case-insensitive token comparison for ABAP
    if (stmt1.tokens[i] !== stmt2.tokens[i]) {
      return false;
    }
  }
  
  return true;
}

// Run round-trip test
function runRoundTripTest(testCase) {
  console.log(`\n${"=".repeat(60)}`);
  console.log(`Test: ${testCase.name}`);
  console.log("=".repeat(60));
  
  try {
    // Step 1: Parse original ABAP
    console.log("\n1. Original ABAP:");
    console.log(testCase.abap);
    
    const ast1 = parseABAP(testCase.abap);
    console.log(`   ✓ Parsed to AST (${ast1.statements.length} statements)`);
    
    // Step 2: Convert AST back to ABAP
    const abapPrime = astToABAP(ast1);
    console.log("\n2. Regenerated ABAP:");
    console.log(abapPrime);
    
    // Step 3: Parse regenerated ABAP
    const ast2 = parseABAP(abapPrime);
    console.log(`   ✓ Re-parsed to AST (${ast2.statements.length} statements)`);
    
    // Step 4: Compare ASTs
    console.log("\n3. Comparing ASTs...");
    const equivalent = compareASTs(ast1, ast2);
    
    if (equivalent) {
      console.log("   ✓ ASTs are equivalent!");
      console.log("\nRESULT: ✅ PASSED");
      return true;
    } else {
      console.log("   ✗ ASTs differ!");
      console.log("\nRESULT: ❌ FAILED");
      return false;
    }
  } catch (error) {
    console.log(`\nERROR: ${error.message}`);
    console.log("RESULT: ❌ FAILED");
    return false;
  }
}

// Test with corpus files if available
function testCorpus(corpusPath) {
  console.log(`\n${"#".repeat(60)}`);
  console.log(`Testing corpus: ${corpusPath}`);
  console.log("#".repeat(60));
  
  if (!fs.existsSync(corpusPath)) {
    console.log("Corpus path not found, skipping corpus tests");
    return;
  }
  
  const files = fs.readdirSync(corpusPath)
    .filter(f => f.endsWith('.abap'))
    .slice(0, 10); // Test first 10 files
  
  let passed = 0;
  let failed = 0;
  
  for (const file of files) {
    const filePath = path.join(corpusPath, file);
    const content = fs.readFileSync(filePath, 'utf8');
    
    console.log(`\nTesting: ${file}`);
    
    try {
      const ast1 = parseABAP(content, file);
      const abapPrime = astToABAP(ast1);
      const ast2 = parseABAP(abapPrime, file);
      
      if (compareASTs(ast1, ast2)) {
        console.log("  ✅ PASSED");
        passed++;
      } else {
        console.log("  ❌ FAILED - AST mismatch");
        failed++;
      }
    } catch (error) {
      console.log(`  ❌ FAILED - ${error.message}`);
      failed++;
    }
  }
  
  console.log(`\nCorpus Results: ${passed} passed, ${failed} failed`);
}

// Main execution
function main() {
  console.log("ABAP Round-Trip Transformation Test");
  console.log("====================================");
  console.log("Testing: ABAP → AST → ABAP' → AST'");
  console.log("Goal: AST ≡ AST' (semantic equivalence)");
  
  let totalPassed = 0;
  let totalFailed = 0;
  
  // Run test cases
  for (const testCase of TEST_CASES) {
    if (runRoundTripTest(testCase)) {
      totalPassed++;
    } else {
      totalFailed++;
    }
  }
  
  // Test corpus if available
  const corpusPath = "/Users/alice/dev/abap-ts/tree-sitter-abap/_corpus";
  testCorpus(corpusPath);
  
  // Summary
  console.log(`\n${"=".repeat(60)}`);
  console.log("SUMMARY");
  console.log("=".repeat(60));
  console.log(`Test Cases: ${totalPassed} passed, ${totalFailed} failed`);
  console.log(`Success Rate: ${(totalPassed / (totalPassed + totalFailed) * 100).toFixed(1)}%`);
  
  if (totalFailed === 0) {
    console.log("\n🎉 All tests passed! Round-trip transformation works correctly.");
  } else {
    console.log("\n⚠️  Some tests failed. Review the transformation logic.");
  }
}

// Run the tests
main();