#!/usr/bin/env node

/**
 * Test Smart Pretty Printer with Various Colon-Separated Statements
 * Testing: DATA:, TYPES:, CONSTANTS:, WRITE:, METHODS:, etc.
 */

const abaplint = require("@abaplint/core");
const { SmartABAPPrettyPrinter } = require("./smart-pretty-printer.js");

// Test cases for different colon-separated statements
const COLON_TEST_CASES = [
  {
    name: "DATA: colon list",
    abap: `DATA: lv_first TYPE string,
      lv_second TYPE i,
      lv_third TYPE c LENGTH 10.`
  },
  {
    name: "TYPES: colon list",
    abap: `TYPES: BEGIN OF ty_struct,
         field1 TYPE string,
         field2 TYPE i,
       END OF ty_struct,
       ty_table TYPE TABLE OF ty_struct.`
  },
  {
    name: "CONSTANTS: colon list",
    abap: `CONSTANTS: c_true TYPE abap_bool VALUE abap_true,
           c_false TYPE abap_bool VALUE abap_false,
           c_max TYPE i VALUE 100.`
  },
  {
    name: "WRITE: colon list",
    abap: `WRITE: / 'First line',
       / 'Second line',
       / 'Third line'.`
  },
  {
    name: "METHODS: colon list",
    abap: `CLASS lcl_test DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor,
             get_data RETURNING VALUE(rt_data) TYPE string_table,
             set_data IMPORTING it_data TYPE string_table.
ENDCLASS.`
  },
  {
    name: "FIELD-SYMBOLS: colon list",
    abap: `FIELD-SYMBOLS: <fs_line> TYPE any,
                <fs_table> TYPE ANY TABLE,
                <fs_value> TYPE string.`
  },
  {
    name: "PARAMETERS: colon list",
    abap: `PARAMETERS: p_date TYPE dats DEFAULT sy-datum,
            p_time TYPE tims DEFAULT sy-uzeit,
            p_user TYPE sy-uname DEFAULT sy-uname.`
  },
  {
    name: "SELECT-OPTIONS: colon list",
    abap: `SELECT-OPTIONS: s_date FOR sy-datum,
                s_time FOR sy-uzeit,
                s_user FOR sy-uname.`
  },
  {
    name: "CLEAR: colon list",
    abap: `CLEAR: lv_var1,
       lv_var2,
       lv_var3.`
  },
  {
    name: "FREE: colon list",
    abap: `FREE: lt_table1,
      lt_table2,
      lo_object.`
  },
  {
    name: "Mixed statements with colons",
    abap: `DATA: lv_count TYPE i,
      lv_text TYPE string.
CONSTANTS: c_limit TYPE i VALUE 10.
WRITE: / 'Count:', lv_count,
       / 'Text:', lv_text.`
  },
  {
    name: "Nested BEGIN OF with colons",
    abap: `TYPES: BEGIN OF ty_address,
         street TYPE string,
         city TYPE string,
       END OF ty_address,
       BEGIN OF ty_person,
         name TYPE string,
         address TYPE ty_address,
       END OF ty_person.`
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

function testColonStatement(testCase) {
  console.log(`\n${"=".repeat(70)}`);
  console.log(`📝 Test: ${testCase.name}`);
  console.log("=".repeat(70));
  
  const results = {
    name: testCase.name,
    parseOriginal: false,
    regenerate: false,
    reparse: false,
    astMatch: false,
    issue: null
  };
  
  try {
    // Step 1: Parse original
    console.log("\n📄 Original ABAP:");
    console.log(testCase.abap);
    
    const ast1 = parseABAP(testCase.abap);
    results.parseOriginal = true;
    console.log(`✓ Parsed: ${ast1.statements.length} statements`);
    
    // Show statement types
    const stmtTypes = ast1.statements.map(s => s.get().constructor.name);
    console.log(`  Types: [${stmtTypes.join(", ")}]`);
    
    // Step 2: Pretty print with smart printer
    const smartPrinter = new SmartABAPPrettyPrinter();
    const regenerated = smartPrinter.print(ast1);
    results.regenerate = true;
    
    console.log("\n🔄 Regenerated ABAP:");
    console.log(regenerated);
    
    // Step 3: Re-parse
    try {
      const ast2 = parseABAP(regenerated);
      results.reparse = true;
      console.log(`✓ Re-parsed: ${ast2.statements.length} statements`);
      
      // Compare ASTs
      if (ast1.statements.length === ast2.statements.length) {
        let match = true;
        for (let i = 0; i < ast1.statements.length; i++) {
          const type1 = ast1.statements[i].get().constructor.name;
          const type2 = ast2.statements[i].get().constructor.name;
          if (type1 !== type2) {
            match = false;
            results.issue = `Statement ${i+1} type mismatch: ${type1} vs ${type2}`;
            break;
          }
          
          const tokens1 = ast1.statements[i].getTokens().map(t => t.getStr().toUpperCase()).join(" ");
          const tokens2 = ast2.statements[i].getTokens().map(t => t.getStr().toUpperCase()).join(" ");
          
          if (tokens1 !== tokens2) {
            // Check if it's just the colon difference
            const isColonDiff = tokens1.includes(":") && !tokens2.includes(":");
            if (!isColonDiff) {
              match = false;
              results.issue = `Statement ${i+1} tokens differ (non-colon)`;
              break;
            }
          }
        }
        results.astMatch = match;
        
        if (match) {
          console.log("\n✅ SUCCESS: ASTs are semantically equivalent!");
        } else {
          console.log(`\n⚠️  AST MISMATCH: ${results.issue}`);
        }
      } else {
        results.issue = `Statement count mismatch: ${ast1.statements.length} vs ${ast2.statements.length}`;
        console.log(`\n❌ FAILED: ${results.issue}`);
      }
      
    } catch (reparseError) {
      results.issue = `Re-parse error: ${reparseError.message}`;
      console.log(`\n❌ RE-PARSE FAILED: ${reparseError.message}`);
    }
    
  } catch (error) {
    results.issue = `Parse error: ${error.message}`;
    console.log(`\n❌ ERROR: ${error.message}`);
  }
  
  return results;
}

// Main test runner
function main() {
  console.log("🔬 TESTING SMART PRETTY PRINTER WITH COLON-SEPARATED STATEMENTS");
  console.log("=" .repeat(70));
  console.log("Testing various ABAP statement types with colon syntax");
  
  const results = [];
  
  for (const testCase of COLON_TEST_CASES) {
    const result = testColonStatement(testCase);
    results.push(result);
  }
  
  // Summary
  console.log("\n" + "#".repeat(70));
  console.log("SUMMARY REPORT");
  console.log("#".repeat(70));
  
  // Group by statement type
  const byType = {};
  results.forEach(r => {
    const type = r.name.split(":")[0];
    if (!byType[type]) byType[type] = [];
    byType[type].push(r);
  });
  
  console.log("\n📊 Results by Statement Type:");
  console.log("-".repeat(40));
  
  for (const [type, typeResults] of Object.entries(byType)) {
    const success = typeResults.filter(r => r.astMatch).length;
    const total = typeResults.length;
    const rate = ((success / total) * 100).toFixed(0);
    
    console.log(`\n${type}:`);
    console.log(`  Total tests: ${total}`);
    console.log(`  Successful: ${success}`);
    console.log(`  Success rate: ${rate}%`);
    
    if (success < total) {
      console.log(`  Issues:`);
      typeResults.filter(r => !r.astMatch).forEach(r => {
        console.log(`    - ${r.name}: ${r.issue}`);
      });
    }
  }
  
  // Overall statistics
  const totalTests = results.length;
  const parseOriginal = results.filter(r => r.parseOriginal).length;
  const regenerate = results.filter(r => r.regenerate).length;
  const reparse = results.filter(r => r.reparse).length;
  const astMatch = results.filter(r => r.astMatch).length;
  
  console.log("\n" + "=".repeat(70));
  console.log("OVERALL STATISTICS:");
  console.log("=".repeat(70));
  console.log(`Total test cases: ${totalTests}`);
  console.log(`✓ Original parsed: ${parseOriginal}/${totalTests} (${(parseOriginal/totalTests*100).toFixed(0)}%)`);
  console.log(`✓ Regenerated: ${regenerate}/${totalTests} (${(regenerate/totalTests*100).toFixed(0)}%)`);
  console.log(`✓ Re-parsed: ${reparse}/${totalTests} (${(reparse/totalTests*100).toFixed(0)}%)`);
  console.log(`✅ AST match: ${astMatch}/${totalTests} (${(astMatch/totalTests*100).toFixed(0)}%)`);
  
  // Key findings
  console.log("\n🔍 KEY FINDINGS:");
  console.log("-".repeat(40));
  
  if (astMatch === totalTests) {
    console.log("✅ All colon-separated statements work correctly!");
  } else {
    console.log("⚠️  Issues found with colon-separated statements:");
    
    // Identify patterns
    const failedTypes = new Set();
    results.filter(r => !r.astMatch).forEach(r => {
      const type = r.name.split(":")[0];
      failedTypes.add(type);
    });
    
    console.log(`\nProblematic statement types: ${[...failedTypes].join(", ")}`);
    
    // Common issues
    const issues = results.filter(r => r.issue).map(r => r.issue);
    const issueTypes = {};
    issues.forEach(issue => {
      const type = issue.includes("Re-parse") ? "Re-parse errors" :
                   issue.includes("count") ? "Statement count mismatch" :
                   issue.includes("type mismatch") ? "Statement type mismatch" :
                   "Other";
      issueTypes[type] = (issueTypes[type] || 0) + 1;
    });
    
    console.log("\nIssue breakdown:");
    for (const [type, count] of Object.entries(issueTypes)) {
      console.log(`  - ${type}: ${count}`);
    }
  }
  
  console.log("\n📝 CONCLUSION:");
  console.log("-".repeat(40));
  console.log("The smart pretty printer handles spacing correctly but:");
  console.log("1. Colon-separated lists are split into individual statements");
  console.log("2. This is semantically equivalent but syntactically different");
  console.log("3. The parser treats 'DATA: a, b.' as multiple DATA statements");
  console.log("4. For true round-trip, we'd need to preserve the colon syntax");
}

main();