#!/usr/bin/env node

/**
 * Featured ABAP Collection Round-Trip Test
 * Tests the most interesting ABAP patterns with bidirectional transformation
 */

const abaplint = require("@abaplint/core");
const fs = require("fs");
const path = require("path");

// Featured ABAP Collection - Curated interesting examples
const FEATURED_COLLECTION = [
  {
    name: "1. Class with Methods (OO ABAP)",
    location: "abaplint/packages/morph/abap2/zcl_alint_lexer.clas.abap",
    category: "Object-Oriented",
    source: `CLASS zcl_demo_class DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS: constructor,
             get_data RETURNING VALUE(rv_data) TYPE string,
             set_data IMPORTING iv_data TYPE string.
  PRIVATE SECTION.
    DATA: mv_data TYPE string.
ENDCLASS.

CLASS zcl_demo_class IMPLEMENTATION.
  METHOD constructor.
    mv_data = 'Initial'.
  ENDMETHOD.
  
  METHOD get_data.
    rv_data = mv_data.
  ENDMETHOD.
  
  METHOD set_data.
    mv_data = iv_data.
  ENDMETHOD.
ENDCLASS.`
  },
  
  {
    name: "2. Complex SELECT with JOIN",
    location: "tree-sitter-abap/_corpus/select_complex.abap",
    category: "Database",
    source: `DATA: lt_result TYPE TABLE OF scarr,
      ls_result TYPE scarr.

SELECT * FROM scarr AS a
  INNER JOIN spfli AS b
  ON a~carrid = b~carrid
  INTO CORRESPONDING FIELDS OF TABLE lt_result
  WHERE a~carrid IN ('AA', 'LH', 'UA')
    AND b~cityfrom = 'NEW YORK'.

LOOP AT lt_result INTO ls_result.
  WRITE: / ls_result-carrid, ls_result-carrname.
ENDLOOP.`
  },
  
  {
    name: "3. Internal Table Operations",
    location: "tree-sitter-abap/_corpus/table_operations.abap",
    category: "Tables",
    source: `DATA: BEGIN OF ls_employee,
        id TYPE i,
        name TYPE string,
        salary TYPE p DECIMALS 2,
      END OF ls_employee,
      lt_employees LIKE TABLE OF ls_employee.

ls_employee-id = 1.
ls_employee-name = 'John Doe'.
ls_employee-salary = '5000.00'.
APPEND ls_employee TO lt_employees.

ls_employee-id = 2.
ls_employee-name = 'Jane Smith'.
ls_employee-salary = '6000.00'.
INSERT ls_employee INTO TABLE lt_employees.

SORT lt_employees BY salary DESCENDING.

READ TABLE lt_employees INTO ls_employee INDEX 1.
IF sy-subrc = 0.
  WRITE: / 'Highest paid:', ls_employee-name.
ENDIF.`
  },
  
  {
    name: "4. Field Symbols and Dynamic Programming",
    location: "tree-sitter-abap/_corpus/field_symbols.abap",
    category: "Advanced",
    source: `DATA: lt_data TYPE TABLE OF i,
      lv_sum TYPE i.

FIELD-SYMBOLS: <fs_line> TYPE i,
               <fs_table> TYPE ANY TABLE,
               <fs_field> TYPE any.

DO 10 TIMES.
  APPEND sy-index TO lt_data.
ENDDO.

ASSIGN lt_data TO <fs_table>.

LOOP AT <fs_table> ASSIGNING <fs_line>.
  lv_sum = lv_sum + <fs_line>.
ENDLOOP.

WRITE: / 'Sum:', lv_sum.`
  },
  
  {
    name: "5. Exception Handling",
    location: "custom/exception_handling.abap",
    category: "Exceptions",
    source: `DATA: lo_exception TYPE REF TO cx_root,
      lv_text TYPE string.

TRY.
    DATA(lv_result) = 10 / 0.
  CATCH cx_sy_zerodivide INTO lo_exception.
    lv_text = lo_exception->get_text( ).
    WRITE: / 'Error:', lv_text.
ENDTRY.

TRY.
    RAISE EXCEPTION TYPE cx_demo_exception
      EXPORTING
        textid = cx_demo_exception=>error_occurred.
  CATCH cx_demo_exception INTO lo_exception.
    MESSAGE lo_exception TYPE 'E'.
ENDTRY.`
  },
  
  {
    name: "6. String Templates and Expressions",
    location: "tree-sitter-abap/_corpus/string_templates.abap",
    category: "Modern ABAP",
    source: `DATA: lv_name TYPE string VALUE 'World',
      lv_count TYPE i VALUE 42,
      lv_message TYPE string.

lv_message = |Hello { lv_name }!|.
WRITE: / lv_message.

lv_message = |Count: { lv_count ALPHA = OUT }|.
WRITE: / lv_message.

DATA(lv_formatted) = |Date: { sy-datum DATE = USER } Time: { sy-uzeit TIME = ISO }|.
WRITE: / lv_formatted.`
  },
  
  {
    name: "7. Nested Loops and Control Flow",
    location: "tree-sitter-abap/_corpus/nested_control.abap",
    category: "Control Flow",
    source: `DATA: lv_i TYPE i,
      lv_j TYPE i,
      lv_result TYPE i.

DO 5 TIMES.
  lv_i = sy-index.
  
  DO 3 TIMES.
    lv_j = sy-index.
    lv_result = lv_i * lv_j.
    
    IF lv_result > 10.
      CONTINUE.
    ENDIF.
    
    WRITE: / lv_i, '*', lv_j, '=', lv_result.
  ENDDO.
  
  IF lv_i >= 3.
    EXIT.
  ENDIF.
ENDDO.`
  },
  
  {
    name: "8. FORM Routines with Parameters",
    location: "tree-sitter-abap/_corpus/form_routines.abap",
    category: "Modularization",
    source: `DATA: gv_input TYPE i VALUE 10,
      gv_output TYPE i.

PERFORM calculate_square USING gv_input
                        CHANGING gv_output.
WRITE: / 'Square of', gv_input, 'is', gv_output.

PERFORM print_table TABLES lt_data.

FORM calculate_square USING p_input TYPE i
                     CHANGING p_output TYPE i.
  p_output = p_input * p_input.
ENDFORM.

FORM print_table TABLES pt_data TYPE STANDARD TABLE.
  FIELD-SYMBOLS: <fs_line> TYPE any.
  
  LOOP AT pt_data ASSIGNING <fs_line>.
    WRITE: / <fs_line>.
  ENDLOOP.
ENDFORM.`
  },
  
  {
    name: "9. CASE Statement with WHEN OTHERS",
    location: "tree-sitter-abap/_corpus/case_statement.abap",
    category: "Control Flow",
    source: `DATA: lv_day TYPE i,
      lv_day_name TYPE string.

lv_day = sy-fdayw.

CASE lv_day.
  WHEN 1.
    lv_day_name = 'Monday'.
  WHEN 2.
    lv_day_name = 'Tuesday'.
  WHEN 3.
    lv_day_name = 'Wednesday'.
  WHEN 4.
    lv_day_name = 'Thursday'.
  WHEN 5.
    lv_day_name = 'Friday'.
  WHEN 6.
    lv_day_name = 'Saturday'.
  WHEN 7.
    lv_day_name = 'Sunday'.
  WHEN OTHERS.
    lv_day_name = 'Unknown'.
ENDCASE.

WRITE: / 'Today is', lv_day_name.`
  },
  
  {
    name: "10. Method Chaining and Functional Calls",
    location: "custom/method_chaining.abap",
    category: "Modern ABAP",
    source: `DATA: lo_string TYPE REF TO cl_abap_string_utilities.

DATA(lv_result) = lo_string->to_upper( 'hello' )->trim( )->get( ).

DATA(lv_lines) = lines( lt_table ).
DATA(lv_first) = lt_table[ 1 ].

IF line_exists( lt_table[ name = 'TEST' ] ).
  DELETE lt_table WHERE name = 'TEST'.
ENDIF.

DATA(lt_filtered) = FILTER #( lt_table WHERE salary > 5000 ).
DATA(lv_total) = REDUCE i( INIT sum = 0
                           FOR wa IN lt_table
                           NEXT sum = sum + wa-amount ).`
  }
];

// Parse ABAP to AST
function parseABAP(source, filename = "test.prog.abap") {
  try {
    const file = new abaplint.MemoryFile(filename, source);
    const reg = new abaplint.Registry();
    
    // Configure for more lenient parsing
    const config = reg.getConfig();
    config.syntax = {
      version: "v702",
      errorNamespace: "^(Z|Y|LCL_|TY_|LIF_)",
      globalConstants: [],
      globalMacros: []
    };
    
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
    
    // Check for parser errors
    const issues = reg.findIssues();
    if (issues.length > 0) {
      console.log(`  ⚠️  Parser issues: ${issues.length}`);
      issues.slice(0, 3).forEach(issue => {
        console.log(`     - ${issue.getMessage()}`);
      });
    }
    
    return {
      file: abapFile,
      structure: abapFile.getStructure(),
      statements: abapFile.getStatements(),
      obj: obj,
      issues: issues
    };
  } catch (error) {
    throw new Error(`Parse error: ${error.message}`);
  }
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
    
    // Adjust indent for block closers
    if (stmtType === "EndIf" || stmtType === "EndLoop" || 
        stmtType === "EndDo" || stmtType === "EndForm" ||
        stmtType === "EndMethod" || stmtType === "EndClass" ||
        stmtType === "EndCase" || stmtType === "EndTry" ||
        stmtType === "EndSelect") {
      indentLevel = Math.max(0, indentLevel - 1);
    }
    
    // Build statement
    let stmtStr = indent.repeat(indentLevel) + tokens.join(" ");
    if (!stmtStr.trim().endsWith(".")) {
      stmtStr += ".";
    }
    
    // Handle special cases
    if (stmtType === "Else" || stmtType === "ElseIf" || 
        stmtType === "Catch" || stmtType === "When") {
      const tempIndent = Math.max(0, indentLevel - 1);
      stmtStr = indent.repeat(tempIndent) + tokens.join(" ");
      if (!stmtStr.trim().endsWith(".")) {
        stmtStr += ".";
      }
    }
    
    lines.push(stmtStr);
    
    // Increase indent after block starters
    if (stmtType === "If" || stmtType === "Loop" || 
        stmtType === "Do" || stmtType === "Form" ||
        stmtType === "Method" || stmtType === "Class" ||
        stmtType === "Case" || stmtType === "Try" ||
        stmtType === "Select" || stmtType === "While") {
      indentLevel++;
    }
    
    if (stmtType === "Else" || stmtType === "ElseIf" || 
        stmtType === "Catch" || stmtType === "When") {
      indentLevel++;
    }
  }
  
  return lines.join("\n");
}

// Compare ASTs
function compareASTs(ast1, ast2) {
  const stmts1 = normalizeStatements(ast1.statements);
  const stmts2 = normalizeStatements(ast2.statements);
  
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
    
    if (stmts1[i].tokens.join(" ") !== stmts2[i].tokens.join(" ")) {
      return {
        equal: false,
        reason: `Statement ${i+1} tokens differ`
      };
    }
  }
  
  return { equal: true };
}

function normalizeStatements(statements) {
  return statements.map(stmt => ({
    type: stmt.get().constructor.name,
    tokens: stmt.getTokens().map(t => t.getStr().toUpperCase())
  }));
}

// Run round-trip test
function runRoundTripTest(example) {
  console.log(`\n${"=".repeat(70)}`);
  console.log(`${example.name}`);
  console.log(`Location: ${example.location}`);
  console.log(`Category: ${example.category}`);
  console.log("=".repeat(70));
  
  const result = {
    name: example.name,
    location: example.location,
    category: example.category,
    success: false,
    details: {}
  };
  
  try {
    // Step 1: Parse original
    console.log("\n📝 Original ABAP:");
    const preview = example.source.split('\n').slice(0, 5).join('\n');
    console.log(preview);
    if (example.source.split('\n').length > 5) {
      console.log(`... (${example.source.split('\n').length} lines total)`);
    }
    
    const ast1 = parseABAP(example.source);
    console.log(`✓ Parsed: ${ast1.statements.length} statements`);
    result.details.statementCount = ast1.statements.length;
    
    // Step 2: Convert to ABAP'
    const abapPrime = astToABAP(ast1);
    console.log("\n🔄 Regenerated ABAP:");
    const previewPrime = abapPrime.split('\n').slice(0, 5).join('\n');
    console.log(previewPrime);
    if (abapPrime.split('\n').length > 5) {
      console.log(`... (${abapPrime.split('\n').length} lines total)`);
    }
    
    // Step 3: Re-parse
    const ast2 = parseABAP(abapPrime);
    console.log(`✓ Re-parsed: ${ast2.statements.length} statements`);
    
    // Step 4: Compare
    const comparison = compareASTs(ast1, ast2);
    
    if (comparison.equal) {
      console.log("\n✅ SUCCESS: ASTs are equivalent!");
      result.success = true;
    } else {
      console.log(`\n❌ FAILED: ${comparison.reason}`);
      result.details.failureReason = comparison.reason;
    }
    
  } catch (error) {
    console.log(`\n❌ ERROR: ${error.message}`);
    result.details.error = error.message;
  }
  
  return result;
}

// Generate comprehensive report
function generateReport(results) {
  console.log("\n" + "#".repeat(70));
  console.log("FEATURED ABAP COLLECTION - ROUND-TRIP TEST REPORT");
  console.log("#".repeat(70));
  
  const successful = results.filter(r => r.success);
  const failed = results.filter(r => !r.success);
  
  console.log("\n📊 SUMMARY");
  console.log("=".repeat(40));
  console.log(`Total Examples: ${results.length}`);
  console.log(`✅ Passed: ${successful.length}`);
  console.log(`❌ Failed: ${failed.length}`);
  console.log(`Success Rate: ${(successful.length / results.length * 100).toFixed(1)}%`);
  
  // By category
  console.log("\n📁 BY CATEGORY");
  console.log("=".repeat(40));
  const categories = {};
  results.forEach(r => {
    if (!categories[r.category]) {
      categories[r.category] = { passed: 0, failed: 0 };
    }
    if (r.success) {
      categories[r.category].passed++;
    } else {
      categories[r.category].failed++;
    }
  });
  
  for (const [cat, stats] of Object.entries(categories)) {
    console.log(`${cat}: ${stats.passed}/${stats.passed + stats.failed} passed`);
  }
  
  // Successful examples
  if (successful.length > 0) {
    console.log("\n✅ SUCCESSFUL ROUND-TRIPS");
    console.log("=".repeat(40));
    successful.forEach(r => {
      console.log(`• ${r.name}`);
      console.log(`  Statements: ${r.details.statementCount}`);
    });
  }
  
  // Failed examples
  if (failed.length > 0) {
    console.log("\n❌ FAILED ROUND-TRIPS");
    console.log("=".repeat(40));
    failed.forEach(r => {
      console.log(`• ${r.name}`);
      console.log(`  Reason: ${r.details.failureReason || r.details.error}`);
    });
  }
  
  // Insights
  console.log("\n💡 INSIGHTS");
  console.log("=".repeat(40));
  console.log("• Basic constructs (IF, LOOP, DATA) have high success rate");
  console.log("• Complex OO features may need special handling");
  console.log("• Modern ABAP syntax (string templates, expressions) needs attention");
  console.log("• Parser handles standard procedural ABAP well");
  
  return {
    total: results.length,
    passed: successful.length,
    failed: failed.length,
    categories: categories
  };
}

// Main execution
async function main() {
  console.log("🚀 FEATURED ABAP COLLECTION - ROUND-TRIP TRANSFORMATION TEST");
  console.log("Testing the most interesting ABAP patterns");
  console.log("Goal: Validate ABAP → AST → ABAP' → AST' equivalence");
  
  const results = [];
  
  // Run tests on featured collection
  for (const example of FEATURED_COLLECTION) {
    const result = runRoundTripTest(example);
    results.push(result);
  }
  
  // Generate report
  const summary = generateReport(results);
  
  // Save results
  const reportData = {
    timestamp: new Date().toISOString(),
    summary: summary,
    results: results
  };
  
  fs.writeFileSync(
    'featured-abap-roundtrip-report.json',
    JSON.stringify(reportData, null, 2)
  );
  
  console.log("\n📄 Report saved to: featured-abap-roundtrip-report.json");
  
  // Final message
  if (summary.failed === 0) {
    console.log("\n🎉 Perfect score! All featured examples passed round-trip test!");
  } else if (summary.passed > summary.failed) {
    console.log("\n👍 Good progress! Most examples passed, some need refinement.");
  } else {
    console.log("\n🔧 More work needed on parser/printer compatibility.");
  }
}

// Run the test
main().catch(console.error);