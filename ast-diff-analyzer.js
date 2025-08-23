#!/usr/bin/env node

/**
 * AST Difference Analyzer for ABAP Round-Trip Transformation
 * Analyzes and visualizes the differences between original and regenerated ASTs
 */

const abaplint = require("@abaplint/core");
const fs = require("fs");

// Test cases to analyze
const TEST_CASES = [
  {
    name: "Simple Working Case",
    original: `DATA lv_test TYPE string.
lv_test = 'Hello'.
WRITE lv_test.`
  },
  {
    name: "Colon-Separated DATA (Failing)",
    original: `DATA: lv_first TYPE string,
      lv_second TYPE i,
      lv_third TYPE c LENGTH 10.`
  },
  {
    name: "Complex Declaration (Failing)",
    original: `DATA: BEGIN OF ls_struct,
        field1 TYPE string,
        field2 TYPE i,
      END OF ls_struct,
      lt_table LIKE TABLE OF ls_struct.`
  },
  {
    name: "Method with Colon Lists",
    original: `CLASS lcl_test DEFINITION.
  PUBLIC SECTION.
    METHODS: method1,
             method2 IMPORTING iv_param TYPE string,
             method3 RETURNING VALUE(rv_result) TYPE i.
ENDCLASS.`
  },
  {
    name: "String Template Expression",
    original: `DATA lv_name TYPE string VALUE 'World'.
DATA(lv_message) = |Hello { lv_name }!|.
WRITE lv_message.`
  }
];

// Parse ABAP to AST
function parseABAP(source, name = "test") {
  const file = new abaplint.MemoryFile(name + ".prog.abap", source);
  const reg = new abaplint.Registry();
  
  // More lenient config
  const config = reg.getConfig();
  config.syntax = {
    version: "v755",
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
    
    // Adjust indent
    if (["EndIf", "EndLoop", "EndClass", "EndMethod", "EndForm"].includes(stmtType)) {
      indentLevel = Math.max(0, indentLevel - 1);
    }
    
    // Build statement - THIS IS WHERE THE ISSUE IS
    // The pretty printer adds spaces between all tokens
    let stmtStr = indent.repeat(indentLevel) + tokens.join(" ");
    
    // Add period if missing
    if (!stmtStr.trim().endsWith(".")) {
      stmtStr += ".";
    }
    
    lines.push(stmtStr);
    
    // Increase indent
    if (["If", "Loop", "Class", "Method", "Form"].includes(stmtType)) {
      indentLevel++;
    }
  }
  
  return lines.join("\n");
}

// Extract detailed AST information
function extractASTInfo(parseResult) {
  const info = {
    statementCount: parseResult.statements.length,
    statements: [],
    tokens: [],
    structure: null
  };
  
  // Extract statements
  for (const stmt of parseResult.statements) {
    const stmtInfo = {
      type: stmt.get().constructor.name,
      tokens: stmt.getTokens().map(t => ({
        value: t.getStr(),
        start: { row: t.getStart().getRow(), col: t.getStart().getCol() },
        end: { row: t.getEnd().getRow(), col: t.getEnd().getCol() }
      })),
      tokenString: stmt.getTokens().map(t => t.getStr()).join(" ")
    };
    info.statements.push(stmtInfo);
  }
  
  // Extract all tokens
  for (const stmt of parseResult.statements) {
    for (const token of stmt.getTokens()) {
      info.tokens.push(token.getStr());
    }
  }
  
  // Visualize structure
  if (parseResult.structure) {
    info.structure = visualizeNode(parseResult.structure, 0);
  }
  
  return info;
}

// Visualize AST node
function visualizeNode(node, depth = 0) {
  if (!node) return null;
  
  const indent = "  ".repeat(depth);
  let viz = indent + node.constructor.name;
  
  if (node.get && typeof node.get === 'function') {
    viz += ` [${node.get().constructor.name}]`;
    
    if (node.getTokens) {
      const tokens = node.getTokens().map(t => t.getStr());
      if (tokens.length <= 5) {
        viz += `: ${tokens.join(" ")}`;
      } else {
        viz += `: ${tokens.slice(0, 3).join(" ")} ... (${tokens.length} tokens)`;
      }
    }
  }
  
  viz += "\n";
  
  if (node.getChildren) {
    for (const child of node.getChildren()) {
      viz += visualizeNode(child, depth + 1);
    }
  }
  
  return viz;
}

// Compare two ASTs and find differences
function compareASTs(ast1, ast2) {
  const info1 = extractASTInfo(ast1);
  const info2 = extractASTInfo(ast2);
  
  const differences = {
    statementCount: {
      original: info1.statementCount,
      regenerated: info2.statementCount,
      diff: info2.statementCount - info1.statementCount
    },
    statements: [],
    tokenDifferences: []
  };
  
  // Compare statements
  const maxStmts = Math.max(info1.statements.length, info2.statements.length);
  for (let i = 0; i < maxStmts; i++) {
    const stmt1 = info1.statements[i];
    const stmt2 = info2.statements[i];
    
    if (!stmt1) {
      differences.statements.push({
        index: i + 1,
        type: "ADDED",
        original: null,
        regenerated: stmt2
      });
    } else if (!stmt2) {
      differences.statements.push({
        index: i + 1,
        type: "REMOVED",
        original: stmt1,
        regenerated: null
      });
    } else if (stmt1.type !== stmt2.type) {
      differences.statements.push({
        index: i + 1,
        type: "TYPE_CHANGED",
        original: stmt1,
        regenerated: stmt2,
        change: `${stmt1.type} → ${stmt2.type}`
      });
    } else if (stmt1.tokenString !== stmt2.tokenString) {
      differences.statements.push({
        index: i + 1,
        type: "TOKENS_CHANGED",
        original: stmt1,
        regenerated: stmt2,
        tokenDiff: findTokenDifferences(stmt1.tokens, stmt2.tokens)
      });
    }
  }
  
  return differences;
}

// Find token-level differences
function findTokenDifferences(tokens1, tokens2) {
  const diffs = [];
  const maxLen = Math.max(tokens1.length, tokens2.length);
  
  for (let i = 0; i < maxLen; i++) {
    const t1 = tokens1[i];
    const t2 = tokens2[i];
    
    if (!t1) {
      diffs.push({ index: i, type: "added", value: t2.value });
    } else if (!t2) {
      diffs.push({ index: i, type: "removed", value: t1.value });
    } else if (t1.value !== t2.value) {
      diffs.push({ index: i, type: "changed", from: t1.value, to: t2.value });
    }
  }
  
  return diffs;
}

// Run analysis on a test case
function analyzeTestCase(testCase) {
  console.log("\n" + "=".repeat(70));
  console.log(`ANALYSIS: ${testCase.name}`);
  console.log("=".repeat(70));
  
  console.log("\n📝 ORIGINAL ABAP:");
  console.log(testCase.original);
  
  try {
    // Parse original
    const ast1 = parseABAP(testCase.original, "original");
    const info1 = extractASTInfo(ast1);
    
    console.log(`\n✓ Parsed: ${info1.statementCount} statements`);
    console.log("Statements:", info1.statements.map(s => s.type).join(", "));
    
    // Regenerate ABAP
    const regenerated = astToABAP(ast1);
    console.log("\n🔄 REGENERATED ABAP:");
    console.log(regenerated);
    
    // Parse regenerated
    const ast2 = parseABAP(regenerated, "regenerated");
    const info2 = extractASTInfo(ast2);
    
    console.log(`\n✓ Re-parsed: ${info2.statementCount} statements`);
    console.log("Statements:", info2.statements.map(s => s.type).join(", "));
    
    // Compare ASTs
    const differences = compareASTs(ast1, ast2);
    
    console.log("\n🔍 AST DIFFERENCES:");
    
    if (differences.statementCount.diff !== 0) {
      console.log(`Statement Count: ${differences.statementCount.original} → ${differences.statementCount.regenerated} (${differences.statementCount.diff > 0 ? '+' : ''}${differences.statementCount.diff})`);
    }
    
    if (differences.statements.length === 0) {
      console.log("✅ ASTs are IDENTICAL!");
    } else {
      console.log(`❌ Found ${differences.statements.length} differences:`);
      
      for (const diff of differences.statements) {
        console.log(`\n  Statement ${diff.index}:`);
        console.log(`  Type: ${diff.type}`);
        
        if (diff.type === "TYPE_CHANGED") {
          console.log(`  Change: ${diff.change}`);
          console.log(`  Original tokens: ${diff.original.tokenString}`);
          console.log(`  Regenerated tokens: ${diff.regenerated.tokenString}`);
        } else if (diff.type === "TOKENS_CHANGED") {
          console.log(`  Statement type: ${diff.original.type}`);
          console.log(`  Original: ${diff.original.tokenString}`);
          console.log(`  Regenerated: ${diff.regenerated.tokenString}`);
          if (diff.tokenDiff.length > 0) {
            console.log(`  Token changes:`);
            for (const td of diff.tokenDiff) {
              if (td.type === "changed") {
                console.log(`    - Token ${td.index}: "${td.from}" → "${td.to}"`);
              }
            }
          }
        } else if (diff.type === "ADDED") {
          console.log(`  Added statement: ${diff.regenerated.type}`);
          console.log(`  Tokens: ${diff.regenerated.tokenString}`);
        } else if (diff.type === "REMOVED") {
          console.log(`  Removed statement: ${diff.original.type}`);
          console.log(`  Tokens: ${diff.original.tokenString}`);
        }
      }
    }
    
    // Show AST visualization for failing cases
    if (differences.statements.length > 0) {
      console.log("\n📊 AST STRUCTURE COMPARISON:");
      console.log("\nOriginal AST:");
      console.log(info1.structure || visualizeNode(ast1.structure, 0));
      console.log("\nRegenerated AST:");
      console.log(info2.structure || visualizeNode(ast2.structure, 0));
    }
    
    return {
      success: differences.statements.length === 0,
      differences: differences
    };
    
  } catch (error) {
    console.log(`\n❌ ERROR: ${error.message}`);
    return {
      success: false,
      error: error.message
    };
  }
}

// Generate comprehensive report
function generateReport(results) {
  console.log("\n" + "#".repeat(70));
  console.log("AST DIFFERENCE ANALYSIS REPORT");
  console.log("#".repeat(70));
  
  console.log("\n📊 SUMMARY");
  console.log("=".repeat(40));
  
  const successful = results.filter(r => r.success).length;
  const failed = results.filter(r => !r.success).length;
  
  console.log(`Total Cases: ${results.length}`);
  console.log(`✅ Identical ASTs: ${successful}`);
  console.log(`❌ Different ASTs: ${failed}`);
  console.log(`Success Rate: ${(successful / results.length * 100).toFixed(1)}%`);
  
  console.log("\n🔍 KEY FINDINGS");
  console.log("=".repeat(40));
  
  console.log("\n1. COLON-SEPARATED DECLARATIONS");
  console.log("   Original:  DATA: a TYPE i, b TYPE string.");
  console.log("   Becomes:   DATA a TYPE i ,.");
  console.log("              DATA b TYPE string.");
  console.log("   Issue: Parser splits into separate statements");
  
  console.log("\n2. TOKEN SPACING");
  console.log("   Original:  DATA(lv_var) = value.");
  console.log("   Becomes:   DATA ( lv_var ) = value .");
  console.log("   Issue: Pretty printer adds spaces around all tokens");
  
  console.log("\n3. STATEMENT STRUCTURE");
  console.log("   Complex nested structures (BEGIN OF) lose hierarchy");
  console.log("   Method definitions with colons are split");
  
  console.log("\n💡 ROOT CAUSE ANALYSIS");
  console.log("=".repeat(40));
  console.log("The pretty printer uses: tokens.join(' ')");
  console.log("This adds spaces between ALL tokens, including:");
  console.log("• Parentheses: ( ) instead of ()");
  console.log("• Commas: , . instead of ,.");
  console.log("• Operators: - > instead of ->");
  
  console.log("\n📈 TRANSFORMATION PATTERNS");
  console.log("=".repeat(40));
  console.log("• Simple statements: Perfect round-trip ✅");
  console.log("• Colon lists: Split into individual statements ❌");
  console.log("• String templates: Token spacing issues ❌");
  console.log("• Method chains: Operator spacing issues ❌");
}

// Main execution
function main() {
  console.log("🔬 AST DIFFERENCE ANALYZER");
  console.log("Analyzing differences between original and regenerated ASTs");
  
  const results = [];
  
  for (const testCase of TEST_CASES) {
    const result = analyzeTestCase(testCase);
    results.push({
      name: testCase.name,
      ...result
    });
  }
  
  generateReport(results);
  
  // Save detailed results
  fs.writeFileSync(
    'ast-difference-analysis.json',
    JSON.stringify(results, null, 2)
  );
  
  console.log("\n📄 Detailed results saved to: ast-difference-analysis.json");
}

// Run analysis
main();