#!/usr/bin/env node

const abaplint = require("@abaplint/core");

console.log("🔍 ANALYZING ABAPLINT'S APPROACH");
console.log("=" .repeat(60));

const testCode = `DATA: lv_first TYPE string,
      lv_second TYPE i.`;

console.log("Original code:");
console.log(testCode);

// Step 1: Parse and see what happens to AST
const reg = new abaplint.Registry();
const file = new abaplint.MemoryFile("test.prog.abap", testCode);
reg.addFile(file);
reg.parse();

const obj = reg.getFirstObject();
const abapFile = obj?.getABAPFiles()[0];

if (abapFile) {
  console.log("\n📊 AST Analysis:");
  console.log("-".repeat(30));
  
  // Check statements
  const statements = abapFile.getStatements();
  console.log(`Number of statements: ${statements.length}`);
  
  statements.forEach((stmt, i) => {
    const type = stmt.get().constructor.name;
    const tokens = stmt.getTokens().map(t => t.getStr());
    console.log(`  Statement ${i+1}: ${type}`);
    console.log(`    Tokens: [${tokens.join(', ')}]`);
  });
  
  console.log("\n📝 Raw Content Operations:");
  console.log("-".repeat(30));
  
  // Check raw content
  console.log("Raw content from file:");
  console.log(`"${abapFile.getRaw()}"`);
  
  // Now use PrettyPrinter
  const config = new abaplint.Config();
  const pp = new abaplint.PrettyPrinter(abapFile, config);
  const formatted = pp.run();
  
  console.log("\nFormatted result:");
  console.log(`"${formatted}"`);
  
  console.log("\n🔍 Key Insight:");
  console.log("abaplint.PrettyPrinter works on:");
  console.log("1. Raw file content (preserves original structure)");
  console.log("2. AST nodes for position information");
  console.log("3. In-place string replacement operations");
  console.log("4. Does NOT reconstruct from tokens!");
  
  // Test our reconstruction vs abaplint approach
  console.log("\n🆚 COMPARISON:");
  console.log("-".repeat(30));
  
  // Our approach: reconstruct from AST
  let ourReconstruction = statements.map(stmt => {
    const tokens = stmt.getTokens().map(t => t.getStr());
    return tokens.join(" ") + ".";
  }).join("\n");
  
  console.log("Our AST→tokens approach:");
  console.log(`"${ourReconstruction}"`);
  
  console.log("\nabaplint's approach:");
  console.log(`"${formatted}"`);
  
  console.log("\n💡 THE KEY DIFFERENCE:");
  console.log("- Our approach: ABAP → AST → tokens → ABAP' (loses structure)");
  console.log("- abaplint: ABAP → (AST for analysis) → ABAP' (preserves structure)");
  console.log("- abaplint uses AST for analysis but preserves original text structure!");
}

// Test with more complex case
console.log("\n" + "=".repeat(60));
console.log("TESTING COMPLEX STRUCTURE PRESERVATION");
console.log("=".repeat(60));

const complexCode = `TYPES: BEGIN OF ty_struct,
         field1 TYPE string,
         field2 TYPE i,
       END OF ty_struct,
       ty_table TYPE TABLE OF ty_struct.`;

console.log("Complex test:");
console.log(complexCode);

const reg2 = new abaplint.Registry();
const file2 = new abaplint.MemoryFile("test2.prog.abap", complexCode);
reg2.addFile(file2);
reg2.parse();

const obj2 = reg2.getFirstObject();
const abapFile2 = obj2?.getABAPFiles()[0];

if (abapFile2) {
  // AST shows separate statements
  const stmts2 = abapFile2.getStatements();
  console.log(`\nAST shows ${stmts2.length} separate statements:`);
  stmts2.forEach((stmt, i) => {
    const type = stmt.get().constructor.name;
    console.log(`  ${i+1}. ${type}`);
  });
  
  // But pretty printer preserves structure
  const pp2 = new abaplint.PrettyPrinter(abapFile2, new abaplint.Config());
  const formatted2 = pp2.run();
  
  console.log("\nPretty printer result (preserves colon structure):");
  console.log(formatted2);
  
  console.log("\n✅ CONCLUSION:");
  console.log("abaplint is NOT doing ABAP→AST→ABAP transformation!");
  console.log("It's doing ABAP→(AST analysis)→enhanced ABAP");
  console.log("The colon structure is preserved in the original text!");
}
