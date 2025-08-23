#!/usr/bin/env node

const abaplint = require("@abaplint/core");

console.log("🔍 SIMPLE ABAPLINT ANALYSIS");
console.log("=" .repeat(50));

const testCode = `DATA: lv_first TYPE string,
      lv_second TYPE i.`;

console.log("Original:");
console.log(testCode);

// Parse 
const reg = new abaplint.Registry();
const file = new abaplint.MemoryFile("test.prog.abap", testCode);
reg.addFile(file);
reg.parse();

const obj = reg.getFirstObject();
const abapFile = obj?.getABAPFiles()[0];

if (abapFile) {
  console.log("\n📊 What the parser sees:");
  const statements = abapFile.getStatements();
  console.log(`Statements: ${statements.length}`);
  
  statements.forEach((stmt, i) => {
    console.log(`  ${i+1}. ${stmt.get().constructor.name}`);
    console.log(`     Tokens: ${stmt.getTokens().map(t => t.getStr()).join(' ')}`);
  });
  
  console.log("\n📝 Raw content:");
  console.log(`"${abapFile.getRaw()}"`);
  
  console.log("\n🔧 Without pretty printer (our approach would be):");
  const reconstructed = statements.map(stmt => {
    const tokens = stmt.getTokens().map(t => t.getStr());
    return tokens.join(" ") + (tokens[tokens.length-1] === '.' ? '' : '.');
  }).join("\n");
  console.log(reconstructed);
  
  console.log("\n✅ KEY INSIGHT:");
  console.log("The parser SPLITS the colon statement into 2 separate statements");
  console.log("But the raw content still has the colon structure");
  console.log("abaplint's PrettyPrinter must work with the raw text!");
}

// Test: what if we modify individual tokens?
console.log("\n" + "=".repeat(50));
console.log("TOKEN POSITION ANALYSIS");
console.log("=".repeat(50));

if (abapFile) {
  const statements = abapFile.getStatements();
  
  // Look at token positions
  statements.forEach((stmt, i) => {
    console.log(`\nStatement ${i+1}:`);
    const tokens = stmt.getTokens();
    tokens.forEach((token, j) => {
      const start = token.getStart();
      const str = token.getStr();
      console.log(`  Token ${j+1}: "${str}" at row ${start.getRow()}, col ${start.getCol()}`);
    });
  });
}

console.log("\n💡 CONCLUSION:");
console.log("abaplint uses token POSITIONS to modify the original text in-place");
console.log("It preserves structure because it never reconstructs from scratch!");
