#!/usr/bin/env node

const abaplint = require("@abaplint/core");
const fs = require("fs");

// Test how abaplint actually handles formatting
const TEST_CASES = [
  {
    name: "Simple case",
    abap: `data lv_test type string.
LV_TEST = 'hello'.
write lv_test.`
  },
  {
    name: "DATA with inline declaration", 
    abap: `DATA(lv_result) = calculate( ).`
  },
  {
    name: "Colon list",
    abap: `DATA: a TYPE i,
      b TYPE string.`
  }
];

console.log("🔍 Testing abaplint formatting capabilities");
console.log("=" .repeat(70));

// Check what's available
console.log("\nAvailable exports from abaplint:");
console.log(Object.keys(abaplint).filter(k => k.includes('Pretty') || k.includes('Format') || k.includes('format')));

// Try different approaches
for (const testCase of TEST_CASES) {
  console.log(`\n📝 ${testCase.name}`);
  console.log("-".repeat(40));
  
  try {
    const reg = new abaplint.Registry();
    
    // Try with configuration
    const config = {
      "global": {
        "files": "/src/**/*.*",
        "skipGeneratedGatewayClasses": true,
        "skipGeneratedPersistentClasses": true,
        "skipGeneratedFunctionGroups": true
      },
      "syntax": {
        "version": "v702",
        "errorNamespace": "^(Z|Y|LCL_|TY_|LIF_)"
      },
      "rules": {
        "indentation": {
          "enabled": true,
          "indentationWidth": 2,
          "alignTryCatch": false,
          "globalClassSkipFirst": false,
          "ignoreExceptions": true,
          "severity": "Error",
          "exclude": [],
          "reason": ""
        },
        "keyword_case": {
          "enabled": true,
          "style": "upper",
          "ignoreExceptions": true,
          "ignoreLowerClassImplmentationStatement": true,
          "ignoreGlobalClassDefinition": false,
          "ignoreGlobalInterface": false,
          "ignoreFunctionModuleName": false,
          "ignoreGlobalClassBoundaries": false,
          "ignoreKeywords": [],
          "severity": "Error",
          "exclude": [],
          "reason": ""
        }
      }
    };
    
    const conf = new abaplint.Config(JSON.stringify(config));
    reg.setConfig(conf);
    
    const file = new abaplint.MemoryFile("test.prog.abap", testCase.abap);
    reg.addFile(file);
    reg.parse();
    
    const obj = reg.getFirstObject();
    const abapFile = obj?.getABAPFiles()[0];
    
    if (abapFile) {
      console.log("Original:");
      console.log(testCase.abap);
      
      // Check if PrettyPrinter exists and how to use it
      if (abaplint.PrettyPrinter) {
        const pp = new abaplint.PrettyPrinter(abapFile, conf);
        const result = pp.run();
        console.log("\nFormatted:");
        console.log(result);
      } else {
        console.log("PrettyPrinter not found in exports");
      }
      
      // Check for issues/fixes
      const issues = reg.findIssues();
      console.log(`\nIssues found: ${issues.length}`);
      
      // Check if any issues have fixes
      const fixableIssues = issues.filter(i => i.getFix && i.getFix());
      if (fixableIssues.length > 0) {
        console.log(`Fixable issues: ${fixableIssues.length}`);
        fixableIssues.forEach(issue => {
          const fix = issue.getFix();
          if (fix) {
            console.log(`  - ${issue.getMessage()}`);
          }
        });
      }
    }
  } catch (error) {
    console.log(`Error: ${error.message}`);
    console.log(error.stack);
  }
}

// Try to understand the internal structure
console.log("\n📚 Investigating abaplint internals:");
console.log("-".repeat(40));

try {
  const simpleCode = `DATA lv_test TYPE string.`;
  const reg = new abaplint.Registry();
  const file = new abaplint.MemoryFile("test.prog.abap", simpleCode);
  reg.addFile(file);
  reg.parse();
  
  const obj = reg.getFirstObject();
  const abapFile = obj?.getABAPFiles()[0];
  
  if (abapFile) {
    // Check what methods are available
    console.log("ABAPFile methods:", Object.getOwnPropertyNames(Object.getPrototypeOf(abapFile)).filter(m => !m.startsWith('_')).slice(0, 10));
    
    // Get raw content
    console.log("\nRaw content:", abapFile.getRaw());
    
    // Get statements
    const stmts = abapFile.getStatements();
    console.log(`Statements: ${stmts.length}`);
    
    // Check tokens
    if (stmts.length > 0) {
      const tokens = stmts[0].getTokens();
      console.log(`First statement tokens: ${tokens.map(t => t.getStr()).join(' ')}`);
    }
  }
} catch (error) {
  console.log(`Investigation error: ${error.message}`);
}
