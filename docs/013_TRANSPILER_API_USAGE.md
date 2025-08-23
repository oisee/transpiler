# Transpiler API Usage & Dataset Generation Guide

## Overview

This document explains how to use the ABAP transpiler programmatically from various environments and how to leverage it for dataset generation for language model fine-tuning.

## 1. JavaScript/TypeScript Integration

### Basic Usage

```javascript
import { Transpiler } from "@abaplint/transpiler";
import * as abaplint from "@abaplint/core";

async function transpileABAP(abapCode) {
  // Method 1: Simple API
  const transpiler = new Transpiler();
  const result = await transpiler.runRaw([{
    filename: "test.prog.abap",
    contents: abapCode
  }]);
  
  return result.objects[0].chunk.getCode();
}

// Method 2: Using Registry (more control)
async function transpileWithRegistry(abapCode) {
  const reg = new abaplint.Registry();
  reg.addFile(new abaplint.MemoryFile("test.prog.abap", abapCode));
  reg.parse();
  
  const transpiler = new Transpiler();
  const result = await transpiler.run(reg);
  
  return result;
}
```

### AST Manipulation

```javascript
import * as abaplint from "@abaplint/core";

function analyzeAST(abapCode) {
  const file = new abaplint.MemoryFile("test.prog.abap", abapCode);
  const reg = new abaplint.Registry();
  reg.addFile(file);
  reg.parse();
  
  const obj = reg.getFirstObject();
  const abapFile = obj.getABAPFiles()[0];
  
  // Traverse AST
  for (const statement of abapFile.getStatements()) {
    console.log("Statement:", statement.get().constructor.name);
    console.log("Tokens:", statement.concatTokens());
    
    // Access expressions
    for (const expr of statement.findAllExpressions(abaplint.Expressions.Source)) {
      console.log("Source expression:", expr.concatTokens());
    }
  }
  
  return abapFile.getStructure(); // Root AST node
}
```

## 2. REST API Server

Create a simple REST API wrapper:

```javascript
// server.js
import express from 'express';
import { Transpiler } from "@abaplint/transpiler";

const app = express();
app.use(express.json());

app.post('/transpile', async (req, res) => {
  try {
    const { code, filename = "input.prog.abap" } = req.body;
    
    const transpiler = new Transpiler();
    const result = await transpiler.runRaw([{
      filename,
      contents: code
    }]);
    
    res.json({
      success: true,
      javascript: result.objects[0]?.chunk.getCode(),
      sourceMap: result.objects[0]?.chunk.getMap(filename)
    });
  } catch (error) {
    res.status(400).json({
      success: false,
      error: error.message
    });
  }
});

app.listen(3000, () => {
  console.log('Transpiler API running on port 3000');
});
```

## 3. Go Integration

### Using Node.js Child Process

```go
package main

import (
    "encoding/json"
    "fmt"
    "os/exec"
)

type TranspileRequest struct {
    Code     string `json:"code"`
    Filename string `json:"filename"`
}

type TranspileResponse struct {
    Success    bool   `json:"success"`
    Javascript string `json:"javascript"`
    Error      string `json:"error,omitempty"`
}

func transpileABAP(code string) (*TranspileResponse, error) {
    req := TranspileRequest{
        Code:     code,
        Filename: "input.prog.abap",
    }
    
    reqJSON, _ := json.Marshal(req)
    
    cmd := exec.Command("node", "transpile-worker.js")
    cmd.Stdin = strings.NewReader(string(reqJSON))
    
    output, err := cmd.Output()
    if err != nil {
        return nil, err
    }
    
    var resp TranspileResponse
    json.Unmarshal(output, &resp)
    
    return &resp, nil
}
```

### Worker Script (transpile-worker.js)

```javascript
// transpile-worker.js
import { Transpiler } from "@abaplint/transpiler";

async function main() {
  const input = await readStdin();
  const request = JSON.parse(input);
  
  try {
    const transpiler = new Transpiler();
    const result = await transpiler.runRaw([{
      filename: request.filename,
      contents: request.code
    }]);
    
    console.log(JSON.stringify({
      success: true,
      javascript: result.objects[0]?.chunk.getCode()
    }));
  } catch (error) {
    console.log(JSON.stringify({
      success: false,
      error: error.message
    }));
  }
}

function readStdin() {
  return new Promise((resolve) => {
    let data = '';
    process.stdin.on('data', chunk => data += chunk);
    process.stdin.on('end', () => resolve(data));
  });
}

main();
```

## 4. ABAP Integration via REST

```abap
CLASS zcl_transpiler_client DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS: transpile_code
      IMPORTING
        iv_abap_code TYPE string
      RETURNING
        VALUE(rv_js_code) TYPE string.
ENDCLASS.

CLASS zcl_transpiler_client IMPLEMENTATION.
  METHOD transpile_code.
    DATA: lo_client TYPE REF TO if_http_client,
          lv_json   TYPE string.
    
    " Create HTTP client
    cl_http_client=>create_by_url(
      EXPORTING
        url = 'http://transpiler-api:3000/transpile'
      IMPORTING
        client = lo_client ).
    
    " Set request method and content type
    lo_client->request->set_method( 'POST' ).
    lo_client->request->set_content_type( 'application/json' ).
    
    " Build JSON request
    lv_json = |\{ "code": "{ cl_abap_conv_out_ce=>create( )->convert( iv_abap_code ) }" \}|.
    lo_client->request->set_cdata( lv_json ).
    
    " Send request
    lo_client->send( ).
    lo_client->receive( ).
    
    " Parse response
    DATA(lv_response) = lo_client->response->get_cdata( ).
    " Parse JSON response to get rv_js_code
    
    lo_client->close( ).
  ENDMETHOD.
ENDCLASS.
```

## 5. Dataset Generation for Fine-tuning

### JS → ABAP Dataset Generator

```javascript
// dataset-generator.js
import { Transpiler } from "@abaplint/transpiler";
import * as abaplint from "@abaplint/core";
import fs from 'fs';
import path from 'path';

class DatasetGenerator {
  constructor() {
    this.transpiler = new Transpiler();
    this.dataset = [];
  }
  
  // Extract ABAP patterns and their JS equivalents
  async generatePairs(abapFilePath) {
    const abapCode = fs.readFileSync(abapFilePath, 'utf-8');
    const filename = path.basename(abapFilePath);
    
    // Parse ABAP
    const reg = new abaplint.Registry();
    reg.addFile(new abaplint.MemoryFile(filename, abapCode));
    reg.parse();
    
    // Transpile to JS
    const result = await this.transpiler.run(reg);
    const jsCode = result.objects[0]?.chunk.getCode();
    
    // Extract statement-level pairs
    const abapFile = reg.getFirstObject().getABAPFiles()[0];
    const pairs = this.extractStatementPairs(abapFile, jsCode);
    
    return pairs;
  }
  
  extractStatementPairs(abapFile, jsCode) {
    const pairs = [];
    
    for (const statement of abapFile.getStatements()) {
      const abapSnippet = statement.concatTokens();
      const statementType = statement.get().constructor.name;
      
      // Map common patterns
      const jsEquivalent = this.mapPattern(statementType, statement);
      
      if (jsEquivalent) {
        pairs.push({
          abap: abapSnippet,
          javascript: jsEquivalent,
          type: statementType,
          complexity: this.calculateComplexity(statement)
        });
      }
    }
    
    return pairs;
  }
  
  mapPattern(statementType, statement) {
    // Pattern mapping for common ABAP constructs
    const patterns = {
      'Move': (stmt) => {
        const target = stmt.findFirstExpression(abaplint.Expressions.Target);
        const source = stmt.findFirstExpression(abaplint.Expressions.Source);
        if (target && source) {
          return `${this.toJsVariable(target)} = ${this.toJsExpression(source)};`;
        }
      },
      'If': (stmt) => {
        const cond = stmt.findFirstExpression(abaplint.Expressions.Cond);
        return `if (${this.toJsCondition(cond)}) {`;
      },
      'Loop': (stmt) => {
        const target = stmt.findFirstExpression(abaplint.Expressions.Target);
        const source = stmt.findFirstExpression(abaplint.Expressions.Source);
        return `for await (const ${this.toJsVariable(target)} of ${this.toJsExpression(source)}) {`;
      },
      'Write': (stmt) => {
        const source = stmt.findFirstExpression(abaplint.Expressions.Source);
        return `abap.console.write(${this.toJsExpression(source)});`;
      }
    };
    
    return patterns[statementType]?.(statement);
  }
  
  toJsVariable(expr) {
    // Convert ABAP variable reference to JS
    const name = expr?.concatTokens().toLowerCase();
    return name?.replace(/-/g, '_');
  }
  
  toJsExpression(expr) {
    // Simplified expression conversion
    const tokens = expr?.concatTokens();
    if (!tokens) return 'undefined';
    
    // Handle string literals
    if (tokens.startsWith("'")) {
      return '`' + tokens.slice(1, -1) + '`';
    }
    
    // Handle variables
    return this.toJsVariable(expr);
  }
  
  toJsCondition(cond) {
    // Convert ABAP condition to JS
    const tokens = cond?.concatTokens();
    if (!tokens) return 'true';
    
    return tokens
      .replace(/\sEQ\s/gi, ' === ')
      .replace(/\sNE\s/gi, ' !== ')
      .replace(/\sLT\s/gi, ' < ')
      .replace(/\sGT\s/gi, ' > ')
      .replace(/\sLE\s/gi, ' <= ')
      .replace(/\sGE\s/gi, ' >= ')
      .toLowerCase()
      .replace(/-/g, '_');
  }
  
  calculateComplexity(statement) {
    // Simple complexity metric
    let complexity = 1;
    
    // Count nested expressions
    const expressions = statement.findAllExpressions();
    complexity += Math.floor(expressions.length / 3);
    
    // Account for control flow
    if (statement.get() instanceof abaplint.Statements.If ||
        statement.get() instanceof abaplint.Statements.Loop) {
      complexity += 2;
    }
    
    return Math.min(complexity, 5); // Cap at 5
  }
  
  async generateDataset(abapDirectory) {
    const files = fs.readdirSync(abapDirectory)
      .filter(f => f.endsWith('.abap'));
    
    for (const file of files) {
      const pairs = await this.generatePairs(path.join(abapDirectory, file));
      this.dataset.push(...pairs);
    }
    
    return this.dataset;
  }
  
  saveDataset(outputPath) {
    // Save in different formats
    
    // JSONL format (for fine-tuning)
    const jsonl = this.dataset.map(pair => 
      JSON.stringify({
        prompt: `Convert this JavaScript to ABAP: ${pair.javascript}`,
        completion: pair.abap
      })
    ).join('\n');
    fs.writeFileSync(outputPath + '.jsonl', jsonl);
    
    // CSV format
    const csv = [
      'javascript,abap,type,complexity',
      ...this.dataset.map(p => 
        `"${p.javascript}","${p.abap}","${p.type}",${p.complexity}`
      )
    ].join('\n');
    fs.writeFileSync(outputPath + '.csv', csv);
    
    // Full JSON
    fs.writeFileSync(outputPath + '.json', 
      JSON.stringify(this.dataset, null, 2));
  }
}

// Usage
async function main() {
  const generator = new DatasetGenerator();
  await generator.generateDataset('./test/');
  generator.saveDataset('./dataset/abap_js_pairs');
  
  console.log(`Generated ${generator.dataset.length} training pairs`);
}

main();
```

### Python → ABAP Pattern Extractor

```javascript
// python-abap-patterns.js
class PythonABAPMapper {
  constructor() {
    this.patterns = this.loadPatterns();
  }
  
  loadPatterns() {
    return {
      // Data types
      'int': 'TYPE i',
      'float': 'TYPE f',
      'str': 'TYPE string',
      'bool': 'TYPE abap_bool',
      'list': 'TYPE TABLE OF',
      'dict': 'TYPE HASHED TABLE OF',
      
      // Control structures
      'if (.*):': 'IF $1.',
      'elif (.*):': 'ELSEIF $1.',
      'else:': 'ELSE.',
      'for (\\w+) in (.+):': 'LOOP AT $2 INTO $1.',
      'while (.*):': 'WHILE $1.',
      
      // Functions
      'def (\\w+)\\((.*)\\):': 'METHOD $1.\n  IMPORTING $2.',
      'return (.+)': 'rv_result = $1.',
      
      // Common operations
      'len\\((.+)\\)': 'lines( $1 )',
      '(.+)\\.append\\((.+)\\)': 'APPEND $2 TO $1.',
      '(.+)\\.pop\\(\\)': 'DELETE $1 INDEX lines( $1 ).',
      'print\\((.+)\\)': 'WRITE: / $1.',
    };
  }
  
  generateMappings(pythonCode) {
    const mappings = [];
    const lines = pythonCode.split('\n');
    
    for (const line of lines) {
      for (const [pattern, abapTemplate] of Object.entries(this.patterns)) {
        const regex = new RegExp(pattern);
        const match = line.match(regex);
        
        if (match) {
          let abapCode = abapTemplate;
          for (let i = 1; i < match.length; i++) {
            abapCode = abapCode.replace(`$${i}`, match[i]);
          }
          
          mappings.push({
            python: line.trim(),
            abap: abapCode,
            pattern: pattern
          });
        }
      }
    }
    
    return mappings;
  }
}
```

### API Surface Extraction

```javascript
// api-surface-extractor.js
import * as abaplint from "@abaplint/core";

class APIExtractor {
  extractAPISurface(abapFiles) {
    const api = {
      classes: [],
      interfaces: [],
      functions: [],
      types: [],
      tables: []
    };
    
    const reg = new abaplint.Registry();
    for (const file of abapFiles) {
      reg.addFile(new abaplint.MemoryFile(file.name, file.content));
    }
    reg.parse();
    
    // Extract classes
    for (const obj of reg.getObjectsByType("CLAS")) {
      const def = obj.getDefinition();
      if (def) {
        api.classes.push({
          name: def.getName(),
          methods: this.extractMethods(def),
          attributes: this.extractAttributes(def),
          events: def.getEvents()?.map(e => e.getName()),
          interfaces: def.getImplementing()?.map(i => i.name)
        });
      }
    }
    
    // Extract interfaces
    for (const obj of reg.getObjectsByType("INTF")) {
      const def = obj.getDefinition();
      if (def) {
        api.interfaces.push({
          name: def.getName(),
          methods: this.extractMethods(def),
          attributes: this.extractAttributes(def),
          types: def.getTypeDefinitions()?.getAll()?.map(t => ({
            name: t.getName(),
            type: t.getType().toText()
          }))
        });
      }
    }
    
    // Extract function modules
    for (const obj of reg.getObjectsByType("FUGR")) {
      const modules = obj.getModules();
      for (const mod of modules) {
        api.functions.push({
          name: mod.getName(),
          parameters: this.extractFunctionParams(mod)
        });
      }
    }
    
    return api;
  }
  
  extractMethods(def) {
    return def.getMethodDefinitions()?.getAll()?.map(m => ({
      name: m.getName(),
      visibility: m.getVisibility(),
      parameters: m.getParameters().getAll().map(p => ({
        name: p.getName(),
        type: p.getType().toText(),
        optional: m.getParameters().getOptional().includes(p.getName())
      })),
      returning: m.getReturning()?.getType().toText(),
      isStatic: m.isStatic()
    }));
  }
  
  extractAttributes(def) {
    const attrs = [];
    
    for (const attr of def.getAttributes()?.getAll() || []) {
      attrs.push({
        name: attr.getName(),
        type: attr.getType().toText(),
        visibility: attr.getVisibility(),
        isStatic: attr.getMeta().includes(abaplint.IdentifierMeta.Static),
        isConstant: false
      });
    }
    
    for (const constant of def.getAttributes()?.getConstants() || []) {
      attrs.push({
        name: constant.getName(),
        type: constant.getType().toText(),
        visibility: constant.getVisibility(),
        isStatic: true,
        isConstant: true,
        value: constant.getValue()
      });
    }
    
    return attrs;
  }
  
  extractFunctionParams(module) {
    // Extract function module parameters
    return {
      importing: module.getImporting(),
      exporting: module.getExporting(),
      changing: module.getChanging(),
      tables: module.getTables(),
      exceptions: module.getExceptions()
    };
  }
  
  generateAPIDoc(api) {
    let doc = '# ABAP API Surface\n\n';
    
    // Document classes
    doc += '## Classes\n\n';
    for (const cls of api.classes) {
      doc += `### ${cls.name}\n\n`;
      
      if (cls.interfaces?.length > 0) {
        doc += `Implements: ${cls.interfaces.join(', ')}\n\n`;
      }
      
      doc += '#### Methods\n';
      for (const method of cls.methods || []) {
        doc += `- **${method.name}**(${
          method.parameters.map(p => 
            `${p.name}: ${p.type}${p.optional ? '?' : ''}`
          ).join(', ')
        })`;
        if (method.returning) {
          doc += ` → ${method.returning}`;
        }
        doc += '\n';
      }
      doc += '\n';
    }
    
    return doc;
  }
}

// Usage example
async function extractAPI() {
  const extractor = new APIExtractor();
  
  // Load ABAP files
  const files = [
    { name: 'zcl_example.clas.abap', content: '...' },
    // Add more files
  ];
  
  const api = extractor.extractAPISurface(files);
  const documentation = extractor.generateAPIDoc(api);
  
  // Save API surface
  fs.writeFileSync('api-surface.json', JSON.stringify(api, null, 2));
  fs.writeFileSync('api-documentation.md', documentation);
  
  return api;
}
```

## 6. Reverse Transpilation Approach (Experimental)

```javascript
// reverse-transpiler.js
class ReverseTranspiler {
  constructor() {
    this.jsToAbapPatterns = this.loadReversePatterns();
  }
  
  loadReversePatterns() {
    return [
      // Variable declarations
      {
        pattern: /const (\w+) = new abap\.types\.(\w+)\(\)/g,
        abap: 'DATA $1 TYPE $2.'
      },
      // Method calls
      {
        pattern: /abap\.statements\.(\w+)\((.+)\)/g,
        abap: (match, stmt, args) => {
          const abapStmt = stmt.replace(/([A-Z])/g, '_$1').toUpperCase();
          return `${abapStmt} ${this.parseArgs(args)}.`;
        }
      },
      // Conditionals
      {
        pattern: /if \(abap\.compare\.(\w+)\((.+), (.+)\)\)/g,
        abap: 'IF $2 $1 $3.'
      },
      // Loops
      {
        pattern: /for await \(const (\w+) of abap\.statements\.loop\((.+)\)\)/g,
        abap: 'LOOP AT $2 INTO $1.'
      }
    ];
  }
  
  reverseTranspile(jsCode) {
    let abapCode = jsCode;
    
    for (const rule of this.jsToAbapPatterns) {
      if (typeof rule.abap === 'string') {
        abapCode = abapCode.replace(rule.pattern, rule.abap);
      } else if (typeof rule.abap === 'function') {
        abapCode = abapCode.replace(rule.pattern, rule.abap);
      }
    }
    
    return this.formatABAP(abapCode);
  }
  
  formatABAP(code) {
    // Basic ABAP formatting
    return code
      .replace(/\./g, '.\n')
      .replace(/\n\s*\n/g, '\n')
      .trim();
  }
}
```

## Summary

This guide provides multiple approaches for integrating the ABAP transpiler:

1. **Direct JavaScript API** - For Node.js applications
2. **REST API wrapper** - For language-agnostic access
3. **Go integration** - Via child process
4. **ABAP integration** - Via REST calls
5. **Dataset generation** - For ML fine-tuning
6. **API surface extraction** - For documentation and analysis

The transpiler's AST-based approach allows for sophisticated code analysis and transformation, making it ideal for building training datasets for ABAP language models.