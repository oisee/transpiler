/**
 * Bidirectional ABAP ↔ AST Transformer
 * 
 * Provides round-trip transformation between ABAP source code and AST
 * with validation to ensure semantic equivalence.
 */

import * as abaplint from "@abaplint/core";
import * as fs from "fs";
import * as path from "path";

// ============================================
// ABAP Parser using abaplint
// ============================================

export class ABAPParser {
  parse(source, filename = "input.abap") {
    const file = new abaplint.MemoryFile(filename, source);
    const reg = new abaplint.Registry();
    reg.addFile(file);
    reg.parse();
    
    const obj = reg.getFirstObject();
    if (!obj) {
      throw new Error("No ABAP object found in source");
    }
    
    const abapFile = obj.getABAPFiles()[0];
    if (!abapFile) {
      throw new Error("No ABAP file found in object");
    }
    
    return {
      ast: abapFile.getStructure(),
      statements: abapFile.getStatements(),
      tokens: this.extractTokens(abapFile),
      sourceMap: this.buildSourceMap(abapFile),
      file: abapFile
    };
  }
  
  extractTokens(file) {
    const tokens = [];
    for (const statement of file.getStatements()) {
      for (const token of statement.getTokens()) {
        tokens.push({
          type: "Token",
          value: token.getStr(),
          start: {
            row: token.getStart().getRow(),
            col: token.getStart().getCol()
          },
          end: {
            row: token.getEnd().getRow(),
            col: token.getEnd().getCol()
          }
        });
      }
    }
    return tokens;
  }
  
  buildSourceMap(file) {
    const map = new Map();
    for (const statement of file.getStatements()) {
      const key = `${statement.getStart().getRow()}:${statement.getStart().getCol()}`;
      map.set(key, {
        statement: statement.get().constructor.name,
        tokens: statement.getTokens().map(t => t.getStr())
      });
    }
    return map;
  }
}

// ============================================
// ABAP Pretty Printer - AST to ABAP
// ============================================

export class ABAPPrettyPrinter {
  constructor(options = {}) {
    this.options = {
      indent: "  ",
      uppercaseKeywords: true,
      preserveComments: true,
      ...options
    };
    this.reset();
  }
  
  reset() {
    this.indentLevel = 0;
    this.output = [];
    this.currentLine = [];
  }
  
  print(ast) {
    this.reset();
    this.visitNode(ast);
    return this.output.join("\n");
  }
  
  visitNode(node) {
    if (!node) return;
    
    const nodeType = node.constructor.name;
    
    // Handle different node types
    if (node.get && typeof node.get === 'function') {
      // Statement node
      const stmtType = node.get().constructor.name;
      this.printStatement(node, stmtType);
    } else if (node.getChildren) {
      // Structure or expression node
      for (const child of node.getChildren()) {
        this.visitNode(child);
      }
    }
  }
  
  printStatement(node, type) {
    const indent = this.options.indent.repeat(this.indentLevel);
    const tokens = node.getTokens ? node.getTokens() : [];
    
    // Build statement string
    let stmt = indent;
    
    // Special handling for different statement types
    switch(type) {
      case 'Data':
        stmt += this.formatDataStatement(tokens);
        break;
      case 'Move':
        stmt += this.formatMoveStatement(tokens);
        break;
      case 'Write':
        stmt += this.formatWriteStatement(tokens);
        break;
      case 'If':
        stmt += this.formatIfStatement(tokens);
        this.indentLevel++;
        break;
      case 'ElseIf':
        this.indentLevel--;
        stmt = this.options.indent.repeat(this.indentLevel) + this.formatElseIfStatement(tokens);
        this.indentLevel++;
        break;
      case 'Else':
        this.indentLevel--;
        stmt = this.options.indent.repeat(this.indentLevel) + this.formatElseStatement(tokens);
        this.indentLevel++;
        break;
      case 'EndIf':
        this.indentLevel--;
        stmt = this.options.indent.repeat(this.indentLevel) + this.formatEndIfStatement(tokens);
        break;
      case 'Loop':
        stmt += this.formatLoopStatement(tokens);
        this.indentLevel++;
        break;
      case 'EndLoop':
        this.indentLevel--;
        stmt = this.options.indent.repeat(this.indentLevel) + this.formatEndLoopStatement(tokens);
        break;
      default:
        // Generic formatting
        stmt += this.formatGenericStatement(tokens);
    }
    
    this.output.push(stmt);
  }
  
  formatDataStatement(tokens) {
    let result = "DATA";
    let colonMode = false;
    
    for (let i = 1; i < tokens.length; i++) {
      const token = tokens[i].getStr();
      if (token === ':') {
        result += ':';
        colonMode = true;
      } else if (token === ',') {
        result += ',';
      } else if (token === '.') {
        // Don't add space before period
        result += '.';
      } else {
        result += ' ' + this.formatToken(token);
      }
    }
    
    if (!result.endsWith('.')) {
      result += '.';
    }
    
    return result;
  }
  
  formatMoveStatement(tokens) {
    // Handle both old and new syntax
    // Old: MOVE x TO y.
    // New: y = x.
    
    const parts = [];
    for (const token of tokens) {
      parts.push(this.formatToken(token.getStr()));
    }
    
    // Check for assignment operator
    if (parts.includes('=')) {
      return parts.join(' ') + '.';
    } else {
      return 'MOVE ' + parts.slice(1).join(' ') + '.';
    }
  }
  
  formatWriteStatement(tokens) {
    let result = "WRITE";
    for (let i = 1; i < tokens.length; i++) {
      result += ' ' + this.formatToken(tokens[i].getStr());
    }
    return result + '.';
  }
  
  formatIfStatement(tokens) {
    let result = "IF";
    for (let i = 1; i < tokens.length; i++) {
      result += ' ' + this.formatToken(tokens[i].getStr());
    }
    return result + '.';
  }
  
  formatElseIfStatement(tokens) {
    let result = "ELSEIF";
    for (let i = 1; i < tokens.length; i++) {
      result += ' ' + this.formatToken(tokens[i].getStr());
    }
    return result + '.';
  }
  
  formatElseStatement(tokens) {
    return "ELSE.";
  }
  
  formatEndIfStatement(tokens) {
    return "ENDIF.";
  }
  
  formatLoopStatement(tokens) {
    let result = "LOOP";
    for (let i = 1; i < tokens.length; i++) {
      result += ' ' + this.formatToken(tokens[i].getStr());
    }
    return result + '.';
  }
  
  formatEndLoopStatement(tokens) {
    return "ENDLOOP.";
  }
  
  formatGenericStatement(tokens) {
    const parts = [];
    for (const token of tokens) {
      parts.push(this.formatToken(token.getStr()));
    }
    let result = parts.join(' ');
    if (!result.endsWith('.')) {
      result += '.';
    }
    return result;
  }
  
  formatToken(str) {
    if (this.options.uppercaseKeywords && this.isKeyword(str)) {
      return str.toUpperCase();
    }
    return str;
  }
  
  isKeyword(str) {
    const keywords = [
      'DATA', 'TYPE', 'VALUE', 'LENGTH', 'DECIMALS',
      'MOVE', 'TO', 'WRITE', 'IF', 'ELSE', 'ELSEIF', 'ENDIF',
      'LOOP', 'AT', 'INTO', 'ENDLOOP', 'WHILE', 'ENDWHILE',
      'DO', 'TIMES', 'ENDDO', 'CASE', 'WHEN', 'ENDCASE',
      'FORM', 'ENDFORM', 'PERFORM', 'USING', 'CHANGING',
      'FUNCTION', 'ENDFUNCTION', 'CALL', 'METHOD', 'ENDMETHOD',
      'CLASS', 'ENDCLASS', 'INTERFACE', 'ENDINTERFACE',
      'SELECT', 'FROM', 'WHERE', 'INTO', 'ENDSELECT',
      'APPEND', 'INSERT', 'DELETE', 'MODIFY', 'CLEAR', 'REFRESH',
      'AND', 'OR', 'NOT', 'EQ', 'NE', 'LT', 'LE', 'GT', 'GE',
      'IS', 'INITIAL', 'BOUND', 'ASSIGNED'
    ];
    
    return keywords.includes(str.toUpperCase());
  }
}

// ============================================
// AST Equivalence Checker
// ============================================

export class ASTEquivalenceChecker {
  areEquivalent(ast1, ast2) {
    return this.compareNodes(ast1, ast2);
  }
  
  compareNodes(node1, node2) {
    if (!node1 && !node2) return true;
    if (!node1 || !node2) return false;
    
    // Compare node types
    const type1 = node1.constructor.name;
    const type2 = node2.constructor.name;
    
    if (type1 !== type2) {
      console.log(`Node type mismatch: ${type1} vs ${type2}`);
      return false;
    }
    
    // For statement nodes, compare statement types
    if (node1.get && node2.get) {
      const stmtType1 = node1.get().constructor.name;
      const stmtType2 = node2.get().constructor.name;
      
      if (stmtType1 !== stmtType2) {
        console.log(`Statement type mismatch: ${stmtType1} vs ${stmtType2}`);
        return false;
      }
      
      // Compare tokens
      const tokens1 = this.normalizeTokens(node1.getTokens ? node1.getTokens() : []);
      const tokens2 = this.normalizeTokens(node2.getTokens ? node2.getTokens() : []);
      
      if (!this.compareTokenLists(tokens1, tokens2)) {
        console.log(`Token mismatch in ${stmtType1}`);
        console.log(`  Tokens1: ${tokens1.join(' ')}`);
        console.log(`  Tokens2: ${tokens2.join(' ')}`);
        return false;
      }
    }
    
    // Compare children recursively
    const children1 = node1.getChildren ? node1.getChildren() : [];
    const children2 = node2.getChildren ? node2.getChildren() : [];
    
    if (children1.length !== children2.length) {
      console.log(`Children count mismatch: ${children1.length} vs ${children2.length}`);
      return false;
    }
    
    for (let i = 0; i < children1.length; i++) {
      if (!this.compareNodes(children1[i], children2[i])) {
        return false;
      }
    }
    
    return true;
  }
  
  normalizeTokens(tokens) {
    return tokens.map(t => {
      const str = typeof t === 'string' ? t : t.getStr();
      // Normalize to uppercase for case-insensitive comparison
      return str.toUpperCase();
    });
  }
  
  compareTokenLists(tokens1, tokens2) {
    if (tokens1.length !== tokens2.length) {
      return false;
    }
    
    for (let i = 0; i < tokens1.length; i++) {
      if (tokens1[i] !== tokens2[i]) {
        return false;
      }
    }
    
    return true;
  }
}

// ============================================
// Round-Trip Testing Framework
// ============================================

export class RoundTripTester {
  constructor() {
    this.parser = new ABAPParser();
    this.printer = new ABAPPrettyPrinter();
    this.checker = new ASTEquivalenceChecker();
  }
  
  async testRoundTrip(abapSource) {
    try {
      console.log("\n=== Round-Trip Test ===");
      console.log("Original ABAP:");
      console.log(abapSource);
      
      // Step 1: Parse original ABAP
      console.log("\n1. Parsing original ABAP...");
      const parseResult1 = this.parser.parse(abapSource);
      const ast1 = parseResult1.ast;
      
      // Step 2: Pretty-print to ABAP'
      console.log("2. Pretty-printing AST to ABAP'...");
      const abapPrime = this.printer.print(ast1);
      console.log("\nTransformed ABAP':");
      console.log(abapPrime);
      
      // Step 3: Parse ABAP' back to AST
      console.log("\n3. Parsing ABAP' back to AST...");
      const parseResult2 = this.parser.parse(abapPrime);
      const ast2 = parseResult2.ast;
      
      // Step 4: Check equivalence
      console.log("4. Checking AST equivalence...");
      const equivalent = this.checker.areEquivalent(ast1, ast2);
      
      console.log(`\nResult: ${equivalent ? '✓ PASSED' : '✗ FAILED'}`);
      
      return {
        success: equivalent,
        original: abapSource,
        transformed: abapPrime,
        ast1: this.serializeAST(ast1),
        ast2: this.serializeAST(ast2)
      };
    } catch (error) {
      console.error("Error in round-trip test:", error.message);
      return {
        success: false,
        error: error.message,
        original: abapSource
      };
    }
  }
  
  serializeAST(ast) {
    if (!ast) return null;
    
    const serialize = (node, depth = 0) => {
      const indent = "  ".repeat(depth);
      let result = indent + node.constructor.name;
      
      if (node.get) {
        result += ` [${node.get().constructor.name}]`;
        
        if (node.getTokens) {
          const tokens = node.getTokens().map(t => t.getStr());
          result += `: ${tokens.join(' ')}`;
        }
      }
      
      result += "\n";
      
      if (node.getChildren) {
        for (const child of node.getChildren()) {
          result += serialize(child, depth + 1);
        }
      }
      
      return result;
    };
    
    return serialize(ast);
  }
  
  async testCorpus(corpusPath) {
    console.log(`\n=== Testing Corpus: ${corpusPath} ===\n`);
    
    const files = this.loadCorpusFiles(corpusPath);
    const results = [];
    
    for (const file of files) {
      console.log(`\nTesting: ${file.name}`);
      console.log("-".repeat(40));
      
      const result = await this.testRoundTrip(file.content);
      results.push({
        file: file.name,
        ...result
      });
    }
    
    // Summary
    const passed = results.filter(r => r.success).length;
    const failed = results.filter(r => !r.success).length;
    
    console.log("\n" + "=".repeat(50));
    console.log("CORPUS TEST SUMMARY");
    console.log("=".repeat(50));
    console.log(`Total Files: ${results.length}`);
    console.log(`Passed: ${passed}`);
    console.log(`Failed: ${failed}`);
    console.log(`Success Rate: ${(passed / results.length * 100).toFixed(2)}%`);
    
    if (failed > 0) {
      console.log("\nFailed Files:");
      results.filter(r => !r.success).forEach(r => {
        console.log(`  - ${r.file}: ${r.error || 'AST mismatch'}`);
      });
    }
    
    return {
      total: results.length,
      passed,
      failed,
      results
    };
  }
  
  loadCorpusFiles(corpusPath) {
    const files = [];
    
    if (fs.existsSync(corpusPath)) {
      const entries = fs.readdirSync(corpusPath);
      
      for (const entry of entries) {
        if (entry.endsWith('.abap')) {
          const filePath = path.join(corpusPath, entry);
          const content = fs.readFileSync(filePath, 'utf8');
          files.push({
            name: entry,
            path: filePath,
            content
          });
        }
      }
    }
    
    return files;
  }
}

// ============================================
// Dataset Generator
// ============================================

export class ABAPDatasetGenerator {
  constructor() {
    this.parser = new ABAPParser();
    this.printer = new ABAPPrettyPrinter();
  }
  
  async generateDataset(corpusPath) {
    const pairs = [];
    const files = this.loadABAPFiles(corpusPath);
    
    for (const file of files) {
      try {
        // Parse to AST
        const parseResult = this.parser.parse(file.content);
        
        // Generate original pair
        pairs.push({
          source: file.content,
          ast: this.serializeAST(parseResult.ast),
          formatted: this.printer.print(parseResult.ast),
          type: 'full_program',
          file: file.name
        });
        
        // Extract statement-level pairs
        for (const stmt of parseResult.statements) {
          const stmtTokens = stmt.getTokens().map(t => t.getStr()).join(' ');
          pairs.push({
            source: stmtTokens + '.',
            ast: this.serializeStatement(stmt),
            type: stmt.get().constructor.name,
            file: file.name
          });
        }
      } catch (error) {
        console.error(`Error processing ${file.name}:`, error.message);
      }
    }
    
    // Calculate statistics
    const stats = {
      total_pairs: pairs.length,
      by_type: {}
    };
    
    for (const pair of pairs) {
      stats.by_type[pair.type] = (stats.by_type[pair.type] || 0) + 1;
    }
    
    return {
      pairs,
      statistics: stats
    };
  }
  
  loadABAPFiles(corpusPath) {
    const files = [];
    
    if (fs.existsSync(corpusPath)) {
      const entries = fs.readdirSync(corpusPath);
      
      for (const entry of entries) {
        if (entry.endsWith('.abap')) {
          const filePath = path.join(corpusPath, entry);
          const content = fs.readFileSync(filePath, 'utf8');
          files.push({
            name: entry,
            path: filePath,
            content
          });
        }
      }
    }
    
    return files;
  }
  
  serializeAST(ast) {
    // Simplified AST serialization for dataset
    return JSON.stringify(this.nodeToObject(ast), null, 2);
  }
  
  serializeStatement(stmt) {
    return JSON.stringify({
      type: stmt.get().constructor.name,
      tokens: stmt.getTokens().map(t => t.getStr())
    });
  }
  
  nodeToObject(node) {
    if (!node) return null;
    
    const obj = {
      type: node.constructor.name
    };
    
    if (node.get) {
      obj.statement = node.get().constructor.name;
      
      if (node.getTokens) {
        obj.tokens = node.getTokens().map(t => t.getStr());
      }
    }
    
    if (node.getChildren && node.getChildren().length > 0) {
      obj.children = node.getChildren().map(c => this.nodeToObject(c));
    }
    
    return obj;
  }
}

// ============================================
// Main test runner
// ============================================

async function main() {
  // Test with simple examples
  const tester = new RoundTripTester();
  
  // Test 1: Simple data declaration and assignment
  await tester.testRoundTrip(`
DATA lv_test TYPE string.
lv_test = 'Hello World'.
WRITE lv_test.
`);
  
  // Test 2: IF statement
  await tester.testRoundTrip(`
DATA lv_number TYPE i.
lv_number = 42.
IF lv_number > 0.
  WRITE 'Positive'.
ELSE.
  WRITE 'Non-positive'.
ENDIF.
`);
  
  // Test corpus if available
  const corpusPath = '/Users/alice/dev/abap-ts/tree-sitter-abap/_corpus';
  if (fs.existsSync(corpusPath)) {
    await tester.testCorpus(corpusPath);
  }
  
  // Generate dataset
  const generator = new ABAPDatasetGenerator();
  const dataset = await generator.generateDataset(corpusPath);
  
  console.log("\n=== Dataset Generation ===");
  console.log(`Generated ${dataset.pairs.length} training pairs`);
  console.log("Distribution by type:");
  for (const [type, count] of Object.entries(dataset.statistics.by_type)) {
    console.log(`  ${type}: ${count}`);
  }
  
  // Save dataset
  fs.writeFileSync(
    'abap_dataset.jsonl',
    dataset.pairs.map(p => JSON.stringify(p)).join('\n')
  );
  console.log("\nDataset saved to abap_dataset.jsonl");
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch(console.error);
}