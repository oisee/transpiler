# Bidirectional ABAP ↔ AST Transformer Design

## Overview

This document outlines the design and implementation of a bidirectional transformation system between ABAP code and its AST representation, enabling round-trip validation and dataset generation.

## Architecture

```
ABAP Source → Parser → AST/CST → Pretty Printer → ABAP'
     ↓                    ↓              ↓            ↓
   Source₁              Tree₁         Tree₂       Source₂
                          ↓              ↓
                    Equivalence Check (Tree₁ ≡ Tree₂)
```

## Tool Selection

### Parser Options Analysis

1. **abaplint** (SELECTED ✓)
   - Most mature and complete ABAP parser
   - Already integrated in transpiler
   - Full AST with position information
   - Supports all ABAP constructs
   
2. **tree-sitter-abap**
   - Fast incremental parsing
   - Good for editor integration
   - CST preservation
   
3. **ANTLR-abap**
   - Grammar-based approach
   - Good for transformation rules
   - Visitor pattern support

## Implementation Components

### 1. Parser Integration (abaplint)

```typescript
import * as abaplint from "@abaplint/core";

export class ABAPParser {
  parse(source: string): ParseResult {
    const file = new abaplint.MemoryFile("input.abap", source);
    const reg = new abaplint.Registry();
    reg.addFile(file);
    reg.parse();
    
    const obj = reg.getFirstObject();
    const abapFile = obj?.getABAPFiles()[0];
    
    return {
      ast: abapFile?.getStructure(),
      statements: abapFile?.getStatements(),
      tokens: this.extractTokens(abapFile),
      sourceMap: this.buildSourceMap(abapFile)
    };
  }
  
  private extractTokens(file: abaplint.ABAPFile) {
    // Extract all tokens with positions for CST reconstruction
    const tokens = [];
    for (const statement of file.getStatements()) {
      for (const token of statement.getTokens()) {
        tokens.push({
          type: token.getStr(),
          value: token.getStr(),
          start: token.getStart(),
          end: token.getEnd()
        });
      }
    }
    return tokens;
  }
}
```

### 2. AST Pretty Printer

```typescript
export class ABAPPrettyPrinter {
  private indentLevel = 0;
  private output: string[] = [];
  
  print(ast: abaplint.INode): string {
    this.output = [];
    this.indentLevel = 0;
    this.visitNode(ast);
    return this.output.join('\n');
  }
  
  private visitNode(node: abaplint.INode) {
    if (node instanceof abaplint.Nodes.StatementNode) {
      this.printStatement(node);
    } else if (node instanceof abaplint.Nodes.StructureNode) {
      this.printStructure(node);
    } else if (node instanceof abaplint.Nodes.ExpressionNode) {
      this.printExpression(node);
    }
    
    // Recursively visit children
    for (const child of node.getChildren()) {
      this.visitNode(child);
    }
  }
  
  private printStatement(stmt: abaplint.Nodes.StatementNode) {
    const type = stmt.get().constructor.name;
    const tokens = stmt.getTokens();
    
    // Statement-specific formatting
    switch(type) {
      case 'Data':
        this.printDataStatement(stmt);
        break;
      case 'If':
        this.printIfStatement(stmt);
        break;
      case 'Loop':
        this.printLoopStatement(stmt);
        break;
      default:
        // Generic statement printing
        this.printGeneric(stmt);
    }
  }
  
  private printDataStatement(stmt: abaplint.Nodes.StatementNode) {
    const indent = '  '.repeat(this.indentLevel);
    const tokens = stmt.getTokens().map(t => t.getStr());
    this.output.push(`${indent}${tokens.join(' ')}.`);
  }
  
  private printIfStatement(stmt: abaplint.Nodes.StatementNode) {
    const indent = '  '.repeat(this.indentLevel);
    const condition = this.extractCondition(stmt);
    this.output.push(`${indent}IF ${condition}.`);
    this.indentLevel++;
  }
  
  private formatToken(token: abaplint.Token): string {
    // Preserve original casing for identifiers
    // Uppercase keywords
    const str = token.getStr();
    if (this.isKeyword(str)) {
      return str.toUpperCase();
    }
    return str;
  }
}
```

### 3. AST Equivalence Checker

```typescript
export class ASTEquivalenceChecker {
  /**
   * Check if two ASTs are semantically equivalent
   * Ignores whitespace and formatting differences
   */
  areEquivalent(ast1: abaplint.INode, ast2: abaplint.INode): boolean {
    return this.compareNodes(ast1, ast2);
  }
  
  private compareNodes(node1: abaplint.INode, node2: abaplint.INode): boolean {
    // Compare node types
    if (node1.constructor.name !== node2.constructor.name) {
      return false;
    }
    
    // For statement nodes, compare the statement type
    if (node1 instanceof abaplint.Nodes.StatementNode && 
        node2 instanceof abaplint.Nodes.StatementNode) {
      if (node1.get().constructor.name !== node2.get().constructor.name) {
        return false;
      }
    }
    
    // Compare semantic tokens (ignore whitespace tokens)
    const tokens1 = this.getSemanticTokens(node1);
    const tokens2 = this.getSemanticTokens(node2);
    
    if (!this.compareTokenLists(tokens1, tokens2)) {
      return false;
    }
    
    // Recursively compare children
    const children1 = node1.getChildren();
    const children2 = node2.getChildren();
    
    if (children1.length !== children2.length) {
      return false;
    }
    
    for (let i = 0; i < children1.length; i++) {
      if (!this.compareNodes(children1[i], children2[i])) {
        return false;
      }
    }
    
    return true;
  }
  
  private getSemanticTokens(node: abaplint.INode): string[] {
    const tokens = [];
    
    if (node instanceof abaplint.Nodes.TokenNode) {
      const token = node.getFirstToken();
      // Normalize tokens for comparison
      tokens.push(token.getStr().toUpperCase());
    }
    
    for (const child of node.getChildren()) {
      tokens.push(...this.getSemanticTokens(child));
    }
    
    return tokens;
  }
  
  private compareTokenLists(tokens1: string[], tokens2: string[]): boolean {
    if (tokens1.length !== tokens2.length) {
      return false;
    }
    
    for (let i = 0; i < tokens1.length; i++) {
      // Case-insensitive comparison for ABAP
      if (tokens1[i].toUpperCase() !== tokens2[i].toUpperCase()) {
        return false;
      }
    }
    
    return true;
  }
}
```

### 4. Round-Trip Testing Framework

```typescript
export class RoundTripTester {
  private parser = new ABAPParser();
  private printer = new ABAPPrettyPrinter();
  private checker = new ASTEquivalenceChecker();
  
  async testRoundTrip(abapSource: string): Promise<TestResult> {
    try {
      // Step 1: Parse original ABAP
      const parseResult1 = this.parser.parse(abapSource);
      const ast1 = parseResult1.ast;
      
      // Step 2: Pretty-print to ABAP'
      const abapPrime = this.printer.print(ast1);
      
      // Step 3: Parse ABAP' back to AST
      const parseResult2 = this.parser.parse(abapPrime);
      const ast2 = parseResult2.ast;
      
      // Step 4: Check equivalence
      const equivalent = this.checker.areEquivalent(ast1, ast2);
      
      return {
        success: equivalent,
        original: abapSource,
        transformed: abapPrime,
        ast1: this.serializeAST(ast1),
        ast2: this.serializeAST(ast2),
        differences: equivalent ? [] : this.findDifferences(ast1, ast2)
      };
    } catch (error) {
      return {
        success: false,
        error: error.message,
        original: abapSource
      };
    }
  }
  
  async testCorpus(corpusPath: string): Promise<CorpusTestResult> {
    const files = await this.loadCorpusFiles(corpusPath);
    const results = [];
    
    for (const file of files) {
      const result = await this.testRoundTrip(file.content);
      results.push({
        file: file.name,
        ...result
      });
    }
    
    return {
      total: results.length,
      passed: results.filter(r => r.success).length,
      failed: results.filter(r => !r.success).length,
      results
    };
  }
}
```

### 5. Dataset Generator

```typescript
export class ABAPDatasetGenerator {
  private parser = new ABAPParser();
  private printer = new ABAPPrettyPrinter();
  
  /**
   * Generate training pairs for ML models
   */
  async generateDataset(corpusPath: string): Promise<Dataset> {
    const pairs = [];
    const files = await this.loadABAPFiles(corpusPath);
    
    for (const file of files) {
      // Parse to AST
      const parseResult = this.parser.parse(file.content);
      
      // Generate variations
      const variations = this.generateVariations(parseResult.ast);
      
      // Create training pairs
      for (const variation of variations) {
        pairs.push({
          input: variation.abap,
          output: this.printer.print(variation.ast),
          ast: this.serializeAST(variation.ast),
          type: variation.type
        });
      }
      
      // Extract statement-level pairs
      for (const stmt of parseResult.statements) {
        pairs.push({
          input: this.extractStatement(stmt),
          ast: this.serializeStatement(stmt),
          type: stmt.get().constructor.name
        });
      }
    }
    
    return {
      pairs,
      statistics: this.calculateStatistics(pairs)
    };
  }
  
  private generateVariations(ast: abaplint.INode) {
    // Generate formatting variations
    return [
      { ast, abap: this.printer.print(ast), type: 'original' },
      { ast, abap: this.printCompact(ast), type: 'compact' },
      { ast, abap: this.printExpanded(ast), type: 'expanded' }
    ];
  }
}
```

## Usage Examples

### Basic Round-Trip Test

```typescript
const tester = new RoundTripTester();

const abapCode = `
DATA: lv_test TYPE string.
lv_test = 'Hello World'.
WRITE lv_test.
`;

const result = await tester.testRoundTrip(abapCode);
console.log(`Round-trip ${result.success ? 'PASSED' : 'FAILED'}`);
```

### Corpus Testing

```typescript
const tester = new RoundTripTester();
const results = await tester.testCorpus('/Users/alice/dev/abap-ts/tree-sitter-abap/_corpus');

console.log(`
Corpus Test Results:
  Total: ${results.total}
  Passed: ${results.passed}
  Failed: ${results.failed}
  Success Rate: ${(results.passed / results.total * 100).toFixed(2)}%
`);
```

### Dataset Generation

```typescript
const generator = new ABAPDatasetGenerator();
const dataset = await generator.generateDataset('/Users/alice/dev/abap-ts/tree-sitter-abap/_corpus');

// Save as JSONL for training
fs.writeFileSync('abap_dataset.jsonl', 
  dataset.pairs.map(p => JSON.stringify(p)).join('\n')
);
```

## Benefits

1. **Validation**: Ensures parser preserves semantics
2. **Dataset Quality**: High-quality training data for ML
3. **Code Formatting**: Consistent ABAP formatting
4. **Transformation Verification**: Validates AST transformations
5. **Regression Testing**: Catches parser regressions

## Next Steps

1. Implement the complete system
2. Test with full ABAP corpus
3. Generate comprehensive datasets
4. Integrate with transpiler pipeline
5. Build transformation rules for optimization