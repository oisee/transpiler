# AST Representation Format for ABAP Code Rewriting and Transformation

## Executive Summary

This document describes the **Abstract Syntax Tree (AST) representation format** used in our ABAP bidirectional transformer and how it can be leveraged for systematic ABAP code rewriting, refactoring, and transformation tasks. The AST format provides a structured, programmatic way to analyze, modify, and generate ABAP code while preserving semantic equivalence.

## AST Structure Overview

### Core AST Elements

Our ABAP AST representation consists of three primary components:

```javascript
const ASTRepresentation = {
  statements: [/* StatementNode[] */],
  structure: StructureNode,
  metadata: {
    sourceFile: string,
    parseTime: timestamp,
    version: string
  }
}
```

### StatementNode Format

Each ABAP statement is represented as:

```javascript
const StatementNode = {
  type: string,           // Statement type (Data, Move, Write, If, etc.)
  tokens: Token[],        // Array of lexical tokens
  position: {
    start: {row: number, col: number},
    end: {row: number, col: number}
  },
  children: ExpressionNode[],
  metadata: object
}
```

### Token Representation

Individual tokens maintain their lexical information:

```javascript
const Token = {
  value: string,          // Raw token text
  type: TokenType,        // Identifier, Keyword, Operator, etc.
  position: Position,     // Source location
  normalized: string      // Uppercase normalized form
}
```

## Practical AST Examples

### Simple Statement: DATA Declaration

```abap
DATA lv_count TYPE i.
```

**AST Representation:**
```javascript
{
  type: "Data",
  tokens: [
    {value: "DATA", type: "Keyword", normalized: "DATA"},
    {value: "lv_count", type: "Identifier", normalized: "LV_COUNT"},
    {value: "TYPE", type: "Keyword", normalized: "TYPE"},
    {value: "i", type: "Type", normalized: "I"},
    {value: ".", type: "Punctuation", normalized: "."}
  ],
  tokenString: "DATA LV_COUNT TYPE I .",
  semantics: {
    operation: "variable_declaration",
    variable: "lv_count",
    dataType: "i"
  }
}
```

### Complex Statement: IF Block

```abap
IF lv_value > 0.
  WRITE 'Positive'.
ENDIF.
```

**AST Representation:**
```javascript
[
  {
    type: "If",
    tokens: ["IF", "lv_value", ">", "0", "."],
    condition: {
      left: "lv_value",
      operator: ">", 
      right: "0"
    }
  },
  {
    type: "Write", 
    tokens: ["WRITE", "'Positive'", "."],
    output: "'Positive'"
  },
  {
    type: "EndIf",
    tokens: ["ENDIF", "."]
  }
]
```

### Colon-Separated Lists

```abap
DATA: lv_first TYPE string,
      lv_second TYPE i.
```

**AST Representation:**
```javascript
// Note: Parser splits colon lists into separate statements
[
  {
    type: "Data",
    tokens: ["DATA", "lv_first", "TYPE", "string", ","],
    originalSyntax: "colon_list",
    groupId: "data_group_1"
  },
  {
    type: "Data", 
    tokens: ["DATA", "lv_second", "TYPE", "i", "."],
    originalSyntax: "colon_list",
    groupId: "data_group_1"
  }
]
```

## AST-Based ABAP Rewriting Patterns

### 1. Variable Renaming

```javascript
function renameVariable(ast, oldName, newName) {
  return ast.statements.map(stmt => ({
    ...stmt,
    tokens: stmt.tokens.map(token => 
      token.type === 'Identifier' && token.normalized === oldName.toUpperCase()
        ? {...token, value: newName, normalized: newName.toUpperCase()}
        : token
    )
  }));
}

// Usage
const renamedAST = renameVariable(originalAST, 'lv_count', 'lv_counter');
```

### 2. Type Modernization

```javascript
function modernizeTypes(ast) {
  const typeMapping = {
    'c': 'string',
    'n': 'string', 
    'p': 'p decimals 2'
  };
  
  return ast.statements.map(stmt => {
    if (stmt.type === 'Data') {
      return modernizeDataStatement(stmt, typeMapping);
    }
    return stmt;
  });
}

function modernizeDataStatement(stmt, typeMap) {
  const tokens = stmt.tokens.slice();
  const typeIndex = tokens.findIndex(t => t.normalized === 'TYPE');
  
  if (typeIndex !== -1 && typeIndex + 1 < tokens.length) {
    const currentType = tokens[typeIndex + 1].normalized;
    if (typeMap[currentType]) {
      tokens[typeIndex + 1] = {
        ...tokens[typeIndex + 1],
        value: typeMap[currentType],
        normalized: typeMap[currentType].toUpperCase()
      };
    }
  }
  
  return {...stmt, tokens};
}
```

### 3. Control Flow Transformation

```javascript
function convertWhileToFor(ast) {
  return ast.statements.map((stmt, index, stmts) => {
    if (stmt.type === 'While') {
      return convertWhileLoop(stmt, stmts, index);
    }
    return stmt;
  });
}

function convertWhileLoop(whileStmt, allStmts, startIndex) {
  // Find ENDWHILE
  let endIndex = startIndex + 1;
  let depth = 1;
  
  while (endIndex < allStmts.length && depth > 0) {
    if (allStmts[endIndex].type === 'While') depth++;
    if (allStmts[endIndex].type === 'EndWhile') depth--;
    endIndex++;
  }
  
  // Convert to DO loop with EXIT condition
  return [
    {type: 'Do', tokens: ['DO', '.']},
    {type: 'If', tokens: ['IF', 'NOT', '(', ...whileStmt.condition, ')', '.']},
    {type: 'Exit', tokens: ['EXIT', '.']},
    {type: 'EndIf', tokens: ['ENDIF', '.']},
    ...allStmts.slice(startIndex + 1, endIndex - 1),
    {type: 'EndDo', tokens: ['ENDDO', '.']}
  ];
}
```

### 4. Method Extraction

```javascript
function extractMethod(ast, lineStart, lineEnd, methodName) {
  const extractedStatements = ast.statements.slice(lineStart - 1, lineEnd);
  
  // Create method definition
  const methodDefinition = {
    type: 'Method',
    tokens: ['METHOD', methodName, '.'],
    body: extractedStatements,
    returnType: inferReturnType(extractedStatements)
  };
  
  // Create method call
  const methodCall = {
    type: 'MethodCall', 
    tokens: ['CALL', 'METHOD', methodName, '.']
  };
  
  // Replace original statements with method call
  const newStatements = [
    ...ast.statements.slice(0, lineStart - 1),
    methodCall,
    ...ast.statements.slice(lineEnd)
  ];
  
  return {
    ...ast,
    statements: newStatements,
    methods: [...(ast.methods || []), methodDefinition]
  };
}
```

## Advanced AST Transformations

### 1. Semantic Analysis Integration

```javascript
class ABAPSemanticAnalyzer {
  analyzeDataFlow(ast) {
    const variables = new Map();
    const dataFlow = [];
    
    for (const stmt of ast.statements) {
      switch (stmt.type) {
        case 'Data':
          this.processVariableDeclaration(stmt, variables);
          break;
        case 'Move':
          this.processAssignment(stmt, variables, dataFlow);
          break;
        case 'Write':
          this.processOutput(stmt, variables, dataFlow);
          break;
      }
    }
    
    return {variables, dataFlow};
  }
  
  processVariableDeclaration(stmt, variables) {
    const varName = stmt.tokens.find(t => t.type === 'Identifier');
    const varType = this.extractType(stmt);
    
    if (varName) {
      variables.set(varName.normalized, {
        name: varName.value,
        type: varType,
        declared: true,
        used: false
      });
    }
  }
  
  processAssignment(stmt, variables, dataFlow) {
    // Extract target = source pattern
    const equalIndex = stmt.tokens.findIndex(t => t.value === '=');
    if (equalIndex > 0 && equalIndex < stmt.tokens.length - 1) {
      const target = stmt.tokens[equalIndex - 1].normalized;
      const sources = this.extractIdentifiers(stmt.tokens.slice(equalIndex + 1));
      
      dataFlow.push({
        type: 'assignment',
        target,
        sources,
        statement: stmt
      });
    }
  }
}
```

### 2. Pattern-Based Refactoring

```javascript
const refactoringPatterns = {
  // Replace direct WRITE with logging
  'modernize_output': {
    match: stmt => stmt.type === 'Write',
    transform: stmt => ({
      type: 'MethodCall',
      tokens: ['cl_log=>info', '(', ...stmt.tokens.slice(1, -1), ')', '.'],
      metadata: {originalPattern: 'direct_write'}
    })
  },
  
  // Convert nested IFs to CASE
  'nested_if_to_case': {
    match: (stmts, index) => {
      return stmts[index].type === 'If' && 
             this.hasNestedElseIfs(stmts, index);
    },
    transform: (stmts, index) => {
      return this.convertToCaseStatement(stmts, index);
    }
  },
  
  // Inline simple variables
  'inline_variables': {
    match: stmt => stmt.type === 'Data' && this.isSimpleConstant(stmt),
    transform: (stmt, context) => {
      return this.inlineVariableUsages(stmt, context);
    }
  }
};
```

### 3. Code Quality Analysis

```javascript
class ABAPQualityAnalyzer {
  analyzeComplexity(ast) {
    let complexity = 1; // Base complexity
    
    for (const stmt of ast.statements) {
      switch (stmt.type) {
        case 'If':
        case 'ElseIf':
        case 'While':
        case 'Do':
        case 'Loop':
          complexity++;
          break;
        case 'Case':
          complexity += this.countCaseConditions(stmt);
          break;
        case 'Try':
          complexity += this.countCatchBlocks(stmt);
          break;
      }
    }
    
    return {
      cyclomaticComplexity: complexity,
      recommendation: complexity > 10 ? 'refactor' : 'acceptable'
    };
  }
  
  findCodeSmells(ast) {
    const smells = [];
    
    // Long parameter lists
    const methods = ast.statements.filter(s => s.type === 'Method');
    methods.forEach(method => {
      const paramCount = this.countParameters(method);
      if (paramCount > 5) {
        smells.push({
          type: 'long_parameter_list',
          location: method.position,
          severity: 'warning'
        });
      }
    });
    
    // Duplicate code detection
    const duplicates = this.findDuplicateBlocks(ast);
    duplicates.forEach(dup => {
      smells.push({
        type: 'duplicate_code',
        locations: dup.locations,
        severity: 'info'
      });
    });
    
    return smells;
  }
}
```

## Integration with ML/AI Systems

### 1. Training Data Generation

```javascript
function generateTrainingPairs(ast) {
  const pairs = [];
  
  // Before/after refactoring pairs
  const refactored = applyRefactorings(ast);
  pairs.push({
    input: astToABAP(ast),
    output: astToABAP(refactored),
    transformation: 'refactoring',
    confidence: 0.95
  });
  
  // Semantic equivalence pairs
  const semanticVariants = generateSemanticVariants(ast);
  semanticVariants.forEach(variant => {
    pairs.push({
      input: astToABAP(ast),
      output: astToABAP(variant),
      transformation: 'semantic_equivalent',
      confidence: 1.0
    });
  });
  
  return pairs;
}
```

### 2. Code Generation Templates

```javascript
const codeTemplates = {
  'data_validation': (field, type, constraints) => ({
    type: 'ValidationBlock',
    pattern: `
      IF ${field} IS INITIAL.
        MESSAGE 'Field ${field} is required' TYPE 'E'.
      ENDIF.
      ${constraints.map(c => generateConstraintCheck(field, c)).join('\n')}
    `,
    ast: generateValidationAST(field, type, constraints)
  }),
  
  'error_handling': (operation, errorVar) => ({
    type: 'ErrorHandlingBlock',
    pattern: `
      TRY.
        ${operation}
      CATCH cx_root INTO ${errorVar}.
        MESSAGE ${errorVar}->get_text() TYPE 'E'.
      ENDTRY.
    `,
    ast: generateErrorHandlingAST(operation, errorVar)
  })
};
```

## Production Applications

### 1. Automated Refactoring Pipeline

```javascript
class ABAPRefactoringPipeline {
  constructor() {
    this.transformations = [
      new VariableNormalizer(),
      new TypeModernizer(), 
      new ControlFlowOptimizer(),
      new MethodExtractor(),
      new CodeStyleFixer()
    ];
  }
  
  async processFile(abapSource) {
    // Parse to AST
    let ast = parseABAP(abapSource);
    
    // Apply transformations
    for (const transformation of this.transformations) {
      const result = await transformation.apply(ast);
      if (result.success) {
        ast = result.ast;
      }
    }
    
    // Generate improved ABAP
    const improvedCode = astToABAP(ast);
    
    // Validate equivalence
    const validation = validateEquivalence(abapSource, improvedCode);
    
    return {
      original: abapSource,
      improved: improvedCode,
      transformations: this.transformations.map(t => t.summary),
      validation
    };
  }
}
```

### 2. Code Analysis Dashboard

```javascript
class ABAPAnalyticsDashboard {
  generateMetrics(ast) {
    return {
      complexity: this.calculateComplexity(ast),
      maintainability: this.assessMaintainability(ast),
      patterns: this.identifyPatterns(ast),
      suggestions: this.generateSuggestions(ast),
      modernization: this.assessModernizationPotential(ast)
    };
  }
  
  visualizeAST(ast) {
    return {
      flowDiagram: this.generateFlowDiagram(ast),
      dependencyGraph: this.buildDependencyGraph(ast),
      complexityHeatmap: this.createComplexityHeatmap(ast)
    };
  }
}
```

## Performance Considerations

### AST Processing Optimization

```javascript
class OptimizedASTProcessor {
  constructor() {
    this.cache = new Map();
    this.memoizedFunctions = new Map();
  }
  
  processLargeAST(ast) {
    // Use streaming processing for large ASTs
    const chunks = this.chunkAST(ast, 1000);
    
    return Promise.all(
      chunks.map(chunk => this.processChunk(chunk))
    ).then(results => this.mergeResults(results));
  }
  
  chunkAST(ast, chunkSize) {
    const chunks = [];
    for (let i = 0; i < ast.statements.length; i += chunkSize) {
      chunks.push({
        ...ast,
        statements: ast.statements.slice(i, i + chunkSize),
        chunkIndex: i / chunkSize
      });
    }
    return chunks;
  }
}
```

## Conclusion

The AST representation format provides a powerful foundation for systematic ABAP code transformation and analysis. Key benefits include:

✅ **Programmatic Analysis** - Systematic code examination and pattern detection  
✅ **Reliable Transformations** - Semantic-preserving code modifications  
✅ **Quality Assessment** - Automated code quality and complexity analysis  
✅ **ML Integration** - Training data generation and AI-assisted development  
✅ **Scalable Processing** - Efficient handling of large codebases  

This AST-based approach enables sophisticated ABAP tooling that goes beyond simple text processing to provide true semantic understanding and transformation capabilities. The format supports both automated refactoring pipelines and interactive development tools, making it valuable for modernization projects, code quality improvement, and educational applications.

**Future Extensions:**
- Cross-system dependency analysis
- Automated testing generation from AST patterns  
- Performance optimization recommendations
- Security vulnerability detection
- Documentation generation from code structure

The AST representation serves as a universal intermediate format that bridges the gap between raw ABAP source code and high-level semantic operations, enabling the next generation of ABAP development tools.