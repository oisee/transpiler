#!/usr/bin/env node

/**
 * Dataset Generator for Universal Code-to-ABAP Translation
 * 
 * Implements Article 020's masking strategies:
 * - Level 1: Expression-level masking
 * - Level 2: Statement-level masking  
 * - Level 3: Block-level masking
 * - Level 4: Method/Function-level masking
 * 
 * Features:
 * - Smart context preservation
 * - Quality validation through transpilation
 * - Scalable processing of large datasets
 * - Multiple output formats (JSONL, CSV, Parquet)
 */

import fs from 'fs-extra';
import path from 'path';
import chalk from 'chalk';
import { Command } from 'commander';
import { createRequire } from 'module';
import crypto from 'crypto';
import { UniversalTranslator, UIRNodeTypes } from './universal-translator.js';

// Import existing bidirectional transformer for validation
const require = createRequire(import.meta.url);
const { RoundTripTester } = require('./bidirectional-transformer.js');

// ============================================
// Masking Strategy Implementation
// ============================================

/**
 * Base class for masking strategies
 */
export class MaskingStrategy {
  constructor(level, name) {
    this.level = level;
    this.name = name;
    this.maskCounter = 0;
  }

  mask(code, ast, metadata = {}) {
    throw new Error('mask method must be implemented by subclasses');
  }

  generateMaskToken() {
    return `<MASK_${this.level}_${++this.maskCounter}>`;
  }

  reset() {
    this.maskCounter = 0;
  }

  calculateMaskComplexity(originalCode, maskedCode) {
    const originalTokens = originalCode.split(/\s+/).length;
    const maskedTokens = maskedCode.split(/\s+/).length;
    const maskTokens = (maskedCode.match(/<MASK_\d+_\d+>/g) || []).length;
    
    return {
      originalTokens,
      maskedTokens,
      maskTokens,
      maskRatio: maskTokens / maskedTokens,
      complexityReduction: (originalTokens - maskedTokens + maskTokens) / originalTokens
    };
  }
}

/**
 * Level 1: Expression-level masking
 * Masks individual expressions like literals, variables, function calls
 */
export class ExpressionMaskingStrategy extends MaskingStrategy {
  constructor() {
    super(1, 'expression');
    this.expressionPatterns = [
      // Literals
      /'\([^']*\)'/g,                    // String literals
      /"\([^"]*\)"/g,                    // Double-quoted strings  
      /\b\d+(\.\d+)?\b/g,                // Numeric literals
      /\b(true|false|null|undefined|None)\b/g, // Boolean/null literals
      
      // Simple expressions
      /\b[a-zA-Z_][a-zA-Z0-9_]*\s*\(/g,  // Function calls
      /\b[a-zA-Z_][a-zA-Z0-9_]*\s*\[/g,  // Array access
      /\b[a-zA-Z_][a-zA-Z0-9_]*\./g,     // Member access
    ];
  }

  mask(code, ast, metadata = {}) {
    let maskedCode = code;
    const maskMap = new Map();
    this.reset();

    // Mask string literals
    maskedCode = maskedCode.replace(/(['"`])((?:(?!\1)[^\\]|\\.)*)(\1)/g, (match) => {
      const maskToken = this.generateMaskToken();
      maskMap.set(maskToken, {
        type: 'string_literal',
        original: match,
        context: this.extractContext(code, match)
      });
      return maskToken;
    });

    // Mask numeric literals
    maskedCode = maskedCode.replace(/\b\d+(\.\d+)?(e[+-]?\d+)?\b/gi, (match) => {
      const maskToken = this.generateMaskToken();
      maskMap.set(maskToken, {
        type: 'numeric_literal',
        original: match,
        context: this.extractContext(code, match)
      });
      return maskToken;
    });

    // Mask boolean literals
    maskedCode = maskedCode.replace(/\b(true|false|null|undefined|None|nil)\b/g, (match) => {
      const maskToken = this.generateMaskToken();
      maskMap.set(maskToken, {
        type: 'boolean_literal',
        original: match,
        context: this.extractContext(code, match)
      });
      return maskToken;
    });

    // Mask variable names (but preserve keywords and common patterns)
    maskedCode = maskedCode.replace(/\b[a-zA-Z_][a-zA-Z0-9_]{2,}\b/g, (match) => {
      if (this.isKeyword(match) || this.isCommonPattern(match)) {
        return match;
      }
      
      const maskToken = this.generateMaskToken();
      maskMap.set(maskToken, {
        type: 'identifier',
        original: match,
        context: this.extractContext(code, match)
      });
      return maskToken;
    });

    const complexity = this.calculateMaskComplexity(code, maskedCode);

    return {
      maskedCode,
      maskMap,
      metadata: {
        strategy: this.name,
        level: this.level,
        maskCount: maskMap.size,
        complexity,
        preservedContext: this.extractGlobalContext(code, maskedCode)
      }
    };
  }

  extractContext(code, match) {
    const index = code.indexOf(match);
    const start = Math.max(0, index - 20);
    const end = Math.min(code.length, index + match.length + 20);
    
    return {
      before: code.slice(start, index),
      after: code.slice(index + match.length, end),
      lineNumber: code.slice(0, index).split('\n').length
    };
  }

  extractGlobalContext(originalCode, maskedCode) {
    // Extract function signatures, class declarations, imports, etc.
    const context = {
      functions: [],
      classes: [],
      imports: [],
      comments: []
    };

    // Extract function signatures
    const functionPattern = /(?:function|def|func)\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*\([^)]*\)/g;
    let match;
    while ((match = functionPattern.exec(originalCode)) !== null) {
      context.functions.push(match[1]);
    }

    // Extract class declarations
    const classPattern = /(?:class|type)\s+([a-zA-Z_][a-zA-Z0-9_]*)/g;
    while ((match = classPattern.exec(originalCode)) !== null) {
      context.classes.push(match[1]);
    }

    // Extract imports/includes
    const importPattern = /(?:import|from|include|require)\s+[^;\n]*/g;
    while ((match = importPattern.exec(originalCode)) !== null) {
      context.imports.push(match[0]);
    }

    return context;
  }

  isKeyword(word) {
    const keywords = [
      'if', 'else', 'for', 'while', 'do', 'switch', 'case', 'break', 'continue',
      'function', 'return', 'var', 'let', 'const', 'class', 'extends', 'import',
      'export', 'from', 'default', 'async', 'await', 'try', 'catch', 'finally',
      'throw', 'new', 'this', 'super', 'static', 'public', 'private', 'protected',
      'def', 'lambda', 'yield', 'with', 'as', 'pass', 'and', 'or', 'not', 'in',
      'is', 'elif', 'except', 'raise', 'assert', 'global', 'nonlocal',
      'package', 'func', 'type', 'struct', 'interface', 'map', 'chan', 'select',
      'go', 'defer', 'range', 'fallthrough'
    ];
    
    return keywords.includes(word.toLowerCase());
  }

  isCommonPattern(word) {
    // Preserve common patterns that shouldn't be masked
    const patterns = [
      /^[a-z]+$/,           // Single lowercase word (often keywords)
      /^[A-Z]+$/,           // All caps (constants)
      /^[a-z]{1,3}$/,       // Very short variables (i, j, k, etc.)
      /^(get|set|is|has|can|should|will|did)[A-Z]/  // Getter/setter patterns
    ];
    
    return patterns.some(pattern => pattern.test(word));
  }
}

/**
 * Level 2: Statement-level masking
 * Masks complete statements while preserving control flow
 */
export class StatementMaskingStrategy extends MaskingStrategy {
  constructor() {
    super(2, 'statement');
  }

  mask(code, ast, metadata = {}) {
    const lines = code.split('\n');
    const maskedLines = [];
    const maskMap = new Map();
    this.reset();

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      const trimmedLine = line.trim();

      if (this.shouldMaskStatement(trimmedLine)) {
        const maskToken = this.generateMaskToken();
        const statementType = this.classifyStatement(trimmedLine);
        
        maskMap.set(maskToken, {
          type: 'statement',
          statementType,
          original: line,
          lineNumber: i + 1,
          context: this.extractStatementContext(lines, i)
        });
        
        // Preserve indentation
        const indent = line.match(/^\s*/)[0];
        maskedLines.push(indent + maskToken);
      } else {
        maskedLines.push(line);
      }
    }

    const maskedCode = maskedLines.join('\n');
    const complexity = this.calculateMaskComplexity(code, maskedCode);

    return {
      maskedCode,
      maskMap,
      metadata: {
        strategy: this.name,
        level: this.level,
        maskCount: maskMap.size,
        complexity,
        preservedStructure: this.analyzeControlFlow(lines)
      }
    };
  }

  shouldMaskStatement(line) {
    // Don't mask control flow statements, declarations, or structural elements
    const preservePatterns = [
      /^\s*(if|else|elif|elseif|while|for|do|switch|case|default)\b/,
      /^\s*(function|def|class|interface|struct|type)\b/,
      /^\s*(import|from|include|require|package)\b/,
      /^\s*(return|break|continue|pass)\b/,
      /^\s*[{}()[\]]/,                    // Structural braces/brackets
      /^\s*\/[\/\*]|^\s*\*|^\s*#/,      // Comments
      /^\s*$/,                           // Empty lines
      /^\s*[a-zA-Z_][a-zA-Z0-9_]*:/,    // Labels
    ];

    return !preservePatterns.some(pattern => pattern.test(line));
  }

  classifyStatement(line) {
    const patterns = [
      ['assignment', /^\s*[a-zA-Z_][a-zA-Z0-9_.*[\]]*\s*[=:]=\s*/],
      ['function_call', /^\s*[a-zA-Z_][a-zA-Z0-9_.]*\s*\(/],
      ['variable_declaration', /^\s*(var|let|const|DATA)\b/],
      ['expression', /^\s*[^=;{}]+[;]?\s*$/]
    ];

    for (const [type, pattern] of patterns) {
      if (pattern.test(line)) {
        return type;
      }
    }

    return 'unknown';
  }

  extractStatementContext(lines, lineIndex) {
    const contextSize = 2;
    const start = Math.max(0, lineIndex - contextSize);
    const end = Math.min(lines.length, lineIndex + contextSize + 1);

    return {
      before: lines.slice(start, lineIndex),
      after: lines.slice(lineIndex + 1, end),
      indentLevel: this.getIndentLevel(lines[lineIndex]),
      blockContext: this.getBlockContext(lines, lineIndex)
    };
  }

  getIndentLevel(line) {
    const match = line.match(/^(\s*)/);
    return match ? match[1].length : 0;
  }

  getBlockContext(lines, lineIndex) {
    // Find the nearest containing block (function, class, if, etc.)
    for (let i = lineIndex; i >= 0; i--) {
      const line = lines[i].trim();
      if (/^(function|def|class|if|for|while|switch)\b/.test(line)) {
        return {
          type: line.split(/\s+/)[0],
          line: i + 1,
          content: line
        };
      }
    }
    return { type: 'global', line: 1, content: '' };
  }

  analyzeControlFlow(lines) {
    const analysis = {
      totalLines: lines.length,
      codeLines: 0,
      commentLines: 0,
      blankLines: 0,
      controlStructures: [],
      indentationLevels: new Set()
    };

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      const trimmedLine = line.trim();
      
      if (!trimmedLine) {
        analysis.blankLines++;
      } else if (/^\s*\/[\/\*]|^\s*\*|^\s*#/.test(line)) {
        analysis.commentLines++;
      } else {
        analysis.codeLines++;
        analysis.indentationLevels.add(this.getIndentLevel(line));
        
        // Detect control structures
        const controlPattern = /^(if|else|elif|while|for|switch|case|try|catch|finally|def|function|class)\b/;
        if (controlPattern.test(trimmedLine)) {
          analysis.controlStructures.push({
            type: trimmedLine.split(/\s+/)[0],
            line: i + 1
          });
        }
      }
    }

    analysis.indentationLevels = Array.from(analysis.indentationLevels).sort((a, b) => a - b);
    return analysis;
  }
}

/**
 * Level 3: Block-level masking
 * Masks entire code blocks (functions bodies, loops, etc.)
 */
export class BlockMaskingStrategy extends MaskingStrategy {
  constructor() {
    super(3, 'block');
  }

  mask(code, ast, metadata = {}) {
    const lines = code.split('\n');
    const blocks = this.identifyBlocks(lines);
    const maskedLines = [...lines];
    const maskMap = new Map();
    this.reset();

    // Sort blocks by end line descending to avoid index shifting
    blocks.sort((a, b) => b.endLine - a.endLine);

    for (const block of blocks) {
      const maskToken = this.generateMaskToken();
      const originalBlock = lines.slice(block.startLine, block.endLine + 1).join('\n');
      
      maskMap.set(maskToken, {
        type: 'block',
        blockType: block.type,
        original: originalBlock,
        startLine: block.startLine + 1,
        endLine: block.endLine + 1,
        context: this.extractBlockContext(lines, block)
      });

      // Replace block with mask token, preserving the opening line
      const indent = lines[block.startLine].match(/^\s*/)[0];
      maskedLines.splice(
        block.startLine + 1, 
        block.endLine - block.startLine,
        indent + '  ' + maskToken
      );
    }

    const maskedCode = maskedLines.join('\n');
    const complexity = this.calculateMaskComplexity(code, maskedCode);

    return {
      maskedCode,
      maskMap,
      metadata: {
        strategy: this.name,
        level: this.level,
        maskCount: maskMap.size,
        complexity,
        blocksAnalysis: this.analyzeBlocks(blocks)
      }
    };
  }

  identifyBlocks(lines) {
    const blocks = [];
    const blockStack = [];

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      const trimmedLine = line.trim();
      
      // Opening block patterns
      if (this.isBlockStart(trimmedLine)) {
        const blockType = this.getBlockType(trimmedLine);
        blockStack.push({
          type: blockType,
          startLine: i,
          indentLevel: this.getIndentLevel(line)
        });
      }
      
      // Closing block patterns
      else if (this.isBlockEnd(trimmedLine, blockStack)) {
        const block = blockStack.pop();
        if (block) {
          blocks.push({
            ...block,
            endLine: i
          });
        }
      }
      
      // Auto-close blocks based on indentation (for Python-like languages)
      else if (blockStack.length > 0) {
        const currentIndent = this.getIndentLevel(line);
        const lastBlock = blockStack[blockStack.length - 1];
        
        if (trimmedLine && currentIndent <= lastBlock.indentLevel) {
          // Block ended due to indentation
          const block = blockStack.pop();
          blocks.push({
            ...block,
            endLine: i - 1
          });
        }
      }
    }

    // Close any remaining open blocks
    while (blockStack.length > 0) {
      const block = blockStack.pop();
      blocks.push({
        ...block,
        endLine: lines.length - 1
      });
    }

    return blocks.filter(block => block.endLine > block.startLine);
  }

  isBlockStart(line) {
    const patterns = [
      /^(function|def|class|interface|struct)\s+/,  // Function/class definitions
      /^(if|while|for|do|switch|try)\s*.*[{:]?\s*$/, // Control structures
      /^\s*{\s*$/,                                   // Opening brace
      /^.*:\s*$/ // Python-style colons
    ];

    return patterns.some(pattern => pattern.test(line));
  }

  isBlockEnd(line, blockStack) {
    if (blockStack.length === 0) return false;

    const patterns = [
      /^\s*}\s*$/,           // Closing brace
      /^(end|endif|endfor|endwhile|endfunction|endclass|endswitch|endtry)\b/, // End keywords
    ];

    return patterns.some(pattern => pattern.test(line));
  }

  getBlockType(line) {
    const typePatterns = [
      ['function', /^(function|def)\s+/],
      ['class', /^class\s+/],
      ['interface', /^interface\s+/],
      ['if', /^if\s+/],
      ['while', /^while\s+/],
      ['for', /^for\s+/],
      ['do', /^do\s*$/],
      ['switch', /^switch\s+/],
      ['try', /^try\s*$/],
      ['block', /^\s*{\s*$/]
    ];

    for (const [type, pattern] of typePatterns) {
      if (pattern.test(line)) {
        return type;
      }
    }

    return 'unknown';
  }

  getIndentLevel(line) {
    const match = line.match(/^(\s*)/);
    return match ? match[1].length : 0;
  }

  extractBlockContext(lines, block) {
    return {
      signature: lines[block.startLine].trim(),
      precedingLines: lines.slice(Math.max(0, block.startLine - 3), block.startLine),
      followingLines: lines.slice(block.endLine + 1, Math.min(lines.length, block.endLine + 4)),
      nestedBlocks: this.countNestedBlocks(lines, block)
    };
  }

  countNestedBlocks(lines, parentBlock) {
    const blockLines = lines.slice(parentBlock.startLine + 1, parentBlock.endLine);
    return this.identifyBlocks(blockLines).length;
  }

  analyzeBlocks(blocks) {
    const analysis = {
      totalBlocks: blocks.length,
      byType: {},
      averageSize: 0,
      maxSize: 0,
      nestingLevels: new Set()
    };

    let totalSize = 0;

    for (const block of blocks) {
      const type = block.type;
      analysis.byType[type] = (analysis.byType[type] || 0) + 1;
      
      const size = block.endLine - block.startLine + 1;
      totalSize += size;
      analysis.maxSize = Math.max(analysis.maxSize, size);
      analysis.nestingLevels.add(Math.floor(block.indentLevel / 2)); // Approximate nesting
    }

    analysis.averageSize = blocks.length > 0 ? totalSize / blocks.length : 0;
    analysis.nestingLevels = Array.from(analysis.nestingLevels).sort((a, b) => a - b);

    return analysis;
  }
}

/**
 * Level 4: Method/Function-level masking
 * Masks entire functions while preserving signatures and overall structure
 */
export class FunctionMaskingStrategy extends MaskingStrategy {
  constructor() {
    super(4, 'function');
  }

  mask(code, ast, metadata = {}) {
    const functions = this.identifyFunctions(code);
    let maskedCode = code;
    const maskMap = new Map();
    this.reset();

    // Sort functions by end position descending to avoid index shifting
    functions.sort((a, b) => b.end - a.end);

    for (const func of functions) {
      const maskToken = this.generateMaskToken();
      const originalFunction = code.slice(func.start, func.end);
      
      maskMap.set(maskToken, {
        type: 'function',
        functionType: func.type,
        name: func.name,
        signature: func.signature,
        original: originalFunction,
        context: this.extractFunctionContext(code, func)
      });

      // Replace function body with mask, keeping signature
      const replacement = func.signature + '\n  ' + maskToken + '\n' + func.closing;
      maskedCode = maskedCode.slice(0, func.start) + replacement + maskedCode.slice(func.end);
    }

    const complexity = this.calculateMaskComplexity(code, maskedCode);

    return {
      maskedCode,
      maskMap,
      metadata: {
        strategy: this.name,
        level: this.level,
        maskCount: maskMap.size,
        complexity,
        functionsAnalysis: this.analyzeFunctions(functions)
      }
    };
  }

  identifyFunctions(code) {
    const functions = [];
    
    // JavaScript/TypeScript function patterns
    const jsPatterns = [
      /function\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*\([^)]*\)\s*{/g,
      /const\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*=\s*\([^)]*\)\s*=>\s*{/g,
      /([a-zA-Z_][a-zA-Z0-9_]*)\s*:\s*\([^)]*\)\s*=>\s*{/g
    ];

    // Python function patterns
    const pyPatterns = [
      /def\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*\([^)]*\)\s*:/g
    ];

    // Go function patterns
    const goPatterns = [
      /func\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*\([^)]*\)(?:\s*[^{]*)?s*{/g
    ];

    const allPatterns = [...jsPatterns, ...pyPatterns, ...goPatterns];

    for (const pattern of allPatterns) {
      let match;
      while ((match = pattern.exec(code)) !== null) {
        const functionStart = match.index;
        const functionName = match[1] || 'anonymous';
        const signature = match[0];
        
        // Find function end
        const functionEnd = this.findFunctionEnd(code, functionStart, signature);
        
        if (functionEnd > functionStart) {
          functions.push({
            name: functionName,
            signature: signature,
            start: functionStart,
            end: functionEnd,
            type: this.detectFunctionType(signature),
            closing: this.getClosingPattern(signature)
          });
        }
      }
    }

    return functions;
  }

  findFunctionEnd(code, start, signature) {
    if (signature.includes('{')) {
      // Brace-delimited function
      return this.findMatchingBrace(code, start);
    } else if (signature.includes(':')) {
      // Indentation-based function (Python)
      return this.findIndentationEnd(code, start);
    }
    
    return start;
  }

  findMatchingBrace(code, start) {
    let braceCount = 0;
    let inString = false;
    let stringChar = '';
    
    for (let i = start; i < code.length; i++) {
      const char = code[i];
      
      if (!inString) {
        if (char === '"' || char === "'" || char === '`') {
          inString = true;
          stringChar = char;
        } else if (char === '{') {
          braceCount++;
        } else if (char === '}') {
          braceCount--;
          if (braceCount === 0) {
            return i + 1;
          }
        }
      } else {
        if (char === stringChar && code[i-1] !== '\\') {
          inString = false;
        }
      }
    }
    
    return code.length;
  }

  findIndentationEnd(code, start) {
    const lines = code.slice(start).split('\n');
    const firstLineIndent = lines[0].match(/^\s*/)[0].length;
    
    for (let i = 1; i < lines.length; i++) {
      const line = lines[i];
      const trimmedLine = line.trim();
      
      if (trimmedLine && line.match(/^\s*/)[0].length <= firstLineIndent) {
        // Found a line at the same or lower indentation level
        const endIndex = start + lines.slice(0, i).join('\n').length;
        return endIndex;
      }
    }
    
    return code.length;
  }

  detectFunctionType(signature) {
    if (signature.includes('function')) return 'function_declaration';
    if (signature.includes('=>')) return 'arrow_function';
    if (signature.includes('def')) return 'python_function';
    if (signature.includes('func')) return 'go_function';
    return 'unknown';
  }

  getClosingPattern(signature) {
    if (signature.includes('{')) return '}';
    if (signature.includes(':')) return '';
    return '';
  }

  extractFunctionContext(code, func) {
    const contextSize = 100;
    const beforeStart = Math.max(0, func.start - contextSize);
    const afterEnd = Math.min(code.length, func.end + contextSize);
    
    return {
      before: code.slice(beforeStart, func.start),
      after: code.slice(func.end, afterEnd),
      parameters: this.extractParameters(func.signature),
      docstring: this.extractDocstring(code, func.start)
    };
  }

  extractParameters(signature) {
    const match = signature.match(/\(([^)]*)\)/);
    if (!match) return [];
    
    const paramString = match[1].trim();
    if (!paramString) return [];
    
    return paramString.split(',').map(param => param.trim().split(/\s+/)[0]);
  }

  extractDocstring(code, functionStart) {
    // Look for docstring/comment before function
    const beforeFunction = code.slice(0, functionStart);
    const lines = beforeFunction.split('\n');
    
    const docLines = [];
    for (let i = lines.length - 1; i >= 0; i--) {
      const line = lines[i].trim();
      if (line.startsWith('/**') || line.startsWith('/*') || 
          line.startsWith('"""') || line.startsWith("'''") ||
          line.startsWith('//') || line.startsWith('#')) {
        docLines.unshift(line);
      } else if (line && docLines.length > 0) {
        break;
      } else if (line && docLines.length === 0) {
        break;
      }
    }
    
    return docLines.join('\n');
  }

  analyzeFunctions(functions) {
    const analysis = {
      totalFunctions: functions.length,
      byType: {},
      averageLength: 0,
      maxLength: 0,
      parameterStats: {
        total: 0,
        average: 0,
        max: 0
      }
    };

    let totalLength = 0;
    let totalParams = 0;

    for (const func of functions) {
      const type = func.type;
      analysis.byType[type] = (analysis.byType[type] || 0) + 1;
      
      const length = func.end - func.start;
      totalLength += length;
      analysis.maxLength = Math.max(analysis.maxLength, length);
      
      const params = this.extractParameters(func.signature);
      totalParams += params.length;
      analysis.parameterStats.max = Math.max(analysis.parameterStats.max, params.length);
    }

    analysis.averageLength = functions.length > 0 ? totalLength / functions.length : 0;
    analysis.parameterStats.total = totalParams;
    analysis.parameterStats.average = functions.length > 0 ? totalParams / functions.length : 0;

    return analysis;
  }
}

// ============================================
// Dataset Quality Validation
// ============================================

/**
 * Validates dataset quality through various methods including transpilation
 */
export class DatasetQualityValidator {
  constructor() {
    this.translator = new UniversalTranslator();
    this.roundTripTester = new RoundTripTester();
    this.validationResults = new Map();
  }

  async validateDatasetItem(item) {
    const validationId = this.generateValidationId(item);
    
    try {
      const results = {
        id: validationId,
        timestamp: new Date().toISOString(),
        masking: await this.validateMasking(item),
        translation: await this.validateTranslation(item),
        roundTrip: await this.validateRoundTrip(item),
        quality: null
      };

      results.quality = this.calculateQualityScore(results);
      
      this.validationResults.set(validationId, results);
      return results;

    } catch (error) {
      return {
        id: validationId,
        timestamp: new Date().toISOString(),
        error: error.message,
        quality: { overall: 0.0, valid: false }
      };
    }
  }

  async validateMasking(item) {
    const { originalCode, maskedCode, maskMap } = item;
    
    return {
      maskConsistency: this.checkMaskConsistency(maskedCode, maskMap),
      contextPreservation: this.checkContextPreservation(originalCode, maskedCode, maskMap),
      maskCoverage: this.checkMaskCoverage(maskMap),
      syntacticValidity: await this.checkSyntacticValidity(maskedCode, item.sourceLanguage)
    };
  }

  async validateTranslation(item) {
    if (!item.abapCode) {
      return { translated: false, error: 'No ABAP code provided' };
    }

    try {
      // Validate ABAP syntax
      const abapParser = new (require('./bidirectional-transformer.js')).ABAPParser();
      const parseResult = abapParser.parse(item.abapCode);
      
      return {
        translated: true,
        syntacticValidity: !!parseResult.ast,
        statementCount: parseResult.statements?.length || 0,
        complexity: this.measureABAPComplexity(item.abapCode)
      };

    } catch (error) {
      return {
        translated: false,
        error: error.message,
        syntacticValidity: false
      };
    }
  }

  async validateRoundTrip(item) {
    if (!item.abapCode) {
      return { roundTrip: false, error: 'No ABAP code for round-trip test' };
    }

    try {
      const testResult = await this.roundTripTester.testRoundTrip(item.abapCode);
      
      return {
        roundTrip: testResult.success,
        equivalence: testResult.success,
        error: testResult.error,
        transformedCode: testResult.transformed
      };

    } catch (error) {
      return {
        roundTrip: false,
        error: error.message,
        equivalence: false
      };
    }
  }

  checkMaskConsistency(maskedCode, maskMap) {
    const maskTokensInCode = (maskedCode.match(/<MASK_\d+_\d+>/g) || []);
    const maskTokensInMap = Array.from(maskMap.keys());
    
    return {
      allMasksPresent: maskTokensInCode.every(token => maskMap.has(token)),
      noOrphanMasks: maskTokensInMap.every(token => maskedCode.includes(token)),
      tokenCount: {
        inCode: maskTokensInCode.length,
        inMap: maskTokensInMap.length
      }
    };
  }

  checkContextPreservation(originalCode, maskedCode, maskMap) {
    // Check if important structural elements are preserved
    const originalStructure = this.analyzeCodeStructure(originalCode);
    const maskedStructure = this.analyzeCodeStructure(maskedCode);
    
    return {
      controlFlowPreserved: this.compareControlFlow(originalStructure, maskedStructure),
      functionSignaturesPreserved: this.compareFunctionSignatures(originalStructure, maskedStructure),
      importStatementsPreserved: this.compareImports(originalStructure, maskedStructure),
      indentationPreserved: this.compareIndentation(originalCode, maskedCode)
    };
  }

  checkMaskCoverage(maskMap) {
    const coverage = {
      totalMasks: maskMap.size,
      byType: {},
      averageOriginalLength: 0,
      maskDensity: 0
    };

    let totalLength = 0;
    
    for (const [token, info] of maskMap.entries()) {
      const type = info.type;
      coverage.byType[type] = (coverage.byType[type] || 0) + 1;
      totalLength += info.original.length;
    }

    coverage.averageOriginalLength = maskMap.size > 0 ? totalLength / maskMap.size : 0;
    
    return coverage;
  }

  async checkSyntacticValidity(code, language) {
    try {
      const parser = this.translator.getParser(language);
      const result = parser.parse(code);
      
      return {
        valid: true,
        nodeCount: this.countNodes(result),
        hasErrors: false
      };

    } catch (error) {
      return {
        valid: false,
        error: error.message,
        hasErrors: true
      };
    }
  }

  analyzeCodeStructure(code) {
    return {
      controlFlow: this.extractControlFlow(code),
      functionSignatures: this.extractFunctionSignatures(code),
      imports: this.extractImports(code),
      indentationPattern: this.analyzeIndentation(code)
    };
  }

  extractControlFlow(code) {
    const controlKeywords = ['if', 'else', 'for', 'while', 'switch', 'case', 'try', 'catch'];
    const pattern = new RegExp(`\\b(${controlKeywords.join('|')})\\b`, 'g');
    return (code.match(pattern) || []);
  }

  extractFunctionSignatures(code) {
    const signatures = [];
    const patterns = [
      /function\s+[a-zA-Z_][a-zA-Z0-9_]*\s*\([^)]*\)/g,
      /def\s+[a-zA-Z_][a-zA-Z0-9_]*\s*\([^)]*\)/g,
      /func\s+[a-zA-Z_][a-zA-Z0-9_]*\s*\([^)]*\)/g
    ];

    for (const pattern of patterns) {
      let match;
      while ((match = pattern.exec(code)) !== null) {
        signatures.push(match[0]);
      }
    }

    return signatures;
  }

  extractImports(code) {
    const imports = [];
    const patterns = [
      /import\s+[^;\n]*/g,
      /from\s+[^;\n]*/g,
      /require\s*\([^)]*\)/g
    ];

    for (const pattern of patterns) {
      let match;
      while ((match = pattern.exec(code)) !== null) {
        imports.push(match[0]);
      }
    }

    return imports;
  }

  analyzeIndentation(code) {
    const lines = code.split('\n');
    const indentLevels = [];
    
    for (const line of lines) {
      if (line.trim()) {
        const match = line.match(/^(\s*)/);
        indentLevels.push(match ? match[1].length : 0);
      }
    }

    return {
      levels: [...new Set(indentLevels)].sort((a, b) => a - b),
      maxIndent: Math.max(...indentLevels),
      avgIndent: indentLevels.length > 0 ? 
        indentLevels.reduce((a, b) => a + b, 0) / indentLevels.length : 0
    };
  }

  compareControlFlow(original, masked) {
    return original.controlFlow.length === masked.controlFlow.length &&
           original.controlFlow.every((item, index) => item === masked.controlFlow[index]);
  }

  compareFunctionSignatures(original, masked) {
    return original.functionSignatures.length === masked.functionSignatures.length;
  }

  compareImports(original, masked) {
    return original.imports.length === masked.imports.length;
  }

  compareIndentation(originalCode, maskedCode) {
    const originalIndent = this.analyzeIndentation(originalCode);
    const maskedIndent = this.analyzeIndentation(maskedCode);
    
    return {
      levelPreservation: originalIndent.levels.length === maskedIndent.levels.length,
      maxIndentPreserved: originalIndent.maxIndent === maskedIndent.maxIndent,
      avgIndentSimilar: Math.abs(originalIndent.avgIndent - maskedIndent.avgIndent) < 2
    };
  }

  measureABAPComplexity(abapCode) {
    const lines = abapCode.split('\n').filter(line => line.trim());
    const statements = lines.length;
    const controlStructures = (abapCode.match(/\b(IF|WHILE|LOOP|CASE|DO)\b/gi) || []).length;
    const dataDeclarations = (abapCode.match(/\bDATA\b/gi) || []).length;
    
    return {
      lines: statements,
      controlStructures,
      dataDeclarations,
      cyclomaticComplexity: controlStructures + 1
    };
  }

  countNodes(uirTree) {
    let count = 1;
    for (const child of uirTree.children || []) {
      count += this.countNodes(child);
    }
    return count;
  }

  calculateQualityScore(results) {
    const scores = {
      masking: 0,
      translation: 0,
      roundTrip: 0
    };

    // Masking score
    if (results.masking) {
      let maskingScore = 0;
      if (results.masking.maskConsistency?.allMasksPresent) maskingScore += 25;
      if (results.masking.contextPreservation?.controlFlowPreserved) maskingScore += 25;
      if (results.masking.syntacticValidity?.valid) maskingScore += 25;
      if (results.masking.maskCoverage?.totalMasks > 0) maskingScore += 25;
      scores.masking = maskingScore;
    }

    // Translation score
    if (results.translation) {
      let translationScore = 0;
      if (results.translation.translated) translationScore += 50;
      if (results.translation.syntacticValidity) translationScore += 50;
      scores.translation = translationScore;
    }

    // Round-trip score
    if (results.roundTrip) {
      scores.roundTrip = results.roundTrip.roundTrip ? 100 : 0;
    }

    const overall = (scores.masking + scores.translation + scores.roundTrip) / 3;
    
    return {
      overall,
      masking: scores.masking,
      translation: scores.translation,
      roundTrip: scores.roundTrip,
      valid: overall >= 60 // Threshold for valid training example
    };
  }

  generateValidationId(item) {
    const content = JSON.stringify({
      code: item.originalCode?.slice(0, 100),
      strategy: item.metadata?.strategy,
      level: item.metadata?.level
    });
    
    return crypto.createHash('md5').update(content).digest('hex').slice(0, 8);
  }

  getValidationSummary() {
    const results = Array.from(this.validationResults.values());
    
    const summary = {
      total: results.length,
      valid: results.filter(r => r.quality?.valid).length,
      invalid: results.filter(r => !r.quality?.valid).length,
      averageQuality: 0,
      byStrategy: {},
      commonErrors: []
    };

    if (results.length > 0) {
      summary.averageQuality = results.reduce((sum, r) => 
        sum + (r.quality?.overall || 0), 0) / results.length;
    }

    // Group by strategy
    for (const result of results) {
      if (result.metadata?.strategy) {
        const strategy = result.metadata.strategy;
        if (!summary.byStrategy[strategy]) {
          summary.byStrategy[strategy] = { total: 0, valid: 0 };
        }
        summary.byStrategy[strategy].total++;
        if (result.quality?.valid) {
          summary.byStrategy[strategy].valid++;
        }
      }
    }

    return summary;
  }
}

// ============================================
// Main Dataset Generator
// ============================================

/**
 * Main dataset generator that coordinates masking strategies and quality validation
 */
export class UniversalDatasetGenerator {
  constructor(options = {}) {
    this.options = {
      outputFormat: 'jsonl',
      validateQuality: true,
      parallelProcessing: true,
      maxConcurrency: 5,
      qualityThreshold: 60,
      ...options
    };

    this.translator = new UniversalTranslator();
    this.validator = new DatasetQualityValidator();
    this.strategies = this.initializeStrategies();
    this.stats = {
      processed: 0,
      generated: 0,
      valid: 0,
      invalid: 0,
      errors: 0
    };
  }

  initializeStrategies() {
    return [
      new ExpressionMaskingStrategy(),
      new StatementMaskingStrategy(),
      new BlockMaskingStrategy(),
      new FunctionMaskingStrategy()
    ];
  }

  async generateDataset(inputSources, outputPath, options = {}) {
    const config = { ...this.options, ...options };
    
    console.log(chalk.blue('Starting dataset generation...'));
    console.log(chalk.gray(`Input sources: ${inputSources.length}`));
    console.log(chalk.gray(`Output: ${outputPath}`));
    console.log(chalk.gray(`Strategies: ${this.strategies.length}`));

    try {
      // Process all input sources
      const allItems = [];
      
      for (const source of inputSources) {
        const items = await this.processInputSource(source, config);
        allItems.push(...items);
        
        console.log(chalk.green(`✓ Processed ${source.path || source.code?.slice(0, 50)}... (${items.length} items)`));
      }

      console.log(chalk.blue(`Generated ${allItems.length} total items`));

      // Validate quality if enabled
      if (config.validateQuality) {
        console.log(chalk.yellow('Validating dataset quality...'));
        await this.validateDataset(allItems);
      }

      // Filter by quality if threshold is set
      const finalItems = config.qualityThreshold > 0 ? 
        allItems.filter(item => !item.quality || item.quality.overall >= config.qualityThreshold) :
        allItems;

      console.log(chalk.blue(`Final dataset: ${finalItems.length} items (${allItems.length - finalItems.length} filtered out)`));

      // Write output
      await this.writeDataset(finalItems, outputPath, config.outputFormat);
      
      // Generate summary
      const summary = this.generateSummary(allItems, finalItems);
      await this.writeSummary(summary, outputPath);

      console.log(chalk.green('✓ Dataset generation completed successfully'));
      
      return {
        success: true,
        totalItems: allItems.length,
        validItems: finalItems.length,
        outputPath,
        summary
      };

    } catch (error) {
      console.error(chalk.red('✗ Dataset generation failed:'), error.message);
      throw error;
    }
  }

  async processInputSource(source, config) {
    if (source.code) {
      // Direct code input
      return await this.processCode(source.code, source.language, source.metadata || {});
    } else if (source.path) {
      // File input
      return await this.processFile(source.path, config);
    } else if (source.directory) {
      // Directory input
      return await this.processDirectory(source.directory, config);
    } else {
      throw new Error('Invalid input source format');
    }
  }

  async processCode(code, language, metadata = {}) {
    const items = [];
    
    try {
      // Apply each masking strategy
      for (const strategy of this.strategies) {
        const maskResult = strategy.mask(code, null, metadata);
        
        // Translate to ABAP
        const translationResult = await this.translator.translateCode(
          maskResult.maskedCode, 
          language
        );

        // Create dataset item
        const item = {
          id: this.generateItemId(code, strategy.name),
          sourceLanguage: language,
          originalCode: code,
          maskedCode: maskResult.maskedCode,
          maskMap: Object.fromEntries(maskResult.maskMap),
          abapCode: translationResult.success ? translationResult.abapCode : null,
          translationSuccess: translationResult.success,
          translationError: translationResult.success ? null : translationResult.error,
          metadata: {
            ...metadata,
            ...maskResult.metadata,
            translationMetadata: translationResult.metadata
          },
          quality: null, // Will be filled during validation
          timestamp: new Date().toISOString()
        };

        items.push(item);
        this.stats.generated++;
      }

      this.stats.processed++;
      
    } catch (error) {
      console.error(chalk.red(`Error processing code: ${error.message}`));
      this.stats.errors++;
    }

    return items;
  }

  async processFile(filePath, config) {
    try {
      const code = await fs.readFile(filePath, 'utf8');
      const language = this.detectLanguage(filePath);
      const metadata = {
        sourceFile: filePath,
        fileSize: code.length,
        lineCount: code.split('\n').length
      };

      return await this.processCode(code, language, metadata);

    } catch (error) {
      console.error(chalk.red(`Error processing file ${filePath}: ${error.message}`));
      this.stats.errors++;
      return [];
    }
  }

  async processDirectory(dirPath, config) {
    const allItems = [];
    
    try {
      const files = await this.findSourceFiles(dirPath, config);
      
      if (config.parallelProcessing) {
        const pLimit = (await import('p-limit')).default;
        const limit = pLimit(config.maxConcurrency);
        
        const promises = files.map(file => 
          limit(() => this.processFile(file, config))
        );
        
        const results = await Promise.all(promises);
        
        for (const items of results) {
          allItems.push(...items);
        }
      } else {
        for (const file of files) {
          const items = await this.processFile(file, config);
          allItems.push(...items);
        }
      }

    } catch (error) {
      console.error(chalk.red(`Error processing directory ${dirPath}: ${error.message}`));
      this.stats.errors++;
    }

    return allItems;
  }

  async findSourceFiles(dirPath, config) {
    const extensions = ['.js', '.ts', '.py', '.go', '.java', '.c', '.cpp', '.rb', '.rs'];
    const files = [];
    
    const glob = (await import('glob')).glob;
    
    for (const ext of extensions) {
      const pattern = path.join(dirPath, '**', `*${ext}`);
      const matches = await glob(pattern, {
        ignore: ['**/node_modules/**', '**/.git/**', '**/vendor/**', '**/target/**']
      });
      files.push(...matches);
    }
    
    return files;
  }

  detectLanguage(filePath) {
    const ext = path.extname(filePath).toLowerCase();
    const langMap = {
      '.js': 'javascript',
      '.ts': 'typescript',
      '.py': 'python',
      '.go': 'go',
      '.java': 'java',
      '.c': 'c',
      '.cpp': 'cpp',
      '.cc': 'cpp',
      '.cxx': 'cpp',
      '.rb': 'ruby',
      '.rs': 'rust'
    };
    
    return langMap[ext] || 'unknown';
  }

  async validateDataset(items) {
    console.log(chalk.yellow(`Validating ${items.length} dataset items...`));
    
    let validatedCount = 0;
    
    for (const item of items) {
      try {
        const validation = await this.validator.validateDatasetItem(item);
        item.quality = validation.quality;
        
        if (validation.quality.valid) {
          this.stats.valid++;
        } else {
          this.stats.invalid++;
        }
        
        validatedCount++;
        
        if (validatedCount % 100 === 0) {
          console.log(chalk.gray(`Validated ${validatedCount}/${items.length} items...`));
        }
        
      } catch (error) {
        console.error(chalk.red(`Validation error for item ${item.id}: ${error.message}`));
        item.quality = { overall: 0, valid: false, error: error.message };
        this.stats.invalid++;
      }
    }
    
    console.log(chalk.green(`✓ Validation completed: ${this.stats.valid} valid, ${this.stats.invalid} invalid`));
  }

  async writeDataset(items, outputPath, format) {
    await fs.ensureDir(path.dirname(outputPath));
    
    switch (format) {
      case 'jsonl':
        await this.writeJSONL(items, outputPath);
        break;
      case 'json':
        await this.writeJSON(items, outputPath);
        break;
      case 'csv':
        await this.writeCSV(items, outputPath);
        break;
      default:
        throw new Error(`Unsupported output format: ${format}`);
    }
    
    console.log(chalk.green(`✓ Dataset written to ${outputPath} (${format.toUpperCase()} format)`));
  }

  async writeJSONL(items, outputPath) {
    const lines = items.map(item => JSON.stringify(item));
    await fs.writeFile(outputPath, lines.join('\n'), 'utf8');
  }

  async writeJSON(items, outputPath) {
    await fs.writeFile(outputPath, JSON.stringify(items, null, 2), 'utf8');
  }

  async writeCSV(items, outputPath) {
    if (items.length === 0) return;
    
    // Extract CSV headers from first item
    const headers = [
      'id', 'sourceLanguage', 'maskingStrategy', 'maskingLevel',
      'originalCodeLength', 'maskedCodeLength', 'abapCodeLength',
      'translationSuccess', 'qualityScore', 'valid'
    ];
    
    const rows = items.map(item => [
      item.id,
      item.sourceLanguage,
      item.metadata?.strategy || '',
      item.metadata?.level || '',
      item.originalCode?.length || 0,
      item.maskedCode?.length || 0,
      item.abapCode?.length || 0,
      item.translationSuccess ? 'true' : 'false',
      item.quality?.overall || 0,
      item.quality?.valid ? 'true' : 'false'
    ]);
    
    const csvContent = [headers, ...rows]
      .map(row => row.map(cell => `"${cell}"`).join(','))
      .join('\n');
    
    await fs.writeFile(outputPath, csvContent, 'utf8');
  }

  async writeSummary(summary, outputPath) {
    const summaryPath = outputPath.replace(/\.[^.]+$/, '_summary.json');
    await fs.writeFile(summaryPath, JSON.stringify(summary, null, 2), 'utf8');
    console.log(chalk.blue(`Summary written to ${summaryPath}`));
  }

  generateSummary(allItems, validItems) {
    const summary = {
      generation: {
        timestamp: new Date().toISOString(),
        totalItems: allItems.length,
        validItems: validItems.length,
        filterRate: allItems.length > 0 ? (allItems.length - validItems.length) / allItems.length : 0,
        stats: { ...this.stats }
      },
      byLanguage: {},
      byStrategy: {},
      qualityDistribution: {
        excellent: 0, // 90-100
        good: 0,      // 70-89
        fair: 0,      // 50-69
        poor: 0       // 0-49
      },
      validation: this.validator.getValidationSummary()
    };

    // Analyze by language and strategy
    for (const item of allItems) {
      const lang = item.sourceLanguage;
      const strategy = item.metadata?.strategy;
      
      // By language
      if (!summary.byLanguage[lang]) {
        summary.byLanguage[lang] = { total: 0, valid: 0, avgQuality: 0 };
      }
      summary.byLanguage[lang].total++;
      if (item.quality?.valid) {
        summary.byLanguage[lang].valid++;
      }
      
      // By strategy
      if (strategy) {
        if (!summary.byStrategy[strategy]) {
          summary.byStrategy[strategy] = { total: 0, valid: 0, avgQuality: 0 };
        }
        summary.byStrategy[strategy].total++;
        if (item.quality?.valid) {
          summary.byStrategy[strategy].valid++;
        }
      }
      
      // Quality distribution
      const quality = item.quality?.overall || 0;
      if (quality >= 90) summary.qualityDistribution.excellent++;
      else if (quality >= 70) summary.qualityDistribution.good++;
      else if (quality >= 50) summary.qualityDistribution.fair++;
      else summary.qualityDistribution.poor++;
    }

    return summary;
  }

  generateItemId(code, strategyName) {
    const content = code.slice(0, 100) + strategyName;
    return crypto.createHash('md5').update(content).digest('hex').slice(0, 12);
  }

  getStatistics() {
    return {
      ...this.stats,
      validationSummary: this.validator.getValidationSummary()
    };
  }
}

// ============================================
// CLI Interface
// ============================================

function createCLI() {
  const program = new Command();
  
  program
    .name('dataset-generator')
    .description('Universal Code-to-ABAP Dataset Generator with Article 020 Masking Strategies')
    .version('1.0.0');

  program
    .command('generate')
    .description('Generate training dataset from source code')
    .option('-i, --input <path>', 'Input file or directory')
    .option('-o, --output <path>', 'Output file path', 'dataset.jsonl')
    .option('-f, --format <format>', 'Output format (jsonl, json, csv)', 'jsonl')
    .option('--no-validate', 'Skip quality validation')
    .option('--quality-threshold <n>', 'Quality threshold (0-100)', '60')
    .option('--parallel', 'Enable parallel processing', true)
    .option('--max-concurrency <n>', 'Maximum concurrent processes', '5')
    .option('-l, --language <lang>', 'Override language detection')
    .action(async (options) => {
      const generator = new UniversalDatasetGenerator({
        outputFormat: options.format,
        validateQuality: options.validate !== false,
        qualityThreshold: parseInt(options.qualityThreshold),
        parallelProcessing: options.parallel,
        maxConcurrency: parseInt(options.maxConcurrency)
      });

      try {
        if (!options.input) {
          console.error(chalk.red('Input path is required'));
          process.exit(1);
        }

        const inputSources = [{
          path: options.input,
          language: options.language
        }];

        const result = await generator.generateDataset(
          inputSources, 
          options.output
        );

        console.log(chalk.green('\n✓ Dataset generation completed!'));
        console.log(`Generated: ${result.totalItems} items`);
        console.log(`Valid: ${result.validItems} items`);
        console.log(`Output: ${result.outputPath}`);

      } catch (error) {
        console.error(chalk.red('Error:'), error.message);
        process.exit(1);
      }
    });

  program
    .command('validate')
    .description('Validate an existing dataset')
    .option('-i, --input <path>', 'Input dataset file')
    .option('-o, --output <path>', 'Validation report output')
    .action(async (options) => {
      const validator = new DatasetQualityValidator();
      
      try {
        console.log(chalk.blue('Loading dataset...'));
        const dataset = JSON.parse(await fs.readFile(options.input, 'utf8'));
        
        console.log(chalk.blue(`Validating ${dataset.length} items...`));
        
        for (const item of dataset) {
          await validator.validateDatasetItem(item);
        }
        
        const summary = validator.getValidationSummary();
        
        console.log(chalk.green('\n✓ Validation completed!'));
        console.log(`Valid: ${summary.valid}/${summary.total}`);
        console.log(`Average quality: ${summary.averageQuality.toFixed(2)}`);
        
        if (options.output) {
          await fs.writeFile(options.output, JSON.stringify(summary, null, 2));
          console.log(`Report saved to: ${options.output}`);
        }

      } catch (error) {
        console.error(chalk.red('Error:'), error.message);
        process.exit(1);
      }
    });

  program
    .command('demo')
    .description('Generate a small demo dataset')
    .action(async () => {
      const generator = new UniversalDatasetGenerator();
      
      const demoSources = [
        {
          code: `
function fibonacci(n) {
  if (n <= 1) return n;
  return fibonacci(n - 1) + fibonacci(n - 2);
}

const result = fibonacci(10);
console.log(result);
          `,
          language: 'javascript',
          metadata: { source: 'demo' }
        },
        {
          code: `
def quicksort(arr):
    if len(arr) <= 1:
        return arr
    pivot = arr[len(arr) // 2]
    left = [x for x in arr if x < pivot]
    middle = [x for x in arr if x == pivot]
    right = [x for x in arr if x > pivot]
    return quicksort(left) + middle + quicksort(right)

numbers = [3, 1, 4, 1, 5, 9, 2, 6, 5]
sorted_numbers = quicksort(numbers)
print(sorted_numbers)
          `,
          language: 'python',
          metadata: { source: 'demo' }
        }
      ];

      try {
        const result = await generator.generateDataset(
          demoSources,
          'demo_dataset.jsonl'
        );

        console.log(chalk.green('\n✓ Demo dataset generated!'));
        console.log(`Generated: ${result.totalItems} items`);
        console.log(`Valid: ${result.validItems} items`);
        console.log('Check demo_dataset.jsonl and demo_dataset_summary.json');

      } catch (error) {
        console.error(chalk.red('Demo error:'), error.message);
        process.exit(1);
      }
    });

  return program;
}

// ============================================
// Main Entry Point
// ============================================

async function main() {
  console.log(chalk.blue.bold('Universal Code-to-ABAP Dataset Generator'));
  console.log(chalk.gray('Implementing Article 020 Masking Strategies'));
  console.log();

  if (process.argv.length <= 2) {
    const program = createCLI();
    program.help();
  } else {
    const program = createCLI();
    await program.parseAsync(process.argv);
  }
}

// Export for use as module
export default UniversalDatasetGenerator;

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch(error => {
    console.error(chalk.red('Fatal error:'), error.message);
    console.error(error.stack);
    process.exit(1);
  });
}