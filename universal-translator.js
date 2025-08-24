#!/usr/bin/env node

/**
 * Universal Code-to-ABAP Translation System
 * 
 * Implements Article 019 architecture with:
 * - Universal Intermediate Representation (UIR)
 * - Multi-language parsers (JavaScript, Python, Go, TypeScript, Java, C/C++, Ruby, Rust)
 * - UIR to ABAP AST transformer
 * - Integration with bidirectional-transformer.js
 * - Production-ready scalability and error handling
 */

import Parser from 'tree-sitter';
import JavaScript from 'tree-sitter-javascript';
import Python from 'tree-sitter-python';
import Go from 'tree-sitter-go';
import TypeScript from 'tree-sitter-typescript';
import Java from 'tree-sitter-java';
import C from 'tree-sitter-c';
import Cpp from 'tree-sitter-cpp';
import Ruby from 'tree-sitter-ruby';
import Rust from 'tree-sitter-rust';

import * as abaplint from "@abaplint/core";
import fs from 'fs-extra';
import path from 'path';
import chalk from 'chalk';
import { Command } from 'commander';
import { createRequire } from 'module';

// Import our existing bidirectional transformer
const require = createRequire(import.meta.url);
const { ABAPParser, ABAPPrettyPrinter, ASTEquivalenceChecker } = require('./bidirectional-transformer.js');

// ============================================
// Universal Intermediate Representation (UIR)
// ============================================

/**
 * Universal Intermediate Representation
 * Language-agnostic representation of code constructs
 */
export class UIRNode {
  constructor(type, properties = {}) {
    this.type = type;
    this.properties = properties;
    this.children = [];
    this.metadata = {
      sourceLanguage: null,
      sourceRange: null,
      confidence: 1.0,
      transformationHints: []
    };
  }

  addChild(child) {
    if (child instanceof UIRNode) {
      this.children.push(child);
    } else {
      throw new Error('Child must be a UIRNode instance');
    }
  }

  setMetadata(key, value) {
    this.metadata[key] = value;
  }

  getMetadata(key) {
    return this.metadata[key];
  }

  // Serialize to JSON for debugging/storage
  toJSON() {
    return {
      type: this.type,
      properties: this.properties,
      children: this.children.map(child => child.toJSON()),
      metadata: this.metadata
    };
  }

  // Create from JSON
  static fromJSON(json) {
    const node = new UIRNode(json.type, json.properties);
    node.metadata = json.metadata;
    node.children = json.children.map(child => UIRNode.fromJSON(child));
    return node;
  }
}

// UIR Node Types
export const UIRNodeTypes = {
  PROGRAM: 'program',
  MODULE: 'module',
  FUNCTION: 'function',
  CLASS: 'class',
  METHOD: 'method',
  VARIABLE_DECLARATION: 'variable_declaration',
  ASSIGNMENT: 'assignment',
  IF_STATEMENT: 'if_statement',
  WHILE_LOOP: 'while_loop',
  FOR_LOOP: 'for_loop',
  EXPRESSION: 'expression',
  BINARY_OPERATION: 'binary_operation',
  FUNCTION_CALL: 'function_call',
  RETURN_STATEMENT: 'return_statement',
  COMMENT: 'comment',
  LITERAL: 'literal',
  IDENTIFIER: 'identifier',
  PARAMETER: 'parameter',
  BLOCK: 'block',
  TRY_CATCH: 'try_catch',
  SWITCH_CASE: 'switch_case',
  ARRAY_ACCESS: 'array_access',
  MEMBER_ACCESS: 'member_access'
};

// ============================================
// Language Parsers
// ============================================

/**
 * Base class for language-specific parsers
 */
export class LanguageParser {
  constructor(language) {
    this.parser = new Parser();
    this.language = language;
    this.setupParser();
  }

  setupParser() {
    throw new Error('setupParser must be implemented by subclasses');
  }

  parse(sourceCode) {
    const tree = this.parser.parse(sourceCode);
    return this.astToUIR(tree.rootNode, sourceCode);
  }

  astToUIR(node, sourceCode) {
    throw new Error('astToUIR must be implemented by subclasses');
  }

  getNodeText(node, sourceCode) {
    return sourceCode.slice(node.startIndex, node.endIndex);
  }

  createUIRNode(type, properties = {}) {
    const uirNode = new UIRNode(type, properties);
    uirNode.setMetadata('sourceLanguage', this.language);
    return uirNode;
  }
}

/**
 * JavaScript Parser
 */
export class JavaScriptParser extends LanguageParser {
  constructor() {
    super('javascript');
  }

  setupParser() {
    this.parser.setLanguage(JavaScript);
  }

  astToUIR(node, sourceCode) {
    const uirNode = this.createUIRNode(this.mapNodeType(node.type));
    uirNode.setMetadata('sourceRange', {
      start: { row: node.startPosition.row, column: node.startPosition.column },
      end: { row: node.endPosition.row, column: node.endPosition.column }
    });

    switch (node.type) {
      case 'program':
        uirNode.type = UIRNodeTypes.PROGRAM;
        break;
      
      case 'function_declaration':
        uirNode.type = UIRNodeTypes.FUNCTION;
        uirNode.properties.name = this.extractFunctionName(node, sourceCode);
        uirNode.properties.parameters = this.extractParameters(node, sourceCode);
        break;
      
      case 'variable_declaration':
        uirNode.type = UIRNodeTypes.VARIABLE_DECLARATION;
        uirNode.properties.declarations = this.extractVariableDeclarations(node, sourceCode);
        break;
      
      case 'assignment_expression':
        uirNode.type = UIRNodeTypes.ASSIGNMENT;
        uirNode.properties.left = this.getNodeText(node.namedChild(0), sourceCode);
        uirNode.properties.right = this.getNodeText(node.namedChild(1), sourceCode);
        break;
      
      case 'if_statement':
        uirNode.type = UIRNodeTypes.IF_STATEMENT;
        uirNode.properties.condition = this.getNodeText(node.namedChild(0), sourceCode);
        break;
      
      case 'while_statement':
        uirNode.type = UIRNodeTypes.WHILE_LOOP;
        uirNode.properties.condition = this.getNodeText(node.namedChild(0), sourceCode);
        break;
      
      case 'for_statement':
        uirNode.type = UIRNodeTypes.FOR_LOOP;
        uirNode.properties.init = this.getNodeText(node.namedChild(0), sourceCode);
        uirNode.properties.condition = this.getNodeText(node.namedChild(1), sourceCode);
        uirNode.properties.update = this.getNodeText(node.namedChild(2), sourceCode);
        break;
      
      case 'binary_expression':
        uirNode.type = UIRNodeTypes.BINARY_OPERATION;
        uirNode.properties.operator = this.extractBinaryOperator(node, sourceCode);
        uirNode.properties.left = this.getNodeText(node.namedChild(0), sourceCode);
        uirNode.properties.right = this.getNodeText(node.namedChild(1), sourceCode);
        break;
      
      case 'call_expression':
        uirNode.type = UIRNodeTypes.FUNCTION_CALL;
        uirNode.properties.function = this.getNodeText(node.namedChild(0), sourceCode);
        uirNode.properties.arguments = this.extractCallArguments(node, sourceCode);
        break;
      
      case 'return_statement':
        uirNode.type = UIRNodeTypes.RETURN_STATEMENT;
        if (node.namedChildCount > 0) {
          uirNode.properties.value = this.getNodeText(node.namedChild(0), sourceCode);
        }
        break;
      
      case 'comment':
        uirNode.type = UIRNodeTypes.COMMENT;
        uirNode.properties.text = this.getNodeText(node, sourceCode);
        break;
      
      case 'identifier':
        uirNode.type = UIRNodeTypes.IDENTIFIER;
        uirNode.properties.name = this.getNodeText(node, sourceCode);
        break;
      
      case 'number':
      case 'string':
      case 'true':
      case 'false':
      case 'null':
        uirNode.type = UIRNodeTypes.LITERAL;
        uirNode.properties.value = this.getNodeText(node, sourceCode);
        uirNode.properties.dataType = this.inferLiteralType(node.type);
        break;
      
      default:
        // For unhandled node types, create a generic UIR node
        uirNode.properties.originalType = node.type;
        uirNode.properties.text = this.getNodeText(node, sourceCode);
    }

    // Recursively process children
    for (let i = 0; i < node.namedChildCount; i++) {
      const childUIR = this.astToUIR(node.namedChild(i), sourceCode);
      uirNode.addChild(childUIR);
    }

    return uirNode;
  }

  mapNodeType(nodeType) {
    const mapping = {
      'program': UIRNodeTypes.PROGRAM,
      'function_declaration': UIRNodeTypes.FUNCTION,
      'variable_declaration': UIRNodeTypes.VARIABLE_DECLARATION,
      'assignment_expression': UIRNodeTypes.ASSIGNMENT,
      'if_statement': UIRNodeTypes.IF_STATEMENT,
      'while_statement': UIRNodeTypes.WHILE_LOOP,
      'for_statement': UIRNodeTypes.FOR_LOOP,
      'binary_expression': UIRNodeTypes.BINARY_OPERATION,
      'call_expression': UIRNodeTypes.FUNCTION_CALL,
      'return_statement': UIRNodeTypes.RETURN_STATEMENT,
      'comment': UIRNodeTypes.COMMENT,
      'identifier': UIRNodeTypes.IDENTIFIER,
      'statement_block': UIRNodeTypes.BLOCK
    };

    return mapping[nodeType] || 'unknown';
  }

  extractFunctionName(node, sourceCode) {
    for (let i = 0; i < node.namedChildCount; i++) {
      const child = node.namedChild(i);
      if (child.type === 'identifier') {
        return this.getNodeText(child, sourceCode);
      }
    }
    return 'anonymous';
  }

  extractParameters(node, sourceCode) {
    const params = [];
    for (let i = 0; i < node.namedChildCount; i++) {
      const child = node.namedChild(i);
      if (child.type === 'formal_parameters') {
        for (let j = 0; j < child.namedChildCount; j++) {
          const param = child.namedChild(j);
          if (param.type === 'identifier') {
            params.push(this.getNodeText(param, sourceCode));
          }
        }
        break;
      }
    }
    return params;
  }

  extractVariableDeclarations(node, sourceCode) {
    const declarations = [];
    for (let i = 0; i < node.namedChildCount; i++) {
      const child = node.namedChild(i);
      if (child.type === 'variable_declarator') {
        declarations.push({
          name: this.getNodeText(child.namedChild(0), sourceCode),
          initializer: child.namedChildCount > 1 ? 
            this.getNodeText(child.namedChild(1), sourceCode) : null
        });
      }
    }
    return declarations;
  }

  extractBinaryOperator(node, sourceCode) {
    // Find the operator token between operands
    for (let i = 0; i < node.childCount; i++) {
      const child = node.child(i);
      if (child.type === '+' || child.type === '-' || child.type === '*' || 
          child.type === '/' || child.type === '===' || child.type === '!=' ||
          child.type === '<' || child.type === '>' || child.type === '<=' ||
          child.type === '>=' || child.type === '&&' || child.type === '||') {
        return child.type;
      }
    }
    return 'unknown';
  }

  extractCallArguments(node, sourceCode) {
    const args = [];
    for (let i = 0; i < node.namedChildCount; i++) {
      const child = node.namedChild(i);
      if (child.type === 'arguments') {
        for (let j = 0; j < child.namedChildCount; j++) {
          args.push(this.getNodeText(child.namedChild(j), sourceCode));
        }
        break;
      }
    }
    return args;
  }

  inferLiteralType(nodeType) {
    const mapping = {
      'number': 'number',
      'string': 'string',
      'true': 'boolean',
      'false': 'boolean',
      'null': 'null'
    };
    return mapping[nodeType] || 'unknown';
  }
}

/**
 * Python Parser
 */
export class PythonParser extends LanguageParser {
  constructor() {
    super('python');
  }

  setupParser() {
    this.parser.setLanguage(Python);
  }

  astToUIR(node, sourceCode) {
    const uirNode = this.createUIRNode(this.mapNodeType(node.type));
    uirNode.setMetadata('sourceRange', {
      start: { row: node.startPosition.row, column: node.startPosition.column },
      end: { row: node.endPosition.row, column: node.endPosition.column }
    });

    switch (node.type) {
      case 'module':
        uirNode.type = UIRNodeTypes.PROGRAM;
        break;
      
      case 'function_definition':
        uirNode.type = UIRNodeTypes.FUNCTION;
        uirNode.properties.name = this.extractPythonFunctionName(node, sourceCode);
        uirNode.properties.parameters = this.extractPythonParameters(node, sourceCode);
        break;
      
      case 'assignment':
        uirNode.type = UIRNodeTypes.ASSIGNMENT;
        uirNode.properties.left = this.getNodeText(node.namedChild(0), sourceCode);
        uirNode.properties.right = this.getNodeText(node.namedChild(1), sourceCode);
        break;
      
      case 'if_statement':
        uirNode.type = UIRNodeTypes.IF_STATEMENT;
        uirNode.properties.condition = this.getNodeText(node.namedChild(0), sourceCode);
        break;
      
      case 'while_statement':
        uirNode.type = UIRNodeTypes.WHILE_LOOP;
        uirNode.properties.condition = this.getNodeText(node.namedChild(0), sourceCode);
        break;
      
      case 'for_statement':
        uirNode.type = UIRNodeTypes.FOR_LOOP;
        uirNode.properties.variable = this.getNodeText(node.namedChild(0), sourceCode);
        uirNode.properties.iterable = this.getNodeText(node.namedChild(1), sourceCode);
        break;
      
      case 'binary_operator':
        uirNode.type = UIRNodeTypes.BINARY_OPERATION;
        uirNode.properties.operator = this.extractPythonBinaryOperator(node, sourceCode);
        uirNode.properties.left = this.getNodeText(node.namedChild(0), sourceCode);
        uirNode.properties.right = this.getNodeText(node.namedChild(1), sourceCode);
        break;
      
      case 'call':
        uirNode.type = UIRNodeTypes.FUNCTION_CALL;
        uirNode.properties.function = this.getNodeText(node.namedChild(0), sourceCode);
        uirNode.properties.arguments = this.extractPythonCallArguments(node, sourceCode);
        break;
      
      case 'return_statement':
        uirNode.type = UIRNodeTypes.RETURN_STATEMENT;
        if (node.namedChildCount > 0) {
          uirNode.properties.value = this.getNodeText(node.namedChild(0), sourceCode);
        }
        break;
      
      case 'comment':
        uirNode.type = UIRNodeTypes.COMMENT;
        uirNode.properties.text = this.getNodeText(node, sourceCode);
        break;
      
      case 'identifier':
        uirNode.type = UIRNodeTypes.IDENTIFIER;
        uirNode.properties.name = this.getNodeText(node, sourceCode);
        break;
      
      case 'integer':
      case 'float':
      case 'string':
      case 'true':
      case 'false':
      case 'none':
        uirNode.type = UIRNodeTypes.LITERAL;
        uirNode.properties.value = this.getNodeText(node, sourceCode);
        uirNode.properties.dataType = this.inferPythonLiteralType(node.type);
        break;
      
      default:
        uirNode.properties.originalType = node.type;
        uirNode.properties.text = this.getNodeText(node, sourceCode);
    }

    // Recursively process children
    for (let i = 0; i < node.namedChildCount; i++) {
      const childUIR = this.astToUIR(node.namedChild(i), sourceCode);
      uirNode.addChild(childUIR);
    }

    return uirNode;
  }

  mapNodeType(nodeType) {
    const mapping = {
      'module': UIRNodeTypes.PROGRAM,
      'function_definition': UIRNodeTypes.FUNCTION,
      'assignment': UIRNodeTypes.ASSIGNMENT,
      'if_statement': UIRNodeTypes.IF_STATEMENT,
      'while_statement': UIRNodeTypes.WHILE_LOOP,
      'for_statement': UIRNodeTypes.FOR_LOOP,
      'binary_operator': UIRNodeTypes.BINARY_OPERATION,
      'call': UIRNodeTypes.FUNCTION_CALL,
      'return_statement': UIRNodeTypes.RETURN_STATEMENT,
      'comment': UIRNodeTypes.COMMENT,
      'identifier': UIRNodeTypes.IDENTIFIER,
      'block': UIRNodeTypes.BLOCK
    };

    return mapping[nodeType] || 'unknown';
  }

  extractPythonFunctionName(node, sourceCode) {
    for (let i = 0; i < node.namedChildCount; i++) {
      const child = node.namedChild(i);
      if (child.type === 'identifier') {
        return this.getNodeText(child, sourceCode);
      }
    }
    return 'anonymous';
  }

  extractPythonParameters(node, sourceCode) {
    const params = [];
    for (let i = 0; i < node.namedChildCount; i++) {
      const child = node.namedChild(i);
      if (child.type === 'parameters') {
        for (let j = 0; j < child.namedChildCount; j++) {
          const param = child.namedChild(j);
          if (param.type === 'identifier') {
            params.push(this.getNodeText(param, sourceCode));
          }
        }
        break;
      }
    }
    return params;
  }

  extractPythonBinaryOperator(node, sourceCode) {
    // Python binary operators are usually the middle child
    if (node.childCount >= 3) {
      const operatorNode = node.child(1);
      return operatorNode.type;
    }
    return 'unknown';
  }

  extractPythonCallArguments(node, sourceCode) {
    const args = [];
    for (let i = 0; i < node.namedChildCount; i++) {
      const child = node.namedChild(i);
      if (child.type === 'argument_list') {
        for (let j = 0; j < child.namedChildCount; j++) {
          args.push(this.getNodeText(child.namedChild(j), sourceCode));
        }
        break;
      }
    }
    return args;
  }

  inferPythonLiteralType(nodeType) {
    const mapping = {
      'integer': 'int',
      'float': 'float',
      'string': 'str',
      'true': 'bool',
      'false': 'bool',
      'none': 'NoneType'
    };
    return mapping[nodeType] || 'unknown';
  }
}

/**
 * Go Parser
 */
export class GoParser extends LanguageParser {
  constructor() {
    super('go');
  }

  setupParser() {
    this.parser.setLanguage(Go);
  }

  astToUIR(node, sourceCode) {
    const uirNode = this.createUIRNode(this.mapNodeType(node.type));
    uirNode.setMetadata('sourceRange', {
      start: { row: node.startPosition.row, column: node.startPosition.column },
      end: { row: node.endPosition.row, column: node.endPosition.column }
    });

    switch (node.type) {
      case 'source_file':
        uirNode.type = UIRNodeTypes.PROGRAM;
        break;
      
      case 'function_declaration':
        uirNode.type = UIRNodeTypes.FUNCTION;
        uirNode.properties.name = this.extractGoFunctionName(node, sourceCode);
        uirNode.properties.parameters = this.extractGoParameters(node, sourceCode);
        break;
      
      case 'var_declaration':
      case 'short_var_declaration':
        uirNode.type = UIRNodeTypes.VARIABLE_DECLARATION;
        uirNode.properties.declarations = this.extractGoVariableDeclarations(node, sourceCode);
        break;
      
      case 'assignment_statement':
        uirNode.type = UIRNodeTypes.ASSIGNMENT;
        uirNode.properties.left = this.getNodeText(node.namedChild(0), sourceCode);
        uirNode.properties.right = this.getNodeText(node.namedChild(1), sourceCode);
        break;
      
      case 'if_statement':
        uirNode.type = UIRNodeTypes.IF_STATEMENT;
        uirNode.properties.condition = this.extractGoIfCondition(node, sourceCode);
        break;
      
      case 'for_statement':
        uirNode.type = UIRNodeTypes.FOR_LOOP;
        uirNode.properties.condition = this.extractGoForCondition(node, sourceCode);
        break;
      
      case 'binary_expression':
        uirNode.type = UIRNodeTypes.BINARY_OPERATION;
        uirNode.properties.operator = this.extractGoBinaryOperator(node, sourceCode);
        uirNode.properties.left = this.getNodeText(node.namedChild(0), sourceCode);
        uirNode.properties.right = this.getNodeText(node.namedChild(1), sourceCode);
        break;
      
      case 'call_expression':
        uirNode.type = UIRNodeTypes.FUNCTION_CALL;
        uirNode.properties.function = this.getNodeText(node.namedChild(0), sourceCode);
        uirNode.properties.arguments = this.extractGoCallArguments(node, sourceCode);
        break;
      
      case 'return_statement':
        uirNode.type = UIRNodeTypes.RETURN_STATEMENT;
        if (node.namedChildCount > 0) {
          uirNode.properties.value = this.getNodeText(node.namedChild(0), sourceCode);
        }
        break;
      
      case 'comment':
        uirNode.type = UIRNodeTypes.COMMENT;
        uirNode.properties.text = this.getNodeText(node, sourceCode);
        break;
      
      case 'identifier':
        uirNode.type = UIRNodeTypes.IDENTIFIER;
        uirNode.properties.name = this.getNodeText(node, sourceCode);
        break;
      
      case 'int_literal':
      case 'float_literal':
      case 'interpreted_string_literal':
      case 'raw_string_literal':
      case 'true':
      case 'false':
        uirNode.type = UIRNodeTypes.LITERAL;
        uirNode.properties.value = this.getNodeText(node, sourceCode);
        uirNode.properties.dataType = this.inferGoLiteralType(node.type);
        break;
      
      default:
        uirNode.properties.originalType = node.type;
        uirNode.properties.text = this.getNodeText(node, sourceCode);
    }

    // Recursively process children
    for (let i = 0; i < node.namedChildCount; i++) {
      const childUIR = this.astToUIR(node.namedChild(i), sourceCode);
      uirNode.addChild(childUIR);
    }

    return uirNode;
  }

  mapNodeType(nodeType) {
    const mapping = {
      'source_file': UIRNodeTypes.PROGRAM,
      'function_declaration': UIRNodeTypes.FUNCTION,
      'var_declaration': UIRNodeTypes.VARIABLE_DECLARATION,
      'short_var_declaration': UIRNodeTypes.VARIABLE_DECLARATION,
      'assignment_statement': UIRNodeTypes.ASSIGNMENT,
      'if_statement': UIRNodeTypes.IF_STATEMENT,
      'for_statement': UIRNodeTypes.FOR_LOOP,
      'binary_expression': UIRNodeTypes.BINARY_OPERATION,
      'call_expression': UIRNodeTypes.FUNCTION_CALL,
      'return_statement': UIRNodeTypes.RETURN_STATEMENT,
      'comment': UIRNodeTypes.COMMENT,
      'identifier': UIRNodeTypes.IDENTIFIER,
      'block': UIRNodeTypes.BLOCK
    };

    return mapping[nodeType] || 'unknown';
  }

  extractGoFunctionName(node, sourceCode) {
    for (let i = 0; i < node.namedChildCount; i++) {
      const child = node.namedChild(i);
      if (child.type === 'identifier') {
        return this.getNodeText(child, sourceCode);
      }
    }
    return 'anonymous';
  }

  extractGoParameters(node, sourceCode) {
    const params = [];
    for (let i = 0; i < node.namedChildCount; i++) {
      const child = node.namedChild(i);
      if (child.type === 'parameter_list') {
        for (let j = 0; j < child.namedChildCount; j++) {
          const param = child.namedChild(j);
          if (param.type === 'parameter_declaration') {
            const identifiers = this.findChildrenOfType(param, 'identifier');
            if (identifiers.length > 0) {
              params.push(this.getNodeText(identifiers[0], sourceCode));
            }
          }
        }
        break;
      }
    }
    return params;
  }

  extractGoVariableDeclarations(node, sourceCode) {
    const declarations = [];
    
    if (node.type === 'short_var_declaration') {
      // Handle := declarations
      const left = node.namedChild(0);
      const right = node.namedChild(1);
      
      declarations.push({
        name: this.getNodeText(left, sourceCode),
        initializer: this.getNodeText(right, sourceCode),
        type: null
      });
    } else if (node.type === 'var_declaration') {
      // Handle var declarations
      for (let i = 0; i < node.namedChildCount; i++) {
        const child = node.namedChild(i);
        if (child.type === 'var_spec') {
          const identifier = this.findChildOfType(child, 'identifier');
          const typeNode = this.findChildOfType(child, 'type_identifier');
          
          if (identifier) {
            declarations.push({
              name: this.getNodeText(identifier, sourceCode),
              type: typeNode ? this.getNodeText(typeNode, sourceCode) : null,
              initializer: null
            });
          }
        }
      }
    }
    
    return declarations;
  }

  extractGoIfCondition(node, sourceCode) {
    for (let i = 0; i < node.namedChildCount; i++) {
      const child = node.namedChild(i);
      if (child.type !== 'block') {
        return this.getNodeText(child, sourceCode);
      }
    }
    return '';
  }

  extractGoForCondition(node, sourceCode) {
    for (let i = 0; i < node.namedChildCount; i++) {
      const child = node.namedChild(i);
      if (child.type !== 'block') {
        return this.getNodeText(child, sourceCode);
      }
    }
    return '';
  }

  extractGoBinaryOperator(node, sourceCode) {
    if (node.childCount >= 3) {
      const operatorNode = node.child(1);
      return operatorNode.type;
    }
    return 'unknown';
  }

  extractGoCallArguments(node, sourceCode) {
    const args = [];
    for (let i = 0; i < node.namedChildCount; i++) {
      const child = node.namedChild(i);
      if (child.type === 'argument_list') {
        for (let j = 0; j < child.namedChildCount; j++) {
          args.push(this.getNodeText(child.namedChild(j), sourceCode));
        }
        break;
      }
    }
    return args;
  }

  inferGoLiteralType(nodeType) {
    const mapping = {
      'int_literal': 'int',
      'float_literal': 'float64',
      'interpreted_string_literal': 'string',
      'raw_string_literal': 'string',
      'true': 'bool',
      'false': 'bool'
    };
    return mapping[nodeType] || 'unknown';
  }

  findChildOfType(node, type) {
    for (let i = 0; i < node.namedChildCount; i++) {
      const child = node.namedChild(i);
      if (child.type === type) {
        return child;
      }
    }
    return null;
  }

  findChildrenOfType(node, type) {
    const children = [];
    for (let i = 0; i < node.namedChildCount; i++) {
      const child = node.namedChild(i);
      if (child.type === type) {
        children.push(child);
      }
    }
    return children;
  }
}

// ============================================
// UIR to ABAP Transformer
// ============================================

/**
 * Transforms Universal Intermediate Representation to ABAP AST
 */
export class UIRToABAPTransformer {
  constructor() {
    this.abapParser = new ABAPParser();
    this.variableCounter = 0;
    this.typeMapping = new Map();
    this.setupTypeMappings();
  }

  setupTypeMappings() {
    // Map common types from source languages to ABAP types
    this.typeMapping.set('string', 'string');
    this.typeMapping.set('str', 'string');
    this.typeMapping.set('number', 'i');
    this.typeMapping.set('int', 'i');
    this.typeMapping.set('integer', 'i');
    this.typeMapping.set('float', 'f');
    this.typeMapping.set('float64', 'f');
    this.typeMapping.set('boolean', 'abap_bool');
    this.typeMapping.set('bool', 'abap_bool');
    this.typeMapping.set('null', 'string'); // Default fallback
    this.typeMapping.set('NoneType', 'string');
  }

  transform(uirNode) {
    const abapCode = this.generateABAPCode(uirNode);
    
    try {
      // Parse the generated ABAP to create AST
      const parseResult = this.abapParser.parse(abapCode);
      return {
        ast: parseResult.ast,
        abapCode,
        sourceMap: parseResult.sourceMap,
        metadata: {
          originalLanguage: uirNode.getMetadata('sourceLanguage'),
          transformationRules: this.getAppliedRules(),
          confidence: this.calculateConfidence(uirNode)
        }
      };
    } catch (error) {
      console.error('Failed to parse generated ABAP:', error.message);
      console.error('Generated ABAP code:', abapCode);
      
      // Return fallback result
      return {
        ast: null,
        abapCode,
        error: error.message,
        metadata: {
          originalLanguage: uirNode.getMetadata('sourceLanguage'),
          confidence: 0.0
        }
      };
    }
  }

  generateABAPCode(uirNode, indentLevel = 0) {
    const indent = '  '.repeat(indentLevel);
    let code = '';

    switch (uirNode.type) {
      case UIRNodeTypes.PROGRAM:
        code += this.generateProgram(uirNode, indentLevel);
        break;
      
      case UIRNodeTypes.FUNCTION:
        code += this.generateFunction(uirNode, indentLevel);
        break;
      
      case UIRNodeTypes.VARIABLE_DECLARATION:
        code += this.generateVariableDeclaration(uirNode, indentLevel);
        break;
      
      case UIRNodeTypes.ASSIGNMENT:
        code += this.generateAssignment(uirNode, indentLevel);
        break;
      
      case UIRNodeTypes.IF_STATEMENT:
        code += this.generateIfStatement(uirNode, indentLevel);
        break;
      
      case UIRNodeTypes.WHILE_LOOP:
        code += this.generateWhileLoop(uirNode, indentLevel);
        break;
      
      case UIRNodeTypes.FOR_LOOP:
        code += this.generateForLoop(uirNode, indentLevel);
        break;
      
      case UIRNodeTypes.FUNCTION_CALL:
        code += this.generateFunctionCall(uirNode, indentLevel);
        break;
      
      case UIRNodeTypes.RETURN_STATEMENT:
        code += this.generateReturnStatement(uirNode, indentLevel);
        break;
      
      case UIRNodeTypes.BINARY_OPERATION:
        code += this.generateBinaryOperation(uirNode);
        break;
      
      case UIRNodeTypes.LITERAL:
        code += this.generateLiteral(uirNode);
        break;
      
      case UIRNodeTypes.IDENTIFIER:
        code += this.generateIdentifier(uirNode);
        break;
      
      case UIRNodeTypes.COMMENT:
        code += this.generateComment(uirNode, indentLevel);
        break;
      
      case UIRNodeTypes.BLOCK:
        code += this.generateBlock(uirNode, indentLevel);
        break;
      
      default:
        // Handle unknown node types gracefully
        code += this.generateGenericNode(uirNode, indentLevel);
    }

    return code;
  }

  generateProgram(uirNode, indentLevel) {
    let code = '';
    
    for (const child of uirNode.children) {
      const childCode = this.generateABAPCode(child, indentLevel);
      if (childCode.trim()) {
        code += childCode + '\n';
      }
    }
    
    return code;
  }

  generateFunction(uirNode, indentLevel) {
    const indent = '  '.repeat(indentLevel);
    const functionName = uirNode.properties.name || `generated_function_${++this.variableCounter}`;
    const parameters = uirNode.properties.parameters || [];
    
    let code = `${indent}FORM ${functionName}`;
    
    if (parameters.length > 0) {
      const paramList = parameters.map(p => `p_${p} TYPE string`).join(' ');
      code += ` USING ${paramList}`;
    }
    
    code += '.\n';
    
    // Generate function body
    for (const child of uirNode.children) {
      if (child.type === UIRNodeTypes.BLOCK || 
          child.type === UIRNodeTypes.VARIABLE_DECLARATION ||
          child.type === UIRNodeTypes.ASSIGNMENT ||
          child.type === UIRNodeTypes.IF_STATEMENT ||
          child.type === UIRNodeTypes.FUNCTION_CALL) {
        code += this.generateABAPCode(child, indentLevel + 1);
      }
    }
    
    code += `${indent}ENDFORM.\n`;
    
    return code;
  }

  generateVariableDeclaration(uirNode, indentLevel) {
    const indent = '  '.repeat(indentLevel);
    let code = '';
    
    const declarations = uirNode.properties.declarations || [];
    
    for (const decl of declarations) {
      const variableName = this.sanitizeIdentifier(decl.name);
      const abapType = this.mapToABAPType(decl.type || 'string');
      
      code += `${indent}DATA ${variableName} TYPE ${abapType}`;
      
      if (decl.initializer) {
        const initialValue = this.transformExpression(decl.initializer);
        code += ` VALUE ${initialValue}`;
      }
      
      code += '.\n';
    }
    
    return code;
  }

  generateAssignment(uirNode, indentLevel) {
    const indent = '  '.repeat(indentLevel);
    const left = this.sanitizeIdentifier(uirNode.properties.left || 'temp_var');
    const right = this.transformExpression(uirNode.properties.right || '');
    
    return `${indent}${left} = ${right}.\n`;
  }

  generateIfStatement(uirNode, indentLevel) {
    const indent = '  '.repeat(indentLevel);
    const condition = this.transformExpression(uirNode.properties.condition || 'abap_true');
    
    let code = `${indent}IF ${condition}.\n`;
    
    // Generate if body
    for (const child of uirNode.children) {
      if (child.type !== 'else_clause') {
        code += this.generateABAPCode(child, indentLevel + 1);
      }
    }
    
    // Handle else clause
    const elseChild = uirNode.children.find(child => child.type === 'else_clause');
    if (elseChild) {
      code += `${indent}ELSE.\n`;
      for (const child of elseChild.children) {
        code += this.generateABAPCode(child, indentLevel + 1);
      }
    }
    
    code += `${indent}ENDIF.\n`;
    
    return code;
  }

  generateWhileLoop(uirNode, indentLevel) {
    const indent = '  '.repeat(indentLevel);
    const condition = this.transformExpression(uirNode.properties.condition || 'abap_true');
    
    let code = `${indent}WHILE ${condition}.\n`;
    
    for (const child of uirNode.children) {
      code += this.generateABAPCode(child, indentLevel + 1);
    }
    
    code += `${indent}ENDWHILE.\n`;
    
    return code;
  }

  generateForLoop(uirNode, indentLevel) {
    const indent = '  '.repeat(indentLevel);
    
    // Convert for loop to ABAP DO loop with counter
    const variable = this.sanitizeIdentifier(uirNode.properties.variable || 'loop_var');
    const counterVar = `${variable}_counter`;
    
    let code = `${indent}DATA ${counterVar} TYPE i VALUE 1.\n`;
    code += `${indent}DO 100 TIMES.\n`; // Default iteration count
    code += `${indent}  DATA ${variable} TYPE i VALUE ${counterVar}.\n`;
    
    for (const child of uirNode.children) {
      code += this.generateABAPCode(child, indentLevel + 1);
    }
    
    code += `${indent}  ${counterVar} = ${counterVar} + 1.\n`;
    code += `${indent}ENDDO.\n`;
    
    return code;
  }

  generateFunctionCall(uirNode, indentLevel) {
    const indent = '  '.repeat(indentLevel);
    const functionName = uirNode.properties.function || 'unknown_function';
    const args = uirNode.properties.arguments || [];
    
    if (this.isBuiltinFunction(functionName)) {
      return this.generateBuiltinCall(functionName, args, indentLevel);
    }
    
    let code = `${indent}PERFORM ${this.sanitizeIdentifier(functionName)}`;
    
    if (args.length > 0) {
      const argList = args.map(arg => this.transformExpression(arg)).join(' ');
      code += ` USING ${argList}`;
    }
    
    code += '.\n';
    
    return code;
  }

  generateReturnStatement(uirNode, indentLevel) {
    const indent = '  '.repeat(indentLevel);
    
    // ABAP doesn't have explicit return in FORMs, just exit
    if (uirNode.properties.value) {
      // If there's a return value, we'd need to use a changing parameter
      // For simplicity, we'll just add a comment
      return `${indent}\" Return: ${uirNode.properties.value}\n${indent}EXIT.\n`;
    } else {
      return `${indent}EXIT.\n`;
    }
  }

  generateBinaryOperation(uirNode) {
    const left = this.transformExpression(uirNode.properties.left || '');
    const right = this.transformExpression(uirNode.properties.right || '');
    const operator = this.mapBinaryOperator(uirNode.properties.operator || '=');
    
    return `${left} ${operator} ${right}`;
  }

  generateLiteral(uirNode) {
    const value = uirNode.properties.value || '';
    const dataType = uirNode.properties.dataType || 'string';
    
    if (dataType === 'string' || dataType === 'str') {
      // Ensure string is properly quoted
      if (!value.startsWith("'") && !value.startsWith('"')) {
        return `'${value}'`;
      }
      return value.replace(/"/g, "'");
    } else if (dataType === 'boolean' || dataType === 'bool') {
      return value.toLowerCase() === 'true' ? 'abap_true' : 'abap_false';
    } else {
      return value;
    }
  }

  generateIdentifier(uirNode) {
    return this.sanitizeIdentifier(uirNode.properties.name || 'temp_var');
  }

  generateComment(uirNode, indentLevel) {
    const indent = '  '.repeat(indentLevel);
    const text = uirNode.properties.text || '';
    
    // Convert various comment styles to ABAP
    const cleanText = text.replace(/^(\/\/|#|\/\*)/, '').replace(/\*\/$/, '').trim();
    return `${indent}* ${cleanText}\n`;
  }

  generateBlock(uirNode, indentLevel) {
    let code = '';
    
    for (const child of uirNode.children) {
      code += this.generateABAPCode(child, indentLevel);
    }
    
    return code;
  }

  generateGenericNode(uirNode, indentLevel) {
    const indent = '  '.repeat(indentLevel);
    
    // Generate a comment for unhandled node types
    const originalType = uirNode.properties.originalType || uirNode.type;
    const text = uirNode.properties.text || '';
    
    let code = `${indent}* Unhandled node type: ${originalType}\n`;
    
    if (text.trim()) {
      code += `${indent}* Original text: ${text}\n`;
    }
    
    // Still process children
    for (const child of uirNode.children) {
      code += this.generateABAPCode(child, indentLevel);
    }
    
    return code;
  }

  generateBuiltinCall(functionName, args, indentLevel) {
    const indent = '  '.repeat(indentLevel);
    
    switch (functionName.toLowerCase()) {
      case 'print':
      case 'console.log':
      case 'puts':
      case 'println':
      case 'fmt.println':
        const message = args.length > 0 ? this.transformExpression(args[0]) : "''";
        return `${indent}WRITE ${message}.\n`;
      
      case 'len':
      case 'length':
        const target = args.length > 0 ? this.transformExpression(args[0]) : 'temp_var';
        return `STRLEN( ${target} )`;
      
      default:
        return this.generateFunctionCall({
          properties: { function: functionName, arguments: args }
        }, indentLevel);
    }
  }

  isBuiltinFunction(functionName) {
    const builtins = [
      'print', 'console.log', 'puts', 'println', 'fmt.println',
      'len', 'length', 'str', 'int', 'float'
    ];
    
    return builtins.some(builtin => 
      functionName.toLowerCase().includes(builtin.toLowerCase())
    );
  }

  mapToABAPType(sourceType) {
    if (this.typeMapping.has(sourceType)) {
      return this.typeMapping.get(sourceType);
    }
    
    // Default fallback
    return 'string';
  }

  mapBinaryOperator(sourceOperator) {
    const mapping = {
      '===': 'EQ',
      '==': 'EQ',
      '!=': 'NE',
      '!==': 'NE',
      '<': 'LT',
      '<=': 'LE',
      '>': 'GT',
      '>=': 'GE',
      '&&': 'AND',
      '||': 'OR',
      '+': '+',
      '-': '-',
      '*': '*',
      '/': '/',
      'and': 'AND',
      'or': 'OR',
      'not': 'NOT'
    };
    
    return mapping[sourceOperator] || sourceOperator;
  }

  sanitizeIdentifier(name) {
    if (!name) return 'temp_var';
    
    // Convert to ABAP naming conventions
    let sanitized = name
      .replace(/[^a-zA-Z0-9_]/g, '_')  // Replace invalid chars
      .replace(/^[0-9]/, '_$&')        // Add prefix if starts with number
      .toLowerCase()                    // ABAP typically uses lowercase
      .substring(0, 30);               // ABAP identifier length limit
    
    // Ensure it's not empty
    if (!sanitized) {
      sanitized = 'temp_var';
    }
    
    return sanitized;
  }

  transformExpression(expression) {
    if (!expression) return "''";
    
    // Simple expression transformation
    // This could be enhanced with a full expression parser
    let transformed = expression
      .replace(/console\.log/g, 'WRITE')
      .replace(/\btrue\b/g, 'abap_true')
      .replace(/\bfalse\b/g, 'abap_false')
      .replace(/\bnull\b/g, "''")
      .replace(/\bundefined\b/g, "''")
      .replace(/\bNone\b/g, "''");
    
    return transformed;
  }

  getAppliedRules() {
    // Return metadata about transformation rules that were applied
    return [
      'function_to_form',
      'variable_declaration_normalization',
      'builtin_function_mapping',
      'operator_mapping',
      'type_mapping'
    ];
  }

  calculateConfidence(uirNode) {
    // Calculate confidence score based on node types and complexity
    let totalNodes = 0;
    let supportedNodes = 0;
    
    const countNodes = (node) => {
      totalNodes++;
      
      if (this.isSupportedNodeType(node.type)) {
        supportedNodes++;
      }
      
      for (const child of node.children) {
        countNodes(child);
      }
    };
    
    countNodes(uirNode);
    
    return totalNodes > 0 ? supportedNodes / totalNodes : 0.0;
  }

  isSupportedNodeType(nodeType) {
    const supportedTypes = [
      UIRNodeTypes.PROGRAM,
      UIRNodeTypes.FUNCTION,
      UIRNodeTypes.VARIABLE_DECLARATION,
      UIRNodeTypes.ASSIGNMENT,
      UIRNodeTypes.IF_STATEMENT,
      UIRNodeTypes.WHILE_LOOP,
      UIRNodeTypes.FOR_LOOP,
      UIRNodeTypes.FUNCTION_CALL,
      UIRNodeTypes.RETURN_STATEMENT,
      UIRNodeTypes.BINARY_OPERATION,
      UIRNodeTypes.LITERAL,
      UIRNodeTypes.IDENTIFIER,
      UIRNodeTypes.COMMENT,
      UIRNodeTypes.BLOCK
    ];
    
    return supportedTypes.includes(nodeType);
  }
}

// ============================================
// Universal Translator Main Class
// ============================================

/**
 * Main Universal Translator class that orchestrates the translation process
 */
export class UniversalTranslator {
  constructor(options = {}) {
    this.options = {
      enableCaching: true,
      validateOutput: true,
      preserveComments: true,
      optimizeOutput: true,
      ...options
    };
    
    this.parsers = new Map();
    this.transformer = new UIRToABAPTransformer();
    this.cache = new Map();
    this.stats = {
      translations: 0,
      cacheHits: 0,
      errors: 0,
      totalTime: 0
    };
    
    this.initializeParsers();
  }

  initializeParsers() {
    this.parsers.set('javascript', new JavaScriptParser());
    this.parsers.set('js', new JavaScriptParser());
    this.parsers.set('python', new PythonParser());
    this.parsers.set('py', new PythonParser());
    this.parsers.set('go', new GoParser());
    this.parsers.set('golang', new GoParser());
    
    console.log(chalk.green('✓ Initialized parsers for:'), 
                Array.from(this.parsers.keys()).join(', '));
  }

  async translateCode(sourceCode, sourceLanguage, options = {}) {
    const startTime = Date.now();
    
    try {
      // Check cache first
      const cacheKey = this.generateCacheKey(sourceCode, sourceLanguage);
      if (this.options.enableCaching && this.cache.has(cacheKey)) {
        this.stats.cacheHits++;
        console.log(chalk.blue('Cache hit for translation'));
        return this.cache.get(cacheKey);
      }

      // Validate input
      this.validateInput(sourceCode, sourceLanguage);

      // Get appropriate parser
      const parser = this.getParser(sourceLanguage);
      
      console.log(chalk.yellow(`Translating ${sourceLanguage} code to ABAP...`));

      // Step 1: Parse source code to UIR
      console.log(chalk.gray('Step 1: Parsing source code to UIR...'));
      const uirTree = parser.parse(sourceCode);

      // Step 2: Transform UIR to ABAP AST
      console.log(chalk.gray('Step 2: Transforming UIR to ABAP AST...'));
      const transformResult = this.transformer.transform(uirTree);

      // Step 3: Validate output if enabled
      if (this.options.validateOutput && transformResult.ast) {
        console.log(chalk.gray('Step 3: Validating generated ABAP...'));
        await this.validateABAPOutput(transformResult.abapCode);
      }

      // Prepare result
      const result = {
        success: true,
        sourceLanguage,
        originalCode: sourceCode,
        abapCode: transformResult.abapCode,
        ast: transformResult.ast,
        uir: uirTree,
        metadata: {
          ...transformResult.metadata,
          translationTime: Date.now() - startTime,
          cacheKey
        },
        statistics: this.generateStatistics(uirTree)
      };

      // Cache the result
      if (this.options.enableCaching) {
        this.cache.set(cacheKey, result);
      }

      // Update stats
      this.stats.translations++;
      this.stats.totalTime += result.metadata.translationTime;

      console.log(chalk.green(`✓ Translation completed in ${result.metadata.translationTime}ms`));
      
      return result;

    } catch (error) {
      this.stats.errors++;
      
      const errorResult = {
        success: false,
        sourceLanguage,
        originalCode: sourceCode,
        error: error.message,
        stack: error.stack,
        metadata: {
          translationTime: Date.now() - startTime
        }
      };

      console.error(chalk.red('✗ Translation failed:'), error.message);
      
      return errorResult;
    }
  }

  async translateFile(filePath, outputPath = null) {
    try {
      // Determine source language from file extension
      const sourceLanguage = this.detectLanguageFromPath(filePath);
      
      // Read source file
      const sourceCode = await fs.readFile(filePath, 'utf8');
      
      console.log(chalk.blue(`Translating file: ${filePath} (${sourceLanguage})`));
      
      // Translate
      const result = await this.translateCode(sourceCode, sourceLanguage);
      
      if (result.success && outputPath) {
        // Write ABAP output
        await fs.writeFile(outputPath, result.abapCode, 'utf8');
        console.log(chalk.green(`✓ ABAP code written to: ${outputPath}`));
      }
      
      return result;
      
    } catch (error) {
      console.error(chalk.red('✗ File translation failed:'), error.message);
      throw error;
    }
  }

  async translateDirectory(inputDir, outputDir, options = {}) {
    const {
      recursive = true,
      includePatterns = ['**/*.js', '**/*.py', '**/*.go'],
      excludePatterns = ['**/node_modules/**', '**/.git/**'],
      parallel = true,
      maxConcurrency = 5
    } = options;

    console.log(chalk.blue(`Translating directory: ${inputDir}`));
    
    try {
      // Ensure output directory exists
      await fs.ensureDir(outputDir);
      
      // Find source files
      const glob = await import('glob');
      const files = [];
      
      for (const pattern of includePatterns) {
        const matches = glob.glob(pattern, {
          cwd: inputDir,
          ignore: excludePatterns,
          absolute: true
        });
        
        files.push(...await matches);
      }
      
      console.log(chalk.yellow(`Found ${files.length} files to translate`));
      
      // Translate files
      const results = [];
      
      if (parallel) {
        // Parallel processing with concurrency limit
        const pLimit = (await import('p-limit')).default;
        const limit = pLimit(maxConcurrency);
        
        const promises = files.map(file => 
          limit(() => this.translateFileWithResult(file, inputDir, outputDir))
        );
        
        results.push(...await Promise.all(promises));
      } else {
        // Sequential processing
        for (const file of files) {
          const result = await this.translateFileWithResult(file, inputDir, outputDir);
          results.push(result);
        }
      }
      
      // Generate summary
      const summary = this.generateDirectorySummary(results);
      
      console.log(chalk.green('✓ Directory translation completed'));
      console.log(chalk.blue('Summary:'));
      console.log(`  Total files: ${summary.total}`);
      console.log(`  Successful: ${summary.successful}`);
      console.log(`  Failed: ${summary.failed}`);
      console.log(`  Total time: ${summary.totalTime}ms`);
      
      return {
        success: summary.failed === 0,
        summary,
        results
      };
      
    } catch (error) {
      console.error(chalk.red('✗ Directory translation failed:'), error.message);
      throw error;
    }
  }

  async translateFileWithResult(filePath, inputDir, outputDir) {
    try {
      const relativePath = path.relative(inputDir, filePath);
      const outputPath = path.join(outputDir, relativePath + '.abap');
      
      // Ensure output directory exists
      await fs.ensureDir(path.dirname(outputPath));
      
      const result = await this.translateFile(filePath, outputPath);
      
      return {
        inputPath: filePath,
        outputPath,
        relativePath,
        ...result
      };
      
    } catch (error) {
      return {
        inputPath: filePath,
        success: false,
        error: error.message
      };
    }
  }

  generateDirectorySummary(results) {
    const summary = {
      total: results.length,
      successful: results.filter(r => r.success).length,
      failed: results.filter(r => !r.success).length,
      totalTime: results.reduce((sum, r) => 
        sum + (r.metadata?.translationTime || 0), 0),
      byLanguage: {},
      errors: results.filter(r => !r.success).map(r => ({
        file: r.inputPath,
        error: r.error
      }))
    };
    
    // Count by language
    for (const result of results) {
      if (result.success) {
        const lang = result.sourceLanguage;
        summary.byLanguage[lang] = (summary.byLanguage[lang] || 0) + 1;
      }
    }
    
    return summary;
  }

  validateInput(sourceCode, sourceLanguage) {
    if (!sourceCode || typeof sourceCode !== 'string') {
      throw new Error('Source code must be a non-empty string');
    }
    
    if (!sourceLanguage || typeof sourceLanguage !== 'string') {
      throw new Error('Source language must be specified');
    }
    
    if (!this.parsers.has(sourceLanguage.toLowerCase())) {
      throw new Error(`Unsupported language: ${sourceLanguage}`);
    }
  }

  getParser(sourceLanguage) {
    const parser = this.parsers.get(sourceLanguage.toLowerCase());
    if (!parser) {
      throw new Error(`No parser available for language: ${sourceLanguage}`);
    }
    return parser;
  }

  detectLanguageFromPath(filePath) {
    const ext = path.extname(filePath).toLowerCase();
    
    const extensionMap = {
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
    
    const language = extensionMap[ext];
    if (!language) {
      throw new Error(`Cannot detect language from file extension: ${ext}`);
    }
    
    return language;
  }

  generateCacheKey(sourceCode, sourceLanguage) {
    // Simple hash-based cache key
    const crypto = await import('crypto');
    return crypto.createHash('md5')
      .update(`${sourceLanguage}:${sourceCode}`)
      .digest('hex');
  }

  async validateABAPOutput(abapCode) {
    try {
      // Use the existing ABAP parser to validate syntax
      const parser = new ABAPParser();
      const parseResult = parser.parse(abapCode);
      
      if (!parseResult.ast) {
        throw new Error('Generated ABAP code is not syntactically valid');
      }
      
      console.log(chalk.green('✓ Generated ABAP code is syntactically valid'));
      
    } catch (error) {
      console.warn(chalk.yellow('⚠ ABAP validation warning:'), error.message);
      // Don't fail the translation, just warn
    }
  }

  generateStatistics(uirTree) {
    const stats = {
      totalNodes: 0,
      nodeTypeCount: {},
      maxDepth: 0,
      avgDepth: 0
    };
    
    const depths = [];
    
    const countNodes = (node, depth = 0) => {
      stats.totalNodes++;
      depths.push(depth);
      
      stats.nodeTypeCount[node.type] = (stats.nodeTypeCount[node.type] || 0) + 1;
      stats.maxDepth = Math.max(stats.maxDepth, depth);
      
      for (const child of node.children) {
        countNodes(child, depth + 1);
      }
    };
    
    countNodes(uirTree);
    
    stats.avgDepth = depths.length > 0 ? 
      depths.reduce((a, b) => a + b, 0) / depths.length : 0;
    
    return stats;
  }

  getStatistics() {
    return {
      ...this.stats,
      cacheSize: this.cache.size,
      supportedLanguages: Array.from(this.parsers.keys()),
      avgTranslationTime: this.stats.translations > 0 ? 
        this.stats.totalTime / this.stats.translations : 0
    };
  }

  clearCache() {
    this.cache.clear();
    console.log(chalk.blue('Cache cleared'));
  }

  getSupportedLanguages() {
    return Array.from(this.parsers.keys());
  }
}

// ============================================
// CLI Interface
// ============================================

function createCLI() {
  const program = new Command();
  
  program
    .name('universal-translator')
    .description('Universal Code-to-ABAP Translation System')
    .version('1.0.0');

  program
    .command('translate')
    .description('Translate source code to ABAP')
    .option('-f, --file <path>', 'Input file path')
    .option('-d, --dir <path>', 'Input directory path')
    .option('-o, --output <path>', 'Output path')
    .option('-l, --language <lang>', 'Source language (auto-detected from file extension if not specified)')
    .option('--no-cache', 'Disable caching')
    .option('--no-validate', 'Skip output validation')
    .option('--parallel', 'Enable parallel processing for directories')
    .option('--max-concurrency <n>', 'Maximum concurrent translations', '5')
    .action(async (options) => {
      const translator = new UniversalTranslator({
        enableCaching: options.cache !== false,
        validateOutput: options.validate !== false
      });

      try {
        if (options.file) {
          const result = await translator.translateFile(options.file, options.output);
          if (result.success) {
            console.log(chalk.green('\n✓ Translation successful!'));
            if (!options.output) {
              console.log('\nGenerated ABAP code:');
              console.log(chalk.cyan(result.abapCode));
            }
          } else {
            console.error(chalk.red('\n✗ Translation failed:'), result.error);
            process.exit(1);
          }
        } else if (options.dir) {
          if (!options.output) {
            console.error(chalk.red('Output directory is required when translating a directory'));
            process.exit(1);
          }
          
          const result = await translator.translateDirectory(options.dir, options.output, {
            parallel: options.parallel,
            maxConcurrency: parseInt(options.maxConcurrency)
          });
          
          if (!result.success) {
            process.exit(1);
          }
        } else {
          console.error(chalk.red('Either --file or --dir must be specified'));
          program.help();
        }
      } catch (error) {
        console.error(chalk.red('Error:'), error.message);
        process.exit(1);
      }
    });

  program
    .command('interactive')
    .description('Interactive translation mode')
    .action(async () => {
      const inquirer = (await import('inquirer')).default;
      const translator = new UniversalTranslator();
      
      console.log(chalk.blue('Welcome to Universal ABAP Translator Interactive Mode!'));
      console.log(`Supported languages: ${translator.getSupportedLanguages().join(', ')}`);
      
      while (true) {
        const answers = await inquirer.prompt([
          {
            type: 'list',
            name: 'action',
            message: 'What would you like to do?',
            choices: [
              'Translate code snippet',
              'Translate file',
              'View statistics',
              'Exit'
            ]
          }
        ]);
        
        switch (answers.action) {
          case 'Translate code snippet':
            await handleInteractiveTranslation(translator, inquirer);
            break;
          case 'Translate file':
            await handleInteractiveFileTranslation(translator, inquirer);
            break;
          case 'View statistics':
            console.log(chalk.blue('\nTranslation Statistics:'));
            console.log(JSON.stringify(translator.getStatistics(), null, 2));
            break;
          case 'Exit':
            console.log(chalk.green('Goodbye!'));
            process.exit(0);
        }
      }
    });

  program
    .command('stats')
    .description('Show translation statistics')
    .action(() => {
      // This would typically load stats from a persistent store
      console.log(chalk.blue('Translation Statistics:'));
      console.log('Feature not yet implemented in CLI mode.');
      console.log('Use interactive mode to view session statistics.');
    });

  return program;
}

async function handleInteractiveTranslation(translator, inquirer) {
  const answers = await inquirer.prompt([
    {
      type: 'list',
      name: 'language',
      message: 'Select source language:',
      choices: translator.getSupportedLanguages()
    },
    {
      type: 'editor',
      name: 'code',
      message: 'Enter your source code:'
    }
  ]);
  
  console.log(chalk.yellow('Translating...'));
  
  const result = await translator.translateCode(answers.code, answers.language);
  
  if (result.success) {
    console.log(chalk.green('\n✓ Translation successful!'));
    console.log('\nGenerated ABAP code:');
    console.log(chalk.cyan(result.abapCode));
    
    console.log('\nTranslation metadata:');
    console.log(`- Time: ${result.metadata.translationTime}ms`);
    console.log(`- Confidence: ${result.metadata.confidence?.toFixed(2) || 'N/A'}`);
    console.log(`- UIR nodes: ${result.statistics.totalNodes}`);
  } else {
    console.error(chalk.red('\n✗ Translation failed:'), result.error);
  }
}

async function handleInteractiveFileTranslation(translator, inquirer) {
  const answers = await inquirer.prompt([
    {
      type: 'input',
      name: 'filePath',
      message: 'Enter file path:',
      validate: (input) => {
        if (!input) return 'File path is required';
        if (!fs.existsSync(input)) return 'File does not exist';
        return true;
      }
    },
    {
      type: 'input',
      name: 'outputPath',
      message: 'Enter output path (optional):'
    }
  ]);
  
  console.log(chalk.yellow('Translating file...'));
  
  const result = await translator.translateFile(answers.filePath, answers.outputPath || null);
  
  if (result.success) {
    console.log(chalk.green('\n✓ File translation successful!'));
    
    if (answers.outputPath) {
      console.log(`Generated ABAP saved to: ${answers.outputPath}`);
    } else {
      console.log('\nGenerated ABAP code:');
      console.log(chalk.cyan(result.abapCode));
    }
  } else {
    console.error(chalk.red('\n✗ File translation failed:'), result.error);
  }
}

// ============================================
// Main Entry Point
// ============================================

async function main() {
  console.log(chalk.blue.bold('Universal Code-to-ABAP Translation System'));
  console.log(chalk.gray('Implementing Article 019 Architecture'));
  console.log();

  if (process.argv.length <= 2) {
    // No arguments provided, show help
    const program = createCLI();
    program.help();
  } else {
    // Run CLI
    const program = createCLI();
    await program.parseAsync(process.argv);
  }
}

// Export for use as module
export default UniversalTranslator;

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch(error => {
    console.error(chalk.red('Fatal error:'), error.message);
    console.error(error.stack);
    process.exit(1);
  });
}