#!/usr/bin/env node

/**
 * ABAP AST-Based Dataset Generator
 * Creates masked training pairs using AST node manipulation (NOT regex)
 * Implements 4-level hierarchical masking strategy
 */

const fs = require('fs').promises;
const path = require('path');
const crypto = require('crypto');

/**
 * Level 1: Expression-level masking
 * Masks individual expressions within statements
 */
class ExpressionMasker {
  static MASKABLE_TYPES = [
    'FieldChain',
    'Constant',
    'StringTemplate',
    'ArithOperator',
    'CompareOperator',
    'MethodCall',
    'FunctionCall',
    'TableExpression'
  ];
  
  static mask(astData, abapCode) {
    const masks = [];
    
    // Work with our AST structure - traverse statements
    for (const statement of astData.statements) {
      this.processStatement(statement, abapCode, masks);
        // Create mask for this expression
        const mask = {
          id: this.generateId('expr'),
          level: 1,
          task: 'fill_mask',
          astPath: path,
          node: {
            type: node.type,
            parent: parent?.type,
            token: node.token
          },
          original: this.extractCode(abapCode, node),
          masked: this.applyMask(abapCode, node, '<MASK>'),
          target: node.token || this.extractCode(abapCode, node)
        };
        
        // Add context
        mask.context = this.extractContext(astNode, node, parent);
        
        masks.push(mask);
      }
    });
    
    return masks;
  }
  
  static traverse(node, callback, parent = null, path = []) {
    callback(node, parent, path);
    
    if (node.children) {
      node.children.forEach((child, index) => {
        this.traverse(child, callback, node, [...path, 'children', index]);
      });
    }
    
    if (node.nodes) {
      node.nodes.forEach((child, index) => {
        this.traverse(child, callback, node, [...path, 'nodes', index]);
      });
    }
  }
  
  static extractCode(fullCode, node) {
    if (!node.start || !node.end) return node.token || '';
    const lines = fullCode.split('\n');
    const startLine = node.start.row - 1;
    const endLine = node.end.row - 1;
    
    if (startLine === endLine) {
      return lines[startLine].substring(node.start.col - 1, node.end.col);
    }
    
    // Multi-line expression
    let code = lines[startLine].substring(node.start.col - 1);
    for (let i = startLine + 1; i < endLine; i++) {
      code += '\n' + lines[i];
    }
    code += '\n' + lines[endLine].substring(0, node.end.col);
    return code.trim();
  }
  
  static applyMask(fullCode, node, maskToken) {
    const lines = fullCode.split('\n');
    const startLine = node.start.row - 1;
    const endLine = node.end.row - 1;
    
    const before = lines.slice(0, startLine).join('\n');
    const after = lines.slice(endLine + 1).join('\n');
    
    let masked;
    if (startLine === endLine) {
      const line = lines[startLine];
      masked = line.substring(0, node.start.col - 1) + 
               maskToken + 
               line.substring(node.end.col);
    } else {
      masked = lines[startLine].substring(0, node.start.col - 1) + maskToken;
    }
    
    return [before, masked, after].filter(s => s).join('\n');
  }
  
  static extractContext(fullAst, node, parent) {
    // Find containing method/class
    let method = null;
    let className = null;
    
    this.traverse(fullAst, (n, p) => {
      if (n.type === 'Method' && this.isAncestor(n, node)) {
        method = n.token;
      }
      if (n.type === 'ClassDefinition' && this.isAncestor(n, node)) {
        className = n.token;
      }
    });
    
    return {
      method,
      class: className,
      parentStatement: parent?.type
    };
  }
  
  static isAncestor(ancestor, node) {
    // Check if ancestor contains node based on position
    return ancestor.start?.row <= node.start?.row && 
           ancestor.end?.row >= node.end?.row;
  }
  
  static generateId(prefix) {
    return `abap_${prefix}_${crypto.randomBytes(4).toString('hex')}`;
  }
}

/**
 * Level 2: Statement-level masking
 * Masks complete statements
 */
class StatementMasker {
  static MASKABLE_STATEMENTS = [
    'Assignment',
    'MethodCall',
    'Loop',
    'If',
    'Case',
    'Data',
    'Return',
    'Raise'
  ];
  
  static mask(astNode, abapCode) {
    const masks = [];
    
    ExpressionMasker.traverse(astNode, (node, parent, path) => {
      if (this.MASKABLE_STATEMENTS.includes(node.type)) {
        const mask = {
          id: ExpressionMasker.generateId('stmt'),
          level: 2,
          task: 'complete_statement',
          astPath: path,
          node: {
            type: node.type,
            parent: parent?.type
          },
          original: ExpressionMasker.extractCode(abapCode, node),
          masked: '<MASK>',
          target: ExpressionMasker.extractCode(abapCode, node)
        };
        
        mask.context = this.extractStatementContext(astNode, node);
        masks.push(mask);
      }
    });
    
    return masks;
  }
  
  static extractStatementContext(fullAst, node) {
    const context = ExpressionMasker.extractContext(fullAst, node, null);
    
    // Add surrounding statements info
    const siblings = [];
    ExpressionMasker.traverse(fullAst, (n, p) => {
      if (p === node.parent && n !== node) {
        siblings.push(n.type);
      }
    });
    
    context.pattern = this.identifyPattern(node);
    context.surroundingStatements = siblings.slice(0, 3);
    
    return context;
  }
  
  static identifyPattern(node) {
    // Identify common ABAP patterns
    if (node.type === 'Data' && node.token?.includes('REDUCE')) {
      return 'reduce_operation';
    }
    if (node.type === 'Data' && node.token?.includes('VALUE #')) {
      return 'value_constructor';
    }
    if (node.type === 'If' && node.token?.includes('COND')) {
      return 'conditional_expression';
    }
    return node.type.toLowerCase();
  }
}

/**
 * Level 3: Block-level masking
 * Masks method implementations, loops, conditions
 */
class BlockMasker {
  static MASKABLE_BLOCKS = [
    'Method',
    'Loop',
    'If',
    'Case',
    'Try',
    'ClassSection'
  ];
  
  static mask(astNode, abapCode) {
    const masks = [];
    
    ExpressionMasker.traverse(astNode, (node, parent, path) => {
      if (this.MASKABLE_BLOCKS.includes(node.type)) {
        // Extract method signature if it's a method
        let signature = null;
        if (node.type === 'Method') {
          signature = this.extractMethodSignature(node);
        }
        
        const mask = {
          id: ExpressionMasker.generateId('block'),
          level: 3,
          task: 'implement_block',
          astPath: path,
          node: {
            type: node.type,
            name: node.token
          },
          masked: this.createBlockMask(node, abapCode),
          target: this.extractBlockImplementation(node, abapCode),
          signature
        };
        
        masks.push(mask);
      }
    });
    
    return masks;
  }
  
  static extractMethodSignature(node) {
    const signature = {
      name: node.token,
      importing: [],
      exporting: [],
      changing: [],
      returning: null
    };
    
    // Parse parameters from AST
    ExpressionMasker.traverse(node, (n) => {
      if (n.type === 'MethodParameter') {
        const param = {
          name: n.token,
          type: n.dataType
        };
        
        if (n.kind === 'IMPORTING') signature.importing.push(param);
        if (n.kind === 'EXPORTING') signature.exporting.push(param);
        if (n.kind === 'CHANGING') signature.changing.push(param);
        if (n.kind === 'RETURNING') signature.returning = param;
      }
    });
    
    return signature;
  }
  
  static createBlockMask(node, abapCode) {
    const code = ExpressionMasker.extractCode(abapCode, node);
    const lines = code.split('\n');
    
    if (node.type === 'Method') {
      // Keep method signature, mask implementation
      const firstLine = lines[0];
      const lastLine = lines[lines.length - 1];
      return `${firstLine}\n  <MASK>\n${lastLine}`;
    }
    
    // For other blocks, mask the content
    return '<MASK>';
  }
  
  static extractBlockImplementation(node, abapCode) {
    const code = ExpressionMasker.extractCode(abapCode, node);
    
    if (node.type === 'Method') {
      // Extract just the implementation body
      const lines = code.split('\n');
      return lines.slice(1, -1).join('\n').trim();
    }
    
    return code;
  }
}

/**
 * Level 4: Structure-level masking
 * Masks entire classes, interfaces
 */
class StructureMasker {
  static mask(astNode, abapCode) {
    const masks = [];
    
    ExpressionMasker.traverse(astNode, (node, parent, path) => {
      if (node.type === 'ClassDefinition' || node.type === 'ClassImplementation') {
        const mask = {
          id: ExpressionMasker.generateId('struct'),
          level: 4,
          task: 'complete_class',
          prompt: this.generatePrompt(node),
          masked: '<MASK>',
          target: ExpressionMasker.extractCode(abapCode, node)
        };
        
        masks.push(mask);
      }
    });
    
    return masks;
  }
  
  static generatePrompt(node) {
    const className = node.token;
    const methods = [];
    
    ExpressionMasker.traverse(node, (n) => {
      if (n.type === 'Method') {
        methods.push(n.token);
      }
    });
    
    return `Implement ${className} with methods: ${methods.join(', ')}`;
  }
}

/**
 * Main dataset generator
 */
class DatasetGenerator {
  constructor(options = {}) {
    this.corpusDir = options.corpus || 'examples';
    this.astDir = options.ast || 'datasets/abap-ast';
    this.outputDir = options.output || 'datasets/abap-masked-pairs';
    this.levels = options.levels || [1, 2, 3, 4];
  }
  
  async generate() {
    console.log('=== ABAP AST-Based Dataset Generator ===');
    console.log(`Corpus: ${this.corpusDir}`);
    console.log(`AST: ${this.astDir}`);
    console.log(`Output: ${this.outputDir}`);
    console.log(`Levels: ${this.levels.join(', ')}\n`);
    
    // Create output directories
    await this.createOutputDirs();
    
    // Load AST files
    const astFiles = await this.loadAstFiles();
    console.log(`Found ${astFiles.length} AST files\n`);
    
    // Process each file
    const allMasks = {
      level1: [],
      level2: [],
      level3: [],
      level4: []
    };
    
    for (const astFile of astFiles) {
      console.log(`Processing: ${astFile.name}`);
      
      // Load corresponding ABAP code
      const abapCode = await this.loadAbapCode(astFile);
      
      // Generate masks for each level
      if (this.levels.includes(1)) {
        const masks = ExpressionMasker.mask(astFile.ast, abapCode);
        allMasks.level1.push(...masks);
        console.log(`  Level 1: ${masks.length} expression masks`);
      }
      
      if (this.levels.includes(2)) {
        const masks = StatementMasker.mask(astFile.ast, abapCode);
        allMasks.level2.push(...masks);
        console.log(`  Level 2: ${masks.length} statement masks`);
      }
      
      if (this.levels.includes(3)) {
        const masks = BlockMasker.mask(astFile.ast, abapCode);
        allMasks.level3.push(...masks);
        console.log(`  Level 3: ${masks.length} block masks`);
      }
      
      if (this.levels.includes(4)) {
        const masks = StructureMasker.mask(astFile.ast, abapCode);
        allMasks.level4.push(...masks);
        console.log(`  Level 4: ${masks.length} structure masks`);
      }
    }
    
    // Write output files
    await this.writeOutputFiles(allMasks);
    
    // Generate summary
    await this.writeSummary(allMasks);
    
    console.log('\n✓ Dataset generation completed successfully');
  }
  
  async createOutputDirs() {
    const dirs = [
      this.outputDir,
      path.join(this.outputDir, 'level1_expressions'),
      path.join(this.outputDir, 'level2_statements'),
      path.join(this.outputDir, 'level3_blocks'),
      path.join(this.outputDir, 'level4_structures')
    ];
    
    for (const dir of dirs) {
      await fs.mkdir(dir, { recursive: true });
    }
  }
  
  async loadAstFiles() {
    const files = await fs.readdir(this.astDir);
    const astFiles = [];
    
    for (const file of files) {
      if (file.endsWith('.ast.json')) {
        const content = await fs.readFile(path.join(this.astDir, file), 'utf8');
        astFiles.push({
          name: file,
          ast: JSON.parse(content)
        });
      }
    }
    
    return astFiles;
  }
  
  async loadAbapCode(astFile) {
    // Reconstruct ABAP file path from AST filename
    const abapFile = astFile.name
      .replace('.ast.json', '.abap')
      .replace(/-/g, '/');
    
    const abapPath = path.join(this.corpusDir, abapFile);
    
    try {
      return await fs.readFile(abapPath, 'utf8');
    } catch (error) {
      console.warn(`  Warning: Could not load ABAP file: ${abapPath}`);
      return '';
    }
  }
  
  async writeOutputFiles(allMasks) {
    // Level 1: Group by pattern type
    const level1Groups = {
      value_expressions: [],
      cond_expressions: [],
      reduce_operations: [],
      string_templates: [],
      inline_declarations: []
    };
    
    for (const mask of allMasks.level1) {
      if (mask.original.includes('VALUE #')) {
        level1Groups.value_expressions.push(mask);
      } else if (mask.original.includes('COND #')) {
        level1Groups.cond_expressions.push(mask);
      } else if (mask.original.includes('REDUCE')) {
        level1Groups.reduce_operations.push(mask);
      } else if (mask.original.includes('|{')) {
        level1Groups.string_templates.push(mask);
      } else if (mask.original.includes('DATA(')) {
        level1Groups.inline_declarations.push(mask);
      }
    }
    
    for (const [group, masks] of Object.entries(level1Groups)) {
      if (masks.length > 0) {
        await this.writeJsonl(
          path.join(this.outputDir, 'level1_expressions', `${group}.jsonl`),
          masks
        );
      }
    }
    
    // Level 2: Group by statement type
    const level2Groups = {
      method_calls: [],
      loop_statements: [],
      conditionals: [],
      assignments: []
    };
    
    for (const mask of allMasks.level2) {
      if (mask.node.type === 'MethodCall') {
        level2Groups.method_calls.push(mask);
      } else if (mask.node.type === 'Loop') {
        level2Groups.loop_statements.push(mask);
      } else if (mask.node.type === 'If' || mask.node.type === 'Case') {
        level2Groups.conditionals.push(mask);
      } else if (mask.node.type === 'Assignment') {
        level2Groups.assignments.push(mask);
      }
    }
    
    for (const [group, masks] of Object.entries(level2Groups)) {
      if (masks.length > 0) {
        await this.writeJsonl(
          path.join(this.outputDir, 'level2_statements', `${group}.jsonl`),
          masks
        );
      }
    }
    
    // Level 3: Write all blocks
    if (allMasks.level3.length > 0) {
      await this.writeJsonl(
        path.join(this.outputDir, 'level3_blocks', 'blocks.jsonl'),
        allMasks.level3
      );
    }
    
    // Level 4: Write all structures
    if (allMasks.level4.length > 0) {
      await this.writeJsonl(
        path.join(this.outputDir, 'level4_structures', 'structures.jsonl'),
        allMasks.level4
      );
    }
  }
  
  async writeJsonl(filePath, data) {
    const lines = data.map(item => JSON.stringify(item));
    await fs.writeFile(filePath, lines.join('\n'));
  }
  
  async writeSummary(allMasks) {
    const summary = {
      timestamp: new Date().toISOString(),
      statistics: {
        level1: allMasks.level1.length,
        level2: allMasks.level2.length,
        level3: allMasks.level3.length,
        level4: allMasks.level4.length,
        total: allMasks.level1.length + allMasks.level2.length + 
               allMasks.level3.length + allMasks.level4.length
      },
      corpus: {
        directory: this.corpusDir,
        astDirectory: this.astDir
      },
      output: this.outputDir
    };
    
    await fs.writeFile(
      path.join(this.outputDir, 'summary.json'),
      JSON.stringify(summary, null, 2)
    );
    
    console.log('\n=== Generation Summary ===');
    console.log(`Level 1 (Expressions): ${summary.statistics.level1}`);
    console.log(`Level 2 (Statements): ${summary.statistics.level2}`);
    console.log(`Level 3 (Blocks): ${summary.statistics.level3}`);
    console.log(`Level 4 (Structures): ${summary.statistics.level4}`);
    console.log(`Total masked pairs: ${summary.statistics.total}`);
  }
}

/**
 * CLI entry point
 */
async function main() {
  const args = process.argv.slice(2);
  
  const options = {
    corpus: 'examples',
    ast: 'datasets/abap-ast',
    output: 'datasets/abap-masked-pairs',
    levels: [1, 2, 3, 4]
  };
  
  for (let i = 0; i < args.length; i++) {
    switch (args[i]) {
      case '--corpus':
        options.corpus = args[++i];
        break;
      case '--ast':
        options.ast = args[++i];
        break;
      case '--output':
        options.output = args[++i];
        break;
      case '--levels':
        options.levels = args[++i].split(',').map(Number);
        break;
      case '--help':
        console.log(`
ABAP AST-Based Dataset Generator

Usage: node dataset-generator-abap-ast.js [options]

Options:
  --corpus <dir>   ABAP corpus directory (default: examples)
  --ast <dir>      AST directory (default: datasets/abap-ast)
  --output <dir>   Output directory (default: datasets/abap-masked-pairs)
  --levels <list>  Comma-separated levels (default: 1,2,3,4)
  --help          Show this help message

Example:
  node dataset-generator-abap-ast.js --corpus examples --levels 1,2,3,4
        `);
        process.exit(0);
    }
  }
  
  const generator = new DatasetGenerator(options);
  await generator.generate();
}

// Run if called directly
if (require.main === module) {
  main().catch(console.error);
}

module.exports = { 
  ExpressionMasker, 
  StatementMasker, 
  BlockMasker, 
  StructureMasker,
  DatasetGenerator 
};