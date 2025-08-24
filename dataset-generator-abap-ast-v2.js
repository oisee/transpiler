#!/usr/bin/env node

/**
 * ABAP AST-Based Dataset Generator V2
 * Works with our proven AST structure from the bidirectional transformer
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
  static EXPRESSION_PATTERNS = [
    { pattern: 'VALUE #', type: 'value_constructor' },
    { pattern: 'COND #', type: 'conditional_expression' },
    { pattern: 'REDUCE', type: 'reduce_operation' },
    { pattern: 'FILTER', type: 'filter_expression' },
    { pattern: 'DATA(', type: 'inline_declaration' },
    { pattern: '|{', type: 'string_template' }
  ];
  
  static mask(astData, abapCode) {
    const masks = [];
    const lines = abapCode.split('\n');
    
    // Process each statement
    for (const statement of astData.statements) {
      // Get the full statement text
      const stmtText = this.getStatementText(statement, lines);
      
      // Look for maskable expressions
      for (const pattern of this.EXPRESSION_PATTERNS) {
        if (stmtText.includes(pattern.pattern)) {
          // Find tokens that could be masked
          const maskableTokens = this.findMaskableTokens(statement, pattern);
          
          for (const token of maskableTokens) {
            masks.push({
              id: this.generateId('expr'),
              level: 1,
              task: 'fill_mask',
              type: pattern.type,
              original: stmtText,
              masked: this.maskToken(stmtText, token),
              target: token.str,
              context: {
                statementType: statement.type,
                pattern: pattern.type,
                lineNumber: token.start?.row
              }
            });
          }
        }
      }
    }
    
    return masks;
  }
  
  static getStatementText(statement, lines) {
    if (!statement.start || !statement.end) return '';
    
    const startLine = statement.start.row - 1;
    const endLine = statement.end.row - 1;
    
    if (startLine === endLine) {
      return lines[startLine]?.substring(statement.start.col - 1, statement.end.col) || '';
    }
    
    let text = lines[startLine]?.substring(statement.start.col - 1) || '';
    for (let i = startLine + 1; i < endLine; i++) {
      text += '\n' + (lines[i] || '');
    }
    if (endLine < lines.length) {
      text += '\n' + lines[endLine]?.substring(0, statement.end.col) || '';
    }
    
    return text;
  }
  
  static findMaskableTokens(statement, pattern) {
    const maskable = [];
    
    // Look for specific token types to mask
    if (statement.tokens) {
      for (let i = 0; i < statement.tokens.length; i++) {
        const token = statement.tokens[i];
        
        // Mask variables, literals, and field names
        if (this.isMaskableToken(token, statement.tokens, i)) {
          maskable.push(token);
          if (maskable.length >= 3) break; // Limit masks per statement
        }
      }
    }
    
    return maskable;
  }
  
  static isMaskableToken(token, allTokens, index) {
    const str = token.str;
    
    // Don't mask keywords
    const keywords = ['DATA', 'VALUE', 'COND', 'REDUCE', 'FILTER', 'TYPE', 'FOR', 'IN', 'NEXT', 'WHEN', 'THEN', 'ELSE'];
    if (keywords.includes(str.toUpperCase())) return false;
    
    // Don't mask operators
    if (['=', '+', '-', '*', '/', '(', ')', '[', ']', '.', ',', ':', '#'].includes(str)) return false;
    
    // Mask variables, literals, field names
    return /^[a-zA-Z_]\w*$/.test(str) || /^['"`].*['"`]$/.test(str) || /^\d+$/.test(str);
  }
  
  static maskToken(text, token) {
    return text.replace(token.str, '<MASK>');
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
    'Move',
    'Data',
    'MethodCall',
    'Loop',
    'If',
    'Case',
    'Return'
  ];
  
  static mask(astData, abapCode) {
    const masks = [];
    const lines = abapCode.split('\n');
    
    for (const statement of astData.statements) {
      if (this.MASKABLE_STATEMENTS.includes(statement.type)) {
        const stmtText = ExpressionMasker.getStatementText(statement, lines);
        
        if (stmtText && stmtText.length > 10) { // Skip trivial statements
          masks.push({
            id: ExpressionMasker.generateId('stmt'),
            level: 2,
            task: 'complete_statement',
            type: statement.type,
            original: stmtText,
            masked: '<MASK>',
            target: stmtText,
            context: {
              statementType: statement.type,
              lineNumber: statement.start?.row,
              tokenCount: statement.tokens?.length
            }
          });
        }
      }
    }
    
    return masks;
  }
}

/**
 * Level 3: Block-level masking
 * Masks method implementations, loops, conditions
 */
class BlockMasker {
  static mask(astData, abapCode) {
    const masks = [];
    const lines = abapCode.split('\n');
    
    // Group statements into blocks
    const blocks = this.identifyBlocks(astData.statements, lines);
    
    for (const block of blocks) {
      if (block.type === 'METHOD' || block.type === 'LOOP' || block.type === 'IF') {
        const blockText = this.getBlockText(block, lines);
        
        if (blockText) {
          // For methods, keep signature and mask body
          let masked = blockText;
          let target = blockText;
          
          if (block.type === 'METHOD') {
            const bodyStart = blockText.indexOf('\n') + 1;
            const bodyEnd = blockText.lastIndexOf('ENDMETHOD');
            
            if (bodyStart > 0 && bodyEnd > bodyStart) {
              const signature = blockText.substring(0, bodyStart);
              const body = blockText.substring(bodyStart, bodyEnd).trim();
              masked = `${signature}<MASK>\nENDMETHOD.`;
              target = body;
            }
          }
          
          masks.push({
            id: ExpressionMasker.generateId('block'),
            level: 3,
            task: 'implement_block',
            type: block.type,
            masked: masked,
            target: target,
            context: {
              blockType: block.type,
              lineStart: block.startLine,
              lineEnd: block.endLine,
              statementCount: block.statements.length
            }
          });
        }
      }
    }
    
    return masks;
  }
  
  static identifyBlocks(statements, lines) {
    const blocks = [];
    let currentBlock = null;
    
    for (let i = 0; i < statements.length; i++) {
      const stmt = statements[i];
      const stmtText = ExpressionMasker.getStatementText(stmt, lines).toUpperCase();
      
      // Start of block
      if (stmtText.includes('METHOD ') && !stmtText.includes('ENDMETHOD')) {
        currentBlock = {
          type: 'METHOD',
          startLine: stmt.start?.row,
          statements: [stmt]
        };
      } else if (stmtText.includes('LOOP ')) {
        currentBlock = {
          type: 'LOOP',
          startLine: stmt.start?.row,
          statements: [stmt]
        };
      } else if (stmtText.includes('IF ') && !stmtText.includes('ENDIF')) {
        currentBlock = {
          type: 'IF',
          startLine: stmt.start?.row,
          statements: [stmt]
        };
      }
      // End of block
      else if (currentBlock) {
        currentBlock.statements.push(stmt);
        
        if ((currentBlock.type === 'METHOD' && stmtText.includes('ENDMETHOD')) ||
            (currentBlock.type === 'LOOP' && stmtText.includes('ENDLOOP')) ||
            (currentBlock.type === 'IF' && stmtText.includes('ENDIF'))) {
          currentBlock.endLine = stmt.end?.row;
          blocks.push(currentBlock);
          currentBlock = null;
        }
      }
    }
    
    return blocks;
  }
  
  static getBlockText(block, lines) {
    if (!block.startLine || !block.endLine) return '';
    
    const startLine = block.startLine - 1;
    const endLine = block.endLine - 1;
    
    return lines.slice(startLine, endLine + 1).join('\n');
  }
}

/**
 * Level 4: Structure-level masking
 * Masks entire classes
 */
class StructureMasker {
  static mask(astData, abapCode) {
    const masks = [];
    const lines = abapCode.split('\n');
    
    // Look for class definitions
    const classBlocks = this.identifyClasses(lines);
    
    for (const classBlock of classBlocks) {
      masks.push({
        id: ExpressionMasker.generateId('struct'),
        level: 4,
        task: 'complete_class',
        prompt: `Implement ABAP class ${classBlock.name}`,
        masked: '<MASK>',
        target: classBlock.text,
        context: {
          className: classBlock.name,
          lineStart: classBlock.startLine,
          lineEnd: classBlock.endLine
        }
      });
    }
    
    return masks;
  }
  
  static identifyClasses(lines) {
    const classes = [];
    let currentClass = null;
    
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i].toUpperCase();
      
      if (line.includes('CLASS ') && line.includes(' DEFINITION')) {
        const match = lines[i].match(/CLASS\s+(\w+)/i);
        currentClass = {
          name: match ? match[1] : 'unknown',
          startLine: i + 1,
          lines: [lines[i]]
        };
      } else if (currentClass) {
        currentClass.lines.push(lines[i]);
        
        if (line.includes('ENDCLASS') && line.includes(currentClass.name.toUpperCase())) {
          currentClass.endLine = i + 1;
          currentClass.text = currentClass.lines.join('\n');
          classes.push(currentClass);
          currentClass = null;
        }
      }
    }
    
    return classes;
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
    console.log('=== ABAP AST-Based Dataset Generator V2 ===');
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
      
      if (!abapCode) {
        console.log(`  ⚠️  Skipping - no ABAP code found`);
        continue;
      }
      
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
      .replace('translations-', 'translations/')
      .replace('python-', 'python/')
      .replace('javascript-', 'javascript/')
      .replace('go-', 'go/');
    
    const abapPath = path.join(this.corpusDir, abapFile);
    
    try {
      return await fs.readFile(abapPath, 'utf8');
    } catch (error) {
      console.warn(`  Warning: Could not load ABAP file: ${abapPath}`);
      return '';
    }
  }
  
  async writeOutputFiles(allMasks) {
    // Write Level 1 masks
    if (allMasks.level1.length > 0) {
      await this.writeJsonl(
        path.join(this.outputDir, 'level1_expressions', 'expressions.jsonl'),
        allMasks.level1
      );
    }
    
    // Write Level 2 masks
    if (allMasks.level2.length > 0) {
      await this.writeJsonl(
        path.join(this.outputDir, 'level2_statements', 'statements.jsonl'),
        allMasks.level2
      );
    }
    
    // Write Level 3 masks
    if (allMasks.level3.length > 0) {
      await this.writeJsonl(
        path.join(this.outputDir, 'level3_blocks', 'blocks.jsonl'),
        allMasks.level3
      );
    }
    
    // Write Level 4 masks
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
ABAP AST-Based Dataset Generator V2

Usage: node dataset-generator-abap-ast-v2.js [options]

Options:
  --corpus <dir>   ABAP corpus directory (default: examples)
  --ast <dir>      AST directory (default: datasets/abap-ast)
  --output <dir>   Output directory (default: datasets/abap-masked-pairs)
  --levels <list>  Comma-separated levels (default: 1,2,3,4)
  --help          Show this help message

Example:
  node dataset-generator-abap-ast-v2.js --corpus examples --levels 1,2,3,4
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