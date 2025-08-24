#!/usr/bin/env node

/**
 * ABAP AST Extraction Tool
 * Uses our proven bidirectional transformer for AST extraction
 * Based on our 60% AST equivalence achievement
 */

const fs = require('fs').promises;
const path = require('path');
const abaplint = require('@abaplint/core');
const { glob } = require('glob');

/**
 * Extract AST from ABAP file using our proven approach
 * Based on the bidirectional transformer from docs/007
 */
async function extractABAPAst(filePath, content) {
  const registry = new abaplint.Registry();
  
  // Add file to registry using the approach that works - use .prog.abap extension
  const fileName = path.basename(filePath).replace('.abap', '.prog.abap');
  const file = new abaplint.MemoryFile(fileName, content);
  registry.addFile(file);
  registry.parse();
  
  // Get the parsed object
  const obj = registry.getFirstObject();
  if (!obj) {
    throw new Error(`Failed to parse ABAP file: ${filePath}`);
  }
  
  // Get the ABAP file
  const abapFile = obj.getABAPFiles()[0];
  if (!abapFile) {
    throw new Error(`No ABAP file found in: ${filePath}`);
  }
  
  const statements = abapFile.getStatements();
  const structure = abapFile.getStructure();
  
  // Build comprehensive AST representation
  const ast = {
    file: path.basename(filePath),
    type: 'ABAP',
    version: '7.40',
    statements: [],
    structure: null,
    statistics: {
      totalStatements: 0,
      byType: {}
    }
  };
  
  // Process statements - our proven approach from the equivalence tests
  for (const statement of statements) {
    const stmtNode = {
      type: statement.get().constructor.name,
      tokens: statement.getTokens().map(t => ({
        str: t.getStr(),
        start: t.getStart(),
        end: t.getEnd()
      })),
      start: statement.getFirstToken()?.getStart(),
      end: statement.getLastToken()?.getEnd(),
      children: []
    };
    
    // Process children tokens
    for (const child of statement.getChildren()) {
      const childNode = processChild(child);
      if (childNode) {
        stmtNode.children.push(childNode);
      }
    }
    
    // Count node types for statistics
    ast.statistics.byType[stmtNode.type] = (ast.statistics.byType[stmtNode.type] || 0) + 1;
    ast.statistics.totalStatements++;
    
    ast.statements.push(stmtNode);
  }
  
  // Add structure if available
  if (structure) {
    ast.structure = processStructure(structure);
  }
  
  return ast;
}

function processChild(node) {
  const result = {
    type: node.get().constructor.name,
    str: node.getFirstToken ? node.getFirstToken()?.getStr() : undefined,
    children: []
  };
  
  // Only process tokens if the method exists
  if (node.getTokens) {
    result.tokens = node.getTokens().map(t => ({
      str: t.getStr(),
      start: t.getStart(),
      end: t.getEnd()
    }));
  }
  
  // Process children if they exist
  if (node.getChildren) {
    result.children = node.getChildren().map(c => processChild(c));
  }
  
  return result;
}

function processStructure(structure) {
  return {
    type: structure.get().constructor.name,
    children: structure.getChildren().map(child => ({
      type: child.get().constructor.name,
      statements: child.getStatements ? child.getStatements().length : 0,
      children: child.getChildren ? processStructure(child) : null
    }))
  };
}

/**
 * Process all ABAP files in directory
 */
async function processDirectory(inputDir, pattern, outputDir) {
  // Find all ABAP files
  const files = await glob(path.join(inputDir, pattern));
  console.log(`Found ${files.length} ABAP files to process`);
  
  // Ensure output directory exists
  await fs.mkdir(outputDir, { recursive: true });
  
  const results = [];
  
  for (const file of files) {
    try {
      console.log(`Processing: ${file}`);
      
      // Read ABAP file
      const content = await fs.readFile(file, 'utf8');
      
      // Extract AST
      const ast = await extractABAPAst(file, content);
      
      // Generate output filename
      const relativePath = path.relative(inputDir, file);
      const outputName = relativePath
        .replace(/\//g, '-')
        .replace(/\\/g, '-')
        .replace('.abap', '.ast.json');
      const outputPath = path.join(outputDir, outputName);
      
      // Write AST to file
      await fs.writeFile(outputPath, JSON.stringify(ast, null, 2));
      
      results.push({
        source: file,
        output: outputPath,
        statements: ast.statistics.totalStatements,
        types: Object.keys(ast.statistics.byType).length
      });
      
      console.log(`  ✓ Generated AST with ${ast.statistics.totalStatements} statements`);
    } catch (error) {
      console.error(`  ✗ Error processing ${file}: ${error.message}`);
    }
  }
  
  // Write summary
  const summary = {
    timestamp: new Date().toISOString(),
    filesProcessed: results.length,
    totalStatements: results.reduce((sum, r) => sum + r.statements, 0),
    files: results
  };
  
  await fs.writeFile(
    path.join(outputDir, 'summary.json'),
    JSON.stringify(summary, null, 2)
  );
  
  console.log('\n=== Summary ===');
  console.log(`Files processed: ${summary.filesProcessed}`);
  console.log(`Total AST statements: ${summary.totalStatements}`);
  console.log(`Output directory: ${outputDir}`);
}

/**
 * CLI entry point
 */
async function main() {
  const args = process.argv.slice(2);
  
  // Parse command line arguments
  let inputDir = 'examples';
  let pattern = '**/*.abap';
  let outputDir = 'datasets/abap-ast';
  
  for (let i = 0; i < args.length; i++) {
    switch (args[i]) {
      case '--input':
        inputDir = args[++i];
        break;
      case '--pattern':
        pattern = args[++i];
        break;
      case '--output':
        outputDir = args[++i];
        break;
      case '--help':
        console.log(`
ABAP AST Extraction Tool

Usage: node extract-abap-ast.js [options]

Options:
  --input <dir>     Input directory (default: examples)
  --pattern <glob>  File pattern (default: **/*.abap)
  --output <dir>    Output directory (default: datasets/abap-ast)
  --help           Show this help message

Example:
  node extract-abap-ast.js --input examples --output datasets/abap-ast
        `);
        process.exit(0);
    }
  }
  
  console.log('=== ABAP AST Extraction Tool ===');
  console.log(`Input directory: ${inputDir}`);
  console.log(`File pattern: ${pattern}`);
  console.log(`Output directory: ${outputDir}\n`);
  
  try {
    await processDirectory(inputDir, pattern, outputDir);
    console.log('\n✓ AST extraction completed successfully');
  } catch (error) {
    console.error('\n✗ Error:', error.message);
    process.exit(1);
  }
}

// Run if called directly
if (require.main === module) {
  main().catch(console.error);
}

module.exports = { extractABAPAst, processDirectory };