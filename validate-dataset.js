#!/usr/bin/env node

/**
 * Dataset Validation Tool
 * Validates the quality and correctness of the fine-tuning dataset
 */

const fs = require('fs').promises;
const path = require('path');

class DatasetValidator {
  constructor(inputFile) {
    this.inputFile = inputFile;
    this.errors = [];
    this.warnings = [];
    this.stats = {
      total: 0,
      valid: 0,
      invalid: 0,
      avgPromptLength: 0,
      avgResponseLength: 0,
      minResponseLength: Infinity,
      maxResponseLength: 0
    };
  }
  
  async validate() {
    console.log('=== Dataset Validation ===');
    console.log(`File: ${this.inputFile}\n`);
    
    // Load dataset
    const data = await this.loadDataset();
    this.stats.total = data.length;
    
    // Validate each example
    for (let i = 0; i < data.length; i++) {
      this.validateExample(data[i], i);
    }
    
    // Calculate statistics
    this.calculateStats(data);
    
    // Report results
    this.report();
    
    return this.errors.length === 0;
  }
  
  async loadDataset() {
    const content = await fs.readFile(this.inputFile, 'utf8');
    const lines = content.split('\n').filter(line => line.trim());
    
    const data = [];
    for (const line of lines) {
      try {
        data.push(JSON.parse(line));
      } catch (e) {
        this.errors.push(`Invalid JSON at line ${data.length + 1}`);
      }
    }
    
    return data;
  }
  
  validateExample(example, index) {
    // Check structure
    if (!example.messages && !example.prompt && !example.text) {
      this.errors.push(`Example ${index}: Missing required format fields`);
      return;
    }
    
    // OpenAI format validation
    if (example.messages) {
      if (!Array.isArray(example.messages)) {
        this.errors.push(`Example ${index}: messages must be an array`);
        return;
      }
      
      if (example.messages.length !== 3) {
        this.warnings.push(`Example ${index}: Unusual message count (${example.messages.length})`);
      }
      
      const roles = example.messages.map(m => m.role);
      if (!roles.includes('system') || !roles.includes('user') || !roles.includes('assistant')) {
        this.errors.push(`Example ${index}: Missing required roles`);
      }
      
      // Check content quality
      const assistant = example.messages.find(m => m.role === 'assistant');
      if (assistant) {
        this.validateABAPCode(assistant.content, index);
      }
    }
    
    // Anthropic format validation
    if (example.prompt) {
      if (!example.prompt.includes('Human:') || !example.prompt.includes('Assistant:')) {
        this.errors.push(`Example ${index}: Invalid Anthropic format`);
      }
    }
    
    this.stats.valid++;
  }
  
  validateABAPCode(code, index) {
    // Check for modern ABAP patterns
    const modernPatterns = [
      'VALUE #',
      'COND',
      'REDUCE',
      'FILTER',
      'DATA(',
      '=>',
      '|{'
    ];
    
    const hasModernSyntax = modernPatterns.some(pattern => code.includes(pattern));
    
    if (!hasModernSyntax && code.length > 50) {
      this.warnings.push(`Example ${index}: No modern ABAP patterns detected`);
    }
    
    // Check for legacy patterns
    const legacyPatterns = [
      /\bFORM\s+/,
      /\bPERFORM\s+/,
      /\bUSING\s+/,
      /\bCHANGING\s+/,
      /\bOPEN\s+DATASET/,
      /\bREAD\s+DATASET/
    ];
    
    for (const pattern of legacyPatterns) {
      if (pattern.test(code)) {
        this.errors.push(`Example ${index}: Contains legacy ABAP pattern`);
      }
    }
    
    // Check for completeness
    if (code === '<MASK>' || code.includes('<MASK>')) {
      this.errors.push(`Example ${index}: Response contains mask token`);
    }
    
    // Check length
    if (code.length < 5) {
      this.warnings.push(`Example ${index}: Very short response (${code.length} chars)`);
    }
  }
  
  calculateStats(data) {
    let totalPromptLength = 0;
    let totalResponseLength = 0;
    
    for (const example of data) {
      let promptLength = 0;
      let responseLength = 0;
      
      if (example.messages) {
        const user = example.messages.find(m => m.role === 'user');
        const assistant = example.messages.find(m => m.role === 'assistant');
        
        promptLength = user?.content?.length || 0;
        responseLength = assistant?.content?.length || 0;
      } else if (example.prompt) {
        const parts = example.prompt.split('Assistant:');
        promptLength = parts[0].length;
        responseLength = parts[1]?.length || 0;
      } else if (example.text) {
        const parts = example.text.split('### Assistant:');
        promptLength = parts[0].length;
        responseLength = parts[1]?.length || 0;
      }
      
      totalPromptLength += promptLength;
      totalResponseLength += responseLength;
      
      this.stats.minResponseLength = Math.min(this.stats.minResponseLength, responseLength);
      this.stats.maxResponseLength = Math.max(this.stats.maxResponseLength, responseLength);
    }
    
    this.stats.avgPromptLength = Math.round(totalPromptLength / data.length);
    this.stats.avgResponseLength = Math.round(totalResponseLength / data.length);
    this.stats.invalid = this.errors.length;
    this.stats.valid = this.stats.total - this.stats.invalid;
  }
  
  report() {
    console.log('=== Validation Results ===\n');
    
    console.log('Statistics:');
    console.log(`  Total examples: ${this.stats.total}`);
    console.log(`  Valid examples: ${this.stats.valid}`);
    console.log(`  Invalid examples: ${this.stats.invalid}`);
    console.log(`  Avg prompt length: ${this.stats.avgPromptLength} chars`);
    console.log(`  Avg response length: ${this.stats.avgResponseLength} chars`);
    console.log(`  Response length range: ${this.stats.minResponseLength}-${this.stats.maxResponseLength} chars`);
    
    if (this.errors.length > 0) {
      console.log('\n❌ Errors:');
      this.errors.slice(0, 10).forEach(err => console.log(`  - ${err}`));
      if (this.errors.length > 10) {
        console.log(`  ... and ${this.errors.length - 10} more`);
      }
    }
    
    if (this.warnings.length > 0) {
      console.log('\n⚠️  Warnings:');
      this.warnings.slice(0, 10).forEach(warn => console.log(`  - ${warn}`));
      if (this.warnings.length > 10) {
        console.log(`  ... and ${this.warnings.length - 10} more`);
      }
    }
    
    if (this.errors.length === 0) {
      console.log('\n✅ Dataset is valid!');
    } else {
      console.log('\n❌ Dataset has errors that need fixing');
    }
  }
}

/**
 * CLI entry point
 */
async function main() {
  const args = process.argv.slice(2);
  
  if (args.length === 0 || args[0] === '--help') {
    console.log(`
Dataset Validation Tool

Usage: node validate-dataset.js <input-file>

Options:
  --help  Show this help message

Example:
  node validate-dataset.js datasets/fine-tuning/train.jsonl
    `);
    process.exit(0);
  }
  
  const validator = new DatasetValidator(args[0]);
  const isValid = await validator.validate();
  
  process.exit(isValid ? 0 : 1);
}

// Run if called directly
if (require.main === module) {
  main().catch(console.error);
}

module.exports = { DatasetValidator };