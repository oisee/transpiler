#!/usr/bin/env node

/**
 * Fine-tuning Dataset Preparation Tool
 * Converts masked pairs into LLM fine-tuning format with train/val/test splits
 */

const fs = require('fs').promises;
const path = require('path');
const crypto = require('crypto');

class FineTuningPreparer {
  constructor(options = {}) {
    this.inputDir = options.input || 'datasets/abap-masked-pairs';
    this.outputDir = options.output || 'datasets/fine-tuning';
    this.splitRatio = options.split || '80:10:10';
    this.format = options.format || 'openai'; // openai, anthropic, huggingface
  }
  
  async prepare() {
    console.log('=== Fine-tuning Dataset Preparation ===');
    console.log(`Input: ${this.inputDir}`);
    console.log(`Output: ${this.outputDir}`);
    console.log(`Split: ${this.splitRatio}`);
    console.log(`Format: ${this.format}\n`);
    
    // Create output directory
    await fs.mkdir(this.outputDir, { recursive: true });
    
    // Load all masked pairs
    const allPairs = await this.loadAllPairs();
    console.log(`Loaded ${allPairs.length} masked pairs\n`);
    
    // Convert to fine-tuning format
    const formatted = this.formatPairs(allPairs);
    
    // Split dataset
    const splits = this.splitDataset(formatted);
    
    // Write output files
    await this.writeDatasets(splits);
    
    // Write metadata
    await this.writeMetadata(splits);
    
    console.log('\n✓ Fine-tuning dataset prepared successfully');
  }
  
  async loadAllPairs() {
    const pairs = [];
    
    // Recursively read all JSONL files
    async function readDir(dir) {
      const entries = await fs.readdir(dir, { withFileTypes: true });
      
      for (const entry of entries) {
        const fullPath = path.join(dir, entry.name);
        
        if (entry.isDirectory()) {
          await readDir(fullPath);
        } else if (entry.name.endsWith('.jsonl')) {
          const content = await fs.readFile(fullPath, 'utf8');
          const lines = content.split('\n').filter(line => line.trim());
          
          for (const line of lines) {
            try {
              pairs.push(JSON.parse(line));
            } catch (e) {
              console.warn(`Skipping invalid JSON: ${line.substring(0, 50)}...`);
            }
          }
        }
      }
    }
    
    await readDir(this.inputDir);
    return pairs;
  }
  
  formatPairs(pairs) {
    return pairs.map(pair => {
      switch (this.format) {
        case 'openai':
          return this.formatOpenAI(pair);
        case 'anthropic':
          return this.formatAnthropic(pair);
        case 'huggingface':
          return this.formatHuggingFace(pair);
        default:
          return this.formatOpenAI(pair);
      }
    });
  }
  
  formatOpenAI(pair) {
    const systemPrompt = this.getSystemPrompt(pair.level);
    const userPrompt = this.getUserPrompt(pair);
    
    return {
      messages: [
        { role: 'system', content: systemPrompt },
        { role: 'user', content: userPrompt },
        { role: 'assistant', content: pair.target }
      ]
    };
  }
  
  formatAnthropic(pair) {
    const systemPrompt = this.getSystemPrompt(pair.level);
    const userPrompt = this.getUserPrompt(pair);
    
    return {
      prompt: `Human: ${systemPrompt}\n\n${userPrompt}\n\nAssistant: ${pair.target}`,
      metadata: {
        level: pair.level,
        task: pair.task,
        id: pair.id
      }
    };
  }
  
  formatHuggingFace(pair) {
    const systemPrompt = this.getSystemPrompt(pair.level);
    const userPrompt = this.getUserPrompt(pair);
    
    return {
      text: `### System: ${systemPrompt}\n\n### User: ${userPrompt}\n\n### Assistant: ${pair.target}`,
      level: pair.level,
      task: pair.task
    };
  }
  
  getSystemPrompt(level) {
    const prompts = {
      1: 'You are an expert ABAP developer. Complete the masked expressions following modern ABAP 7.40+ best practices. Focus on VALUE #(), COND #(), REDUCE, and inline declarations.',
      2: 'You are an expert ABAP developer. Complete the masked statements following modern ABAP 7.40+ patterns. Use functional methods, inline declarations, and expression-based syntax.',
      3: 'You are an expert ABAP developer. Implement the masked code blocks following modern ABAP principles. Write clean, efficient, and maintainable code.',
      4: 'You are an expert ABAP developer. Complete the class or interface structure following ABAP OO best practices. Use proper encapsulation, clear naming, and modern syntax.'
    };
    
    return prompts[level] || prompts[1];
  }
  
  getUserPrompt(pair) {
    if (pair.level === 1) {
      // Expression level - provide context
      const context = pair.context || {};
      let prompt = `Complete the following ABAP expression:\n\n${pair.masked}`;
      
      if (context.method) {
        prompt += `\n\nContext: Method ${context.method}`;
      }
      if (context.class) {
        prompt += ` in class ${context.class}`;
      }
      
      return prompt;
    }
    
    if (pair.level === 2) {
      // Statement level
      const context = pair.context || {};
      let prompt = `Complete this ABAP statement`;
      
      if (context.pattern) {
        prompt += ` (${context.pattern} pattern)`;
      }
      prompt += `:\n\n${pair.masked}`;
      
      return prompt;
    }
    
    if (pair.level === 3) {
      // Block level
      let prompt = `Implement the following ABAP ${pair.node?.type || 'block'}`;
      
      if (pair.signature) {
        prompt += ' with signature:\n';
        if (pair.signature.importing?.length) {
          prompt += `IMPORTING: ${pair.signature.importing.map(p => `${p.name} TYPE ${p.type}`).join(', ')}\n`;
        }
        if (pair.signature.returning) {
          prompt += `RETURNING: VALUE(${pair.signature.returning.name}) TYPE ${pair.signature.returning.type}\n`;
        }
      }
      
      prompt += `\n${pair.masked}`;
      return prompt;
    }
    
    if (pair.level === 4) {
      // Structure level
      return pair.prompt || `Complete the following ABAP structure:\n\n${pair.masked}`;
    }
    
    return `Complete: ${pair.masked}`;
  }
  
  splitDataset(data) {
    // Shuffle data
    const shuffled = this.shuffle([...data]);
    
    // Parse split ratio
    const ratios = this.splitRatio.split(':').map(Number);
    const total = ratios.reduce((a, b) => a + b, 0);
    
    const trainSize = Math.floor(shuffled.length * ratios[0] / total);
    const valSize = Math.floor(shuffled.length * ratios[1] / total);
    
    return {
      train: shuffled.slice(0, trainSize),
      validation: shuffled.slice(trainSize, trainSize + valSize),
      test: shuffled.slice(trainSize + valSize)
    };
  }
  
  shuffle(array) {
    const arr = [...array];
    for (let i = arr.length - 1; i > 0; i--) {
      const j = Math.floor(Math.random() * (i + 1));
      [arr[i], arr[j]] = [arr[j], arr[i]];
    }
    return arr;
  }
  
  async writeDatasets(splits) {
    for (const [name, data] of Object.entries(splits)) {
      const filename = path.join(this.outputDir, `${name}.jsonl`);
      const lines = data.map(item => JSON.stringify(item));
      await fs.writeFile(filename, lines.join('\n'));
      console.log(`Wrote ${data.length} examples to ${name}.jsonl`);
    }
  }
  
  async writeMetadata(splits) {
    const metadata = {
      timestamp: new Date().toISOString(),
      format: this.format,
      splitRatio: this.splitRatio,
      statistics: {
        train: splits.train.length,
        validation: splits.validation.length,
        test: splits.test.length,
        total: splits.train.length + splits.validation.length + splits.test.length
      },
      levelDistribution: this.analyzeLevels(splits),
      sourceDirectory: this.inputDir,
      outputDirectory: this.outputDir
    };
    
    await fs.writeFile(
      path.join(this.outputDir, 'metadata.json'),
      JSON.stringify(metadata, null, 2)
    );
    
    console.log('\n=== Dataset Statistics ===');
    console.log(`Total examples: ${metadata.statistics.total}`);
    console.log(`Training set: ${metadata.statistics.train}`);
    console.log(`Validation set: ${metadata.statistics.validation}`);
    console.log(`Test set: ${metadata.statistics.test}`);
  }
  
  analyzeLevels(splits) {
    const analysis = {};
    
    // For HuggingFace format which preserves level info
    if (this.format === 'huggingface') {
      for (const [split, data] of Object.entries(splits)) {
        analysis[split] = {
          level1: data.filter(d => d.level === 1).length,
          level2: data.filter(d => d.level === 2).length,
          level3: data.filter(d => d.level === 3).length,
          level4: data.filter(d => d.level === 4).length
        };
      }
    }
    
    return analysis;
  }
}

/**
 * CLI entry point
 */
async function main() {
  const args = process.argv.slice(2);
  
  const options = {
    input: 'datasets/abap-masked-pairs',
    output: 'datasets/fine-tuning',
    split: '80:10:10',
    format: 'openai'
  };
  
  for (let i = 0; i < args.length; i++) {
    switch (args[i]) {
      case '--input':
        options.input = args[++i];
        break;
      case '--output':
        options.output = args[++i];
        break;
      case '--split':
        options.split = args[++i];
        break;
      case '--format':
        options.format = args[++i];
        break;
      case '--help':
        console.log(`
Fine-tuning Dataset Preparation Tool

Usage: node prepare-fine-tuning.js [options]

Options:
  --input <dir>    Input directory with masked pairs (default: datasets/abap-masked-pairs)
  --output <dir>   Output directory (default: datasets/fine-tuning)
  --split <ratio>  Train:Val:Test ratio (default: 80:10:10)
  --format <type>  Output format: openai, anthropic, huggingface (default: openai)
  --help          Show this help message

Example:
  node prepare-fine-tuning.js --format openai --split 80:10:10
        `);
        process.exit(0);
    }
  }
  
  const preparer = new FineTuningPreparer(options);
  await preparer.prepare();
}

// Run if called directly
if (require.main === module) {
  main().catch(console.error);
}

module.exports = { FineTuningPreparer };