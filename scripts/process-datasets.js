#!/usr/bin/env node

/**
 * Dataset Processor for Universal Code-to-ABAP Translation
 * 
 * Processes downloaded source code datasets and converts them into
 * training-ready format using the universal translator and masking strategies.
 * 
 * Features:
 * - Batch processing of large datasets
 * - Parallel processing with configurable concurrency
 * - Progress tracking and resumption capability
 * - Quality filtering and validation
 * - Multiple output formats
 * - Memory management for large datasets
 */

import fs from 'fs-extra';
import path from 'path';
import chalk from 'chalk';
import { Command } from 'commander';
import { glob } from 'glob';
import pLimit from 'p-limit';
import { createWriteStream } from 'fs';
import { Transform } from 'stream';
import { pipeline } from 'stream/promises';

import { UniversalTranslator } from '../universal-translator.js';
import { UniversalDatasetGenerator } from '../dataset-generator.js';

// ============================================
// Configuration and Constants
// ============================================

const DEFAULT_CONFIG = {
  concurrency: 5,
  batchSize: 100,
  qualityThreshold: 60,
  maxFileSize: 1024 * 1024, // 1MB
  enableValidation: true,
  resumeFromCheckpoint: true,
  outputFormat: 'jsonl',
  logLevel: 'info'
};

const SUPPORTED_EXTENSIONS = {
  javascript: ['.js'],
  python: ['.py'],
  go: ['.go'],
  java: ['.java'],
  typescript: ['.ts'],
  c: ['.c'],
  cpp: ['.cpp', '.cc', '.cxx'],
  rust: ['.rs'],
  ruby: ['.rb']
};

// ============================================
// Progress Tracking
// ============================================

class ProgressTracker {
  constructor(total, options = {}) {
    this.total = total;
    this.processed = 0;
    this.successful = 0;
    this.failed = 0;
    this.skipped = 0;
    this.startTime = Date.now();
    this.lastUpdate = Date.now();
    this.updateInterval = options.updateInterval || 1000; // 1 second
    this.showProgress = options.showProgress !== false;
  }

  update(type = 'processed') {
    this[type]++;
    this.processed = this.successful + this.failed + this.skipped;
    
    const now = Date.now();
    if (now - this.lastUpdate >= this.updateInterval && this.showProgress) {
      this.display();
      this.lastUpdate = now;
    }
  }

  display() {
    const percentage = (this.processed / this.total * 100).toFixed(1);
    const elapsed = (Date.now() - this.startTime) / 1000;
    const rate = this.processed / elapsed;
    const eta = this.total > this.processed ? (this.total - this.processed) / rate : 0;
    
    console.log(
      `Progress: ${this.processed}/${this.total} (${percentage}%) | ` +
      `Success: ${this.successful} | Failed: ${this.failed} | Skipped: ${this.skipped} | ` +
      `Rate: ${rate.toFixed(1)}/s | ETA: ${this.formatTime(eta)}`
    );
  }

  displayFinal() {
    const elapsed = (Date.now() - this.startTime) / 1000;
    const rate = this.processed / elapsed;
    
    console.log(chalk.green('\n=== Processing Complete ==='));
    console.log(`Total processed: ${this.processed}`);
    console.log(`Successful: ${this.successful}`);
    console.log(`Failed: ${this.failed}`);
    console.log(`Skipped: ${this.skipped}`);
    console.log(`Total time: ${this.formatTime(elapsed)}`);
    console.log(`Average rate: ${rate.toFixed(1)} files/second`);
    console.log(`Success rate: ${(this.successful / this.processed * 100).toFixed(1)}%`);
  }

  formatTime(seconds) {
    const hours = Math.floor(seconds / 3600);
    const minutes = Math.floor((seconds % 3600) / 60);
    const secs = Math.floor(seconds % 60);
    
    if (hours > 0) {
      return `${hours}h ${minutes}m ${secs}s`;
    } else if (minutes > 0) {
      return `${minutes}m ${secs}s`;
    } else {
      return `${secs}s`;
    }
  }

  getStats() {
    return {
      total: this.total,
      processed: this.processed,
      successful: this.successful,
      failed: this.failed,
      skipped: this.skipped,
      successRate: this.processed > 0 ? this.successful / this.processed : 0,
      elapsedTime: Date.now() - this.startTime
    };
  }
}

// ============================================
// Checkpoint Management
// ============================================

class CheckpointManager {
  constructor(checkpointPath) {
    this.checkpointPath = checkpointPath;
    this.checkpoint = this.loadCheckpoint();
  }

  loadCheckpoint() {
    try {
      if (fs.existsSync(this.checkpointPath)) {
        const data = fs.readJsonSync(this.checkpointPath);
        console.log(chalk.blue(`Loaded checkpoint: ${data.processedFiles.size} files already processed`));
        return {
          processedFiles: new Set(data.processedFiles || []),
          stats: data.stats || {},
          timestamp: data.timestamp
        };
      }
    } catch (error) {
      console.warn(chalk.yellow(`Failed to load checkpoint: ${error.message}`));
    }
    
    return {
      processedFiles: new Set(),
      stats: {},
      timestamp: Date.now()
    };
  }

  saveCheckpoint(stats = {}) {
    try {
      const data = {
        processedFiles: Array.from(this.checkpoint.processedFiles),
        stats,
        timestamp: Date.now()
      };
      
      fs.writeJsonSync(this.checkpointPath, data, { spaces: 2 });
    } catch (error) {
      console.error(chalk.red(`Failed to save checkpoint: ${error.message}`));
    }
  }

  isProcessed(filePath) {
    return this.checkpoint.processedFiles.has(filePath);
  }

  markProcessed(filePath) {
    this.checkpoint.processedFiles.add(filePath);
  }

  clear() {
    this.checkpoint = {
      processedFiles: new Set(),
      stats: {},
      timestamp: Date.now()
    };
    
    try {
      fs.removeSync(this.checkpointPath);
    } catch (error) {
      // Ignore errors when clearing
    }
  }
}

// ============================================
// File Processing Pipeline
// ============================================

class DatasetProcessor {
  constructor(config = {}) {
    this.config = { ...DEFAULT_CONFIG, ...config };
    this.translator = new UniversalTranslator({
      enableCaching: true,
      validateOutput: this.config.enableValidation
    });
    this.datasetGenerator = new UniversalDatasetGenerator({
      validateQuality: this.config.enableValidation,
      qualityThreshold: this.config.qualityThreshold,
      parallelProcessing: false // We handle concurrency at file level
    });
    
    this.stats = {
      totalFiles: 0,
      processedFiles: 0,
      generatedItems: 0,
      validItems: 0,
      errors: []
    };
  }

  async processDatasets(inputDir, outputPath, options = {}) {
    console.log(chalk.blue('Universal Code-to-ABAP Dataset Processor'));
    console.log(chalk.gray('Processing downloaded source code datasets...'));
    console.log();

    const config = { ...this.config, ...options };
    
    // Setup checkpoint management
    const checkpointPath = path.join(path.dirname(outputPath), '.processing_checkpoint.json');
    const checkpoint = new CheckpointManager(checkpointPath);
    
    try {
      // Discover source files
      console.log(chalk.blue('Discovering source files...'));
      const files = await this.discoverFiles(inputDir, config);
      
      if (files.length === 0) {
        console.log(chalk.yellow('No source files found to process'));
        return { success: false, reason: 'No files found' };
      }

      // Filter already processed files if resuming
      const filesToProcess = config.resumeFromCheckpoint ? 
        files.filter(f => !checkpoint.isProcessed(f.path)) : files;
      
      console.log(chalk.green(`Found ${files.length} source files`));
      if (filesToProcess.length !== files.length) {
        console.log(chalk.blue(`Resuming: ${filesToProcess.length} files remaining to process`));
      }
      
      this.stats.totalFiles = filesToProcess.length;

      // Setup progress tracking
      const progress = new ProgressTracker(filesToProcess.length);
      
      // Setup output streams
      const outputStreams = await this.setupOutputStreams(outputPath, config);
      
      // Process files in batches
      await this.processBatches(filesToProcess, outputStreams, config, progress, checkpoint);
      
      // Finalize output
      await this.finalizeOutput(outputStreams, config);
      
      // Display final statistics
      progress.displayFinal();
      
      // Save final checkpoint
      checkpoint.saveCheckpoint(this.stats);
      
      console.log(chalk.green('✓ Dataset processing completed successfully!'));
      
      return {
        success: true,
        stats: {
          ...this.stats,
          ...progress.getStats()
        },
        outputPath
      };
      
    } catch (error) {
      console.error(chalk.red('✗ Dataset processing failed:'), error.message);
      console.error(error.stack);
      
      // Save checkpoint on error
      checkpoint.saveCheckpoint(this.stats);
      
      return {
        success: false,
        error: error.message,
        stats: this.stats
      };
    }
  }

  async discoverFiles(inputDir, config) {
    const files = [];
    
    // Get all supported file extensions
    const allExtensions = Object.values(SUPPORTED_EXTENSIONS).flat();
    const patterns = allExtensions.map(ext => `**/*${ext}`);
    
    for (const pattern of patterns) {
      const matches = await glob(pattern, {
        cwd: inputDir,
        ignore: [
          '**/node_modules/**',
          '**/.git/**',
          '**/target/**',
          '**/build/**',
          '**/dist/**',
          '**/__pycache__/**',
          '**/test/**',
          '**/tests/**',
          '**/spec/**'
        ],
        absolute: true
      });
      
      files.push(...matches);
    }
    
    // Remove duplicates and add metadata
    const uniqueFiles = [...new Set(files)];
    const fileInfos = [];
    
    for (const filePath of uniqueFiles) {
      try {
        const stats = await fs.stat(filePath);
        
        // Skip files that are too large
        if (stats.size > config.maxFileSize) {
          continue;
        }
        
        const language = this.detectLanguage(filePath);
        if (language) {
          fileInfos.push({
            path: filePath,
            language,
            size: stats.size,
            relativePath: path.relative(inputDir, filePath)
          });
        }
      } catch (error) {
        // Skip files we can't read
        continue;
      }
    }
    
    return fileInfos;
  }

  detectLanguage(filePath) {
    const ext = path.extname(filePath).toLowerCase();
    
    for (const [language, extensions] of Object.entries(SUPPORTED_EXTENSIONS)) {
      if (extensions.includes(ext)) {
        return language;
      }
    }
    
    return null;
  }

  async setupOutputStreams(outputPath, config) {
    const outputDir = path.dirname(outputPath);
    await fs.ensureDir(outputDir);
    
    const streams = {
      main: createWriteStream(outputPath, { flags: 'w' }),
      errors: createWriteStream(outputPath.replace(/\.[^.]+$/, '_errors.jsonl'), { flags: 'w' }),
      stats: createWriteStream(outputPath.replace(/\.[^.]+$/, '_stats.jsonl'), { flags: 'w' })
    };
    
    return streams;
  }

  async processBatches(files, outputStreams, config, progress, checkpoint) {
    const limit = pLimit(config.concurrency);
    const batchSize = config.batchSize;
    
    for (let i = 0; i < files.length; i += batchSize) {
      const batch = files.slice(i, Math.min(i + batchSize, files.length));
      
      console.log(chalk.blue(`Processing batch ${Math.floor(i / batchSize) + 1}/${Math.ceil(files.length / batchSize)}`));
      
      // Process batch with concurrency limit
      const promises = batch.map(file => 
        limit(() => this.processFile(file, outputStreams, config, progress, checkpoint))
      );
      
      await Promise.allSettled(promises);
      
      // Periodic checkpoint save
      if (i % (batchSize * 5) === 0) {
        checkpoint.saveCheckpoint(this.stats);
      }
    }
  }

  async processFile(fileInfo, outputStreams, config, progress, checkpoint) {
    try {
      // Read source code
      const sourceCode = await fs.readFile(fileInfo.path, 'utf8');
      
      // Skip empty files
      if (!sourceCode.trim()) {
        progress.update('skipped');
        checkpoint.markProcessed(fileInfo.path);
        return;
      }
      
      // Generate training items using dataset generator
      const inputSources = [{
        code: sourceCode,
        language: fileInfo.language,
        metadata: {
          sourceFile: fileInfo.relativePath,
          fileSize: fileInfo.size,
          processedAt: new Date().toISOString()
        }
      }];
      
      // Process through dataset generator
      const datasetItems = await this.datasetGenerator.processCode(
        sourceCode, 
        fileInfo.language, 
        inputSources[0].metadata
      );
      
      // Write items to output
      for (const item of datasetItems) {
        this.stats.generatedItems++;
        
        if (item.quality && item.quality.valid) {
          this.stats.validItems++;
        }
        
        // Write to main output
        outputStreams.main.write(JSON.stringify(item) + '\n');
        
        // Write statistics
        const statEntry = {
          file: fileInfo.relativePath,
          language: fileInfo.language,
          strategy: item.metadata?.strategy,
          level: item.metadata?.level,
          quality: item.quality?.overall || 0,
          valid: item.quality?.valid || false,
          timestamp: new Date().toISOString()
        };
        
        outputStreams.stats.write(JSON.stringify(statEntry) + '\n');
      }
      
      progress.update('successful');
      checkpoint.markProcessed(fileInfo.path);
      this.stats.processedFiles++;
      
    } catch (error) {
      // Log error
      const errorEntry = {
        file: fileInfo.relativePath,
        language: fileInfo.language,
        error: error.message,
        timestamp: new Date().toISOString()
      };
      
      outputStreams.errors.write(JSON.stringify(errorEntry) + '\n');
      
      this.stats.errors.push({
        file: fileInfo.path,
        error: error.message
      });
      
      progress.update('failed');
      checkpoint.markProcessed(fileInfo.path); // Mark as processed to avoid retry
    }
  }

  async finalizeOutput(outputStreams, config) {
    // Close all streams
    for (const stream of Object.values(outputStreams)) {
      stream.end();
      
      // Wait for stream to finish
      await new Promise((resolve, reject) => {
        stream.on('finish', resolve);
        stream.on('error', reject);
      });
    }
  }
}

// ============================================
// Analysis and Reporting
// ============================================

class DatasetAnalyzer {
  constructor() {
    this.stats = {};
  }

  async analyzeDataset(datasetPath) {
    console.log(chalk.blue('Analyzing generated dataset...'));
    
    try {
      const analysis = {
        overview: await this.getOverview(datasetPath),
        byLanguage: await this.analyzeByLanguage(datasetPath),
        byStrategy: await this.analyzeByStrategy(datasetPath),
        qualityDistribution: await this.analyzeQuality(datasetPath),
        sizeDistribution: await this.analyzeSizes(datasetPath)
      };
      
      this.displayAnalysis(analysis);
      return analysis;
      
    } catch (error) {
      console.error(chalk.red('Analysis failed:'), error.message);
      return null;
    }
  }

  async getOverview(datasetPath) {
    const stats = await fs.stat(datasetPath);
    
    // Count lines in dataset
    const content = await fs.readFile(datasetPath, 'utf8');
    const lines = content.split('\n').filter(line => line.trim());
    
    return {
      fileSize: stats.size,
      totalItems: lines.length,
      created: stats.birthtime,
      modified: stats.mtime
    };
  }

  async analyzeByLanguage(datasetPath) {
    const languageStats = {};
    
    const content = await fs.readFile(datasetPath, 'utf8');
    const lines = content.split('\n').filter(line => line.trim());
    
    for (const line of lines) {
      try {
        const item = JSON.parse(line);
        const lang = item.sourceLanguage;
        
        if (!languageStats[lang]) {
          languageStats[lang] = {
            count: 0,
            validCount: 0,
            averageQuality: 0,
            totalQuality: 0
          };
        }
        
        languageStats[lang].count++;
        
        if (item.quality?.valid) {
          languageStats[lang].validCount++;
        }
        
        if (item.quality?.overall) {
          languageStats[lang].totalQuality += item.quality.overall;
        }
      } catch (error) {
        // Skip invalid JSON lines
        continue;
      }
    }
    
    // Calculate averages
    for (const stats of Object.values(languageStats)) {
      stats.averageQuality = stats.count > 0 ? stats.totalQuality / stats.count : 0;
      stats.validRate = stats.count > 0 ? stats.validCount / stats.count : 0;
    }
    
    return languageStats;
  }

  async analyzeByStrategy(datasetPath) {
    const strategyStats = {};
    
    const content = await fs.readFile(datasetPath, 'utf8');
    const lines = content.split('\n').filter(line => line.trim());
    
    for (const line of lines) {
      try {
        const item = JSON.parse(line);
        const strategy = item.metadata?.strategy || 'unknown';
        const level = item.metadata?.level || 0;
        
        const key = `${strategy}_level_${level}`;
        
        if (!strategyStats[key]) {
          strategyStats[key] = {
            strategy,
            level,
            count: 0,
            validCount: 0,
            averageQuality: 0,
            totalQuality: 0
          };
        }
        
        strategyStats[key].count++;
        
        if (item.quality?.valid) {
          strategyStats[key].validCount++;
        }
        
        if (item.quality?.overall) {
          strategyStats[key].totalQuality += item.quality.overall;
        }
      } catch (error) {
        continue;
      }
    }
    
    // Calculate averages
    for (const stats of Object.values(strategyStats)) {
      stats.averageQuality = stats.count > 0 ? stats.totalQuality / stats.count : 0;
      stats.validRate = stats.count > 0 ? stats.validCount / stats.count : 0;
    }
    
    return strategyStats;
  }

  async analyzeQuality(datasetPath) {
    const qualityBuckets = {
      excellent: 0, // 90-100
      good: 0,      // 70-89
      fair: 0,      // 50-69
      poor: 0       // 0-49
    };
    
    const content = await fs.readFile(datasetPath, 'utf8');
    const lines = content.split('\n').filter(line => line.trim());
    
    for (const line of lines) {
      try {
        const item = JSON.parse(line);
        const quality = item.quality?.overall || 0;
        
        if (quality >= 90) {
          qualityBuckets.excellent++;
        } else if (quality >= 70) {
          qualityBuckets.good++;
        } else if (quality >= 50) {
          qualityBuckets.fair++;
        } else {
          qualityBuckets.poor++;
        }
      } catch (error) {
        continue;
      }
    }
    
    return qualityBuckets;
  }

  async analyzeSizes(datasetPath) {
    const sizeStats = {
      originalCode: { min: Infinity, max: 0, total: 0, count: 0 },
      maskedCode: { min: Infinity, max: 0, total: 0, count: 0 },
      abapCode: { min: Infinity, max: 0, total: 0, count: 0 }
    };
    
    const content = await fs.readFile(datasetPath, 'utf8');
    const lines = content.split('\n').filter(line => line.trim());
    
    for (const line of lines) {
      try {
        const item = JSON.parse(line);
        
        if (item.originalCode) {
          const len = item.originalCode.length;
          sizeStats.originalCode.min = Math.min(sizeStats.originalCode.min, len);
          sizeStats.originalCode.max = Math.max(sizeStats.originalCode.max, len);
          sizeStats.originalCode.total += len;
          sizeStats.originalCode.count++;
        }
        
        if (item.maskedCode) {
          const len = item.maskedCode.length;
          sizeStats.maskedCode.min = Math.min(sizeStats.maskedCode.min, len);
          sizeStats.maskedCode.max = Math.max(sizeStats.maskedCode.max, len);
          sizeStats.maskedCode.total += len;
          sizeStats.maskedCode.count++;
        }
        
        if (item.abapCode) {
          const len = item.abapCode.length;
          sizeStats.abapCode.min = Math.min(sizeStats.abapCode.min, len);
          sizeStats.abapCode.max = Math.max(sizeStats.abapCode.max, len);
          sizeStats.abapCode.total += len;
          sizeStats.abapCode.count++;
        }
      } catch (error) {
        continue;
      }
    }
    
    // Calculate averages
    for (const stats of Object.values(sizeStats)) {
      stats.average = stats.count > 0 ? stats.total / stats.count : 0;
      if (stats.min === Infinity) stats.min = 0;
    }
    
    return sizeStats;
  }

  displayAnalysis(analysis) {
    console.log(chalk.green('\n=== Dataset Analysis ==='));
    
    // Overview
    console.log(chalk.blue('\nOverview:'));
    console.log(`Total items: ${analysis.overview.totalItems}`);
    console.log(`File size: ${(analysis.overview.fileSize / 1024 / 1024).toFixed(2)} MB`);
    console.log(`Created: ${analysis.overview.created}`);
    
    // By language
    console.log(chalk.blue('\nBy Language:'));
    for (const [lang, stats] of Object.entries(analysis.byLanguage)) {
      console.log(
        `${lang.padEnd(12)}: ${stats.count.toString().padStart(6)} items | ` +
        `Valid: ${(stats.validRate * 100).toFixed(1)}% | ` +
        `Quality: ${stats.averageQuality.toFixed(1)}`
      );
    }
    
    // By strategy
    console.log(chalk.blue('\nBy Strategy:'));
    for (const [key, stats] of Object.entries(analysis.byStrategy)) {
      console.log(
        `${key.padEnd(20)}: ${stats.count.toString().padStart(5)} items | ` +
        `Valid: ${(stats.validRate * 100).toFixed(1)}% | ` +
        `Quality: ${stats.averageQuality.toFixed(1)}`
      );
    }
    
    // Quality distribution
    console.log(chalk.blue('\nQuality Distribution:'));
    const total = Object.values(analysis.qualityDistribution).reduce((a, b) => a + b, 0);
    for (const [bucket, count] of Object.entries(analysis.qualityDistribution)) {
      const percentage = total > 0 ? (count / total * 100).toFixed(1) : 0;
      console.log(`${bucket.padEnd(10)}: ${count.toString().padStart(6)} (${percentage}%)`);
    }
    
    // Size statistics
    console.log(chalk.blue('\nSize Statistics (characters):'));
    for (const [type, stats] of Object.entries(analysis.sizeDistribution)) {
      console.log(
        `${type.padEnd(12)}: Avg: ${stats.average.toFixed(0).padStart(5)} | ` +
        `Min: ${stats.min.toString().padStart(5)} | ` +
        `Max: ${stats.max.toString().padStart(6)}`
      );
    }
  }

  async saveAnalysis(analysis, outputPath) {
    const reportPath = outputPath.replace(/\.[^.]+$/, '_analysis.json');
    await fs.writeJson(reportPath, analysis, { spaces: 2 });
    console.log(chalk.green(`Analysis saved to: ${reportPath}`));
  }
}

// ============================================
// CLI Interface
// ============================================

function createCLI() {
  const program = new Command();
  
  program
    .name('process-datasets')
    .description('Process downloaded datasets for ABAP translation training')
    .version('1.0.0');

  program
    .command('process')
    .description('Process downloaded source code into training dataset')
    .argument('<input-dir>', 'Directory containing downloaded source code')
    .argument('<output-file>', 'Output JSONL file path')
    .option('-c, --concurrency <n>', 'Number of concurrent file processors', '5')
    .option('-b, --batch-size <n>', 'Batch size for processing', '100')
    .option('-q, --quality-threshold <n>', 'Minimum quality threshold (0-100)', '60')
    .option('--max-file-size <n>', 'Maximum file size in bytes', '1048576')
    .option('--no-validation', 'Skip quality validation')
    .option('--no-resume', 'Don\'t resume from checkpoint')
    .option('--clear-checkpoint', 'Clear existing checkpoint')
    .option('--log-level <level>', 'Log level (debug, info, warn, error)', 'info')
    .action(async (inputDir, outputFile, options) => {
      const config = {
        concurrency: parseInt(options.concurrency),
        batchSize: parseInt(options.batchSize),
        qualityThreshold: parseInt(options.qualityThreshold),
        maxFileSize: parseInt(options.maxFileSize),
        enableValidation: options.validation !== false,
        resumeFromCheckpoint: options.resume !== false,
        logLevel: options.logLevel
      };
      
      // Clear checkpoint if requested
      if (options.clearCheckpoint) {
        const checkpointPath = path.join(path.dirname(outputFile), '.processing_checkpoint.json');
        try {
          fs.removeSync(checkpointPath);
          console.log(chalk.blue('Checkpoint cleared'));
        } catch (error) {
          // Ignore errors
        }
      }
      
      const processor = new DatasetProcessor(config);
      
      try {
        const result = await processor.processDatasets(inputDir, outputFile, config);
        
        if (result.success) {
          console.log(chalk.green('✓ Processing completed successfully!'));
          console.log(`Generated: ${result.stats.generatedItems} items`);
          console.log(`Valid: ${result.stats.validItems} items`);
          console.log(`Output: ${result.outputPath}`);
          
          // Run analysis
          const analyzer = new DatasetAnalyzer();
          const analysis = await analyzer.analyzeDataset(result.outputPath);
          
          if (analysis) {
            await analyzer.saveAnalysis(analysis, result.outputPath);
          }
          
        } else {
          console.error(chalk.red('✗ Processing failed:'), result.error || result.reason);
          process.exit(1);
        }
      } catch (error) {
        console.error(chalk.red('Fatal error:'), error.message);
        process.exit(1);
      }
    });

  program
    .command('analyze')
    .description('Analyze an existing dataset')
    .argument('<dataset-file>', 'Path to JSONL dataset file')
    .option('-o, --output <file>', 'Save analysis report to file')
    .action(async (datasetFile, options) => {
      const analyzer = new DatasetAnalyzer();
      
      try {
        const analysis = await analyzer.analyzeDataset(datasetFile);
        
        if (analysis && options.output) {
          await analyzer.saveAnalysis(analysis, options.output);
        }
        
      } catch (error) {
        console.error(chalk.red('Analysis failed:'), error.message);
        process.exit(1);
      }
    });

  program
    .command('validate')
    .description('Validate dataset quality')
    .argument('<dataset-file>', 'Path to JSONL dataset file')
    .option('--sample-size <n>', 'Number of items to validate (0 = all)', '1000')
    .option('--quality-threshold <n>', 'Quality threshold for validation', '60')
    .action(async (datasetFile, options) => {
      console.log(chalk.blue('Dataset Quality Validation'));
      console.log(`Dataset: ${datasetFile}`);
      console.log(`Sample size: ${options.sampleSize === '0' ? 'All' : options.sampleSize}`);
      console.log(`Quality threshold: ${options.qualityThreshold}%`);
      
      // This would implement detailed quality validation
      // For now, just show placeholder
      console.log(chalk.yellow('Quality validation not yet implemented'));
      console.log('Use the analyze command for basic quality metrics');
    });

  program
    .command('stats')
    .description('Show processing statistics')
    .argument('<checkpoint-file>', 'Path to checkpoint file')
    .action(async (checkpointFile) => {
      try {
        const checkpoint = fs.readJsonSync(checkpointFile);
        
        console.log(chalk.blue('Processing Statistics'));
        console.log(`Processed files: ${checkpoint.processedFiles.length}`);
        console.log(`Last updated: ${new Date(checkpoint.timestamp)}`);
        
        if (checkpoint.stats) {
          console.log(`Generated items: ${checkpoint.stats.generatedItems || 0}`);
          console.log(`Valid items: ${checkpoint.stats.validItems || 0}`);
          console.log(`Errors: ${checkpoint.stats.errors?.length || 0}`);
        }
        
      } catch (error) {
        console.error(chalk.red('Failed to read checkpoint:'), error.message);
      }
    });

  return program;
}

// ============================================
// Main Entry Point
// ============================================

async function main() {
  console.log(chalk.blue.bold('Universal Code-to-ABAP Dataset Processor'));
  console.log(chalk.gray('Converting source code datasets to training format'));
  console.log();

  if (process.argv.length <= 2) {
    const program = createCLI();
    program.help();
  } else {
    const program = createCLI();
    await program.parseAsync(process.argv);
  }
}

// Handle unhandled promises
process.on('unhandledRejection', (reason, promise) => {
  console.error(chalk.red('Unhandled promise rejection:'), reason);
  process.exit(1);
});

// Handle SIGINT for graceful shutdown
process.on('SIGINT', () => {
  console.log(chalk.yellow('\nReceived SIGINT, shutting down gracefully...'));
  process.exit(0);
});

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch(error => {
    console.error(chalk.red('Fatal error:'), error.message);
    console.error(error.stack);
    process.exit(1);
  });
}

export { DatasetProcessor, DatasetAnalyzer };