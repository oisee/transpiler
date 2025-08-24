#!/usr/bin/env node

/**
 * Translation Validation Script
 * 
 * Validates the quality of code-to-ABAP translations through:
 * - Syntax validation of generated ABAP
 * - Round-trip testing (ABAP → AST → ABAP)
 * - Semantic equivalence checking
 * - Performance benchmarking
 * - Integration with existing 60% AST equivalence infrastructure
 */

import fs from 'fs-extra';
import path from 'path';
import chalk from 'chalk';
import { Command } from 'commander';
import pLimit from 'p-limit';
import { performance } from 'perf_hooks';
import { createRequire } from 'module';

import { UniversalTranslator } from '../universal-translator.js';
import { DatasetQualityValidator } from '../dataset-generator.js';

// Import existing round-trip testing infrastructure
const require = createRequire(import.meta.url);
const { RoundTripTester, ABAPParser, ABAPPrettyPrinter, ASTEquivalenceChecker } = require('../bidirectional-transformer.js');

// ============================================
// Validation Metrics and Scoring
// ============================================

class ValidationMetrics {
  constructor() {
    this.metrics = {
      syntaxValidation: {
        passed: 0,
        failed: 0,
        errors: []
      },
      roundTripTesting: {
        passed: 0,
        failed: 0,
        equivalenceScores: []
      },
      semanticEquivalence: {
        passed: 0,
        failed: 0,
        scores: []
      },
      performance: {
        translationTimes: [],
        validationTimes: [],
        memoryUsage: []
      },
      qualityScores: {
        overall: [],
        byStrategy: {},
        byLanguage: {}
      }
    };
  }

  addSyntaxValidation(passed, error = null) {
    if (passed) {
      this.metrics.syntaxValidation.passed++;
    } else {
      this.metrics.syntaxValidation.failed++;
      if (error) {
        this.metrics.syntaxValidation.errors.push(error);
      }
    }
  }

  addRoundTripTest(passed, equivalenceScore = 0) {
    if (passed) {
      this.metrics.roundTripTesting.passed++;
    } else {
      this.metrics.roundTripTesting.failed++;
    }
    
    if (equivalenceScore > 0) {
      this.metrics.roundTripTesting.equivalenceScores.push(equivalenceScore);
    }
  }

  addSemanticEquivalence(score) {
    this.metrics.semanticEquivalence.scores.push(score);
    
    if (score >= 0.6) { // 60% threshold
      this.metrics.semanticEquivalence.passed++;
    } else {
      this.metrics.semanticEquivalence.failed++;
    }
  }

  addPerformanceData(translationTime, validationTime, memoryUsed) {
    if (translationTime) this.metrics.performance.translationTimes.push(translationTime);
    if (validationTime) this.metrics.performance.validationTimes.push(validationTime);
    if (memoryUsed) this.metrics.performance.memoryUsage.push(memoryUsed);
  }

  addQualityScore(score, strategy = null, language = null) {
    this.metrics.qualityScores.overall.push(score);
    
    if (strategy) {
      if (!this.metrics.qualityScores.byStrategy[strategy]) {
        this.metrics.qualityScores.byStrategy[strategy] = [];
      }
      this.metrics.qualityScores.byStrategy[strategy].push(score);
    }
    
    if (language) {
      if (!this.metrics.qualityScores.byLanguage[language]) {
        this.metrics.qualityScores.byLanguage[language] = [];
      }
      this.metrics.qualityScores.byLanguage[language].push(score);
    }
  }

  calculateSummary() {
    const summary = {
      totalTests: this.getTotalTests(),
      passRate: this.getOverallPassRate(),
      syntaxValidation: this.getSyntaxSummary(),
      roundTripTesting: this.getRoundTripSummary(),
      semanticEquivalence: this.getSemanticSummary(),
      performance: this.getPerformanceSummary(),
      qualityScores: this.getQualitySummary()
    };

    return summary;
  }

  getTotalTests() {
    return this.metrics.syntaxValidation.passed + this.metrics.syntaxValidation.failed;
  }

  getOverallPassRate() {
    const total = this.getTotalTests();
    if (total === 0) return 0;
    
    const passed = Math.min(
      this.metrics.syntaxValidation.passed,
      this.metrics.roundTripTesting.passed,
      this.metrics.semanticEquivalence.passed
    );
    
    return passed / total;
  }

  getSyntaxSummary() {
    const total = this.metrics.syntaxValidation.passed + this.metrics.syntaxValidation.failed;
    
    return {
      total,
      passed: this.metrics.syntaxValidation.passed,
      failed: this.metrics.syntaxValidation.failed,
      passRate: total > 0 ? this.metrics.syntaxValidation.passed / total : 0,
      errorCount: this.metrics.syntaxValidation.errors.length,
      commonErrors: this.getCommonErrors()
    };
  }

  getRoundTripSummary() {
    const total = this.metrics.roundTripTesting.passed + this.metrics.roundTripTesting.failed;
    const scores = this.metrics.roundTripTesting.equivalenceScores;
    
    return {
      total,
      passed: this.metrics.roundTripTesting.passed,
      failed: this.metrics.roundTripTesting.failed,
      passRate: total > 0 ? this.metrics.roundTripTesting.passed / total : 0,
      averageEquivalence: scores.length > 0 ? scores.reduce((a, b) => a + b, 0) / scores.length : 0,
      minEquivalence: scores.length > 0 ? Math.min(...scores) : 0,
      maxEquivalence: scores.length > 0 ? Math.max(...scores) : 0
    };
  }

  getSemanticSummary() {
    const scores = this.metrics.semanticEquivalence.scores;
    const total = scores.length;
    
    return {
      total,
      passed: this.metrics.semanticEquivalence.passed,
      failed: this.metrics.semanticEquivalence.failed,
      passRate: total > 0 ? this.metrics.semanticEquivalence.passed / total : 0,
      averageScore: total > 0 ? scores.reduce((a, b) => a + b, 0) / total : 0,
      minScore: total > 0 ? Math.min(...scores) : 0,
      maxScore: total > 0 ? Math.max(...scores) : 0,
      above60Percent: scores.filter(s => s >= 0.6).length
    };
  }

  getPerformanceSummary() {
    const translationTimes = this.metrics.performance.translationTimes;
    const validationTimes = this.metrics.performance.validationTimes;
    const memoryUsage = this.metrics.performance.memoryUsage;
    
    return {
      translation: this.calculateStats(translationTimes),
      validation: this.calculateStats(validationTimes),
      memory: this.calculateStats(memoryUsage)
    };
  }

  getQualitySummary() {
    const overall = this.metrics.qualityScores.overall;
    
    const summary = {
      overall: this.calculateStats(overall),
      byStrategy: {},
      byLanguage: {}
    };
    
    // By strategy
    for (const [strategy, scores] of Object.entries(this.metrics.qualityScores.byStrategy)) {
      summary.byStrategy[strategy] = this.calculateStats(scores);
    }
    
    // By language
    for (const [language, scores] of Object.entries(this.metrics.qualityScores.byLanguage)) {
      summary.byLanguage[language] = this.calculateStats(scores);
    }
    
    return summary;
  }

  calculateStats(values) {
    if (!values || values.length === 0) {
      return { count: 0, average: 0, min: 0, max: 0, median: 0, stddev: 0 };
    }
    
    const sorted = [...values].sort((a, b) => a - b);
    const sum = values.reduce((a, b) => a + b, 0);
    const average = sum / values.length;
    const median = sorted[Math.floor(sorted.length / 2)];
    
    // Standard deviation
    const variance = values.reduce((acc, val) => acc + Math.pow(val - average, 2), 0) / values.length;
    const stddev = Math.sqrt(variance);
    
    return {
      count: values.length,
      average,
      min: sorted[0],
      max: sorted[sorted.length - 1],
      median,
      stddev
    };
  }

  getCommonErrors() {
    const errorCounts = {};
    
    for (const error of this.metrics.syntaxValidation.errors) {
      const key = error.substring(0, 100); // Truncate for grouping
      errorCounts[key] = (errorCounts[key] || 0) + 1;
    }
    
    return Object.entries(errorCounts)
      .sort(([,a], [,b]) => b - a)
      .slice(0, 10)
      .map(([error, count]) => ({ error, count }));
  }
}

// ============================================
// Translation Validator
// ============================================

class TranslationValidator {
  constructor(options = {}) {
    this.options = {
      concurrency: 5,
      enableRoundTrip: true,
      enableSemantic: true,
      enablePerformance: true,
      timeout: 30000, // 30 seconds
      ...options
    };
    
    this.translator = new UniversalTranslator();
    this.qualityValidator = new DatasetQualityValidator();
    this.roundTripTester = new RoundTripTester();
    this.abapParser = new ABAPParser();
    this.astChecker = new ASTEquivalenceChecker();
    
    this.metrics = new ValidationMetrics();
  }

  async validateDataset(datasetPath, options = {}) {
    console.log(chalk.blue('Translation Validation System'));
    console.log(chalk.gray('Validating code-to-ABAP translation quality'));
    console.log();

    const config = { ...this.options, ...options };
    
    try {
      // Load dataset
      console.log(chalk.blue('Loading dataset...'));
      const items = await this.loadDataset(datasetPath);
      
      if (items.length === 0) {
        console.log(chalk.yellow('No items found in dataset'));
        return { success: false, reason: 'Empty dataset' };
      }
      
      console.log(chalk.green(`Loaded ${items.length} items for validation`));
      
      // Sample if requested
      const itemsToValidate = config.sampleSize && config.sampleSize < items.length ?
        this.sampleItems(items, config.sampleSize) : items;
      
      console.log(chalk.blue(`Validating ${itemsToValidate.length} items...`));
      
      // Validate items
      await this.validateItems(itemsToValidate, config);
      
      // Calculate summary
      const summary = this.metrics.calculateSummary();
      
      // Display results
      this.displayResults(summary);
      
      // Save detailed report if requested
      if (config.outputReport) {
        await this.saveReport(summary, config.outputReport);
      }
      
      return {
        success: true,
        summary,
        metrics: this.metrics.metrics
      };
      
    } catch (error) {
      console.error(chalk.red('Validation failed:'), error.message);
      return {
        success: false,
        error: error.message
      };
    }
  }

  async loadDataset(datasetPath) {
    const content = await fs.readFile(datasetPath, 'utf8');
    const lines = content.split('\n').filter(line => line.trim());
    
    const items = [];
    for (const line of lines) {
      try {
        const item = JSON.parse(line);
        items.push(item);
      } catch (error) {
        // Skip invalid JSON lines
        continue;
      }
    }
    
    return items;
  }

  sampleItems(items, sampleSize) {
    if (sampleSize >= items.length) return items;
    
    // Stratified sampling by language and strategy
    const grouped = {};
    
    for (const item of items) {
      const key = `${item.sourceLanguage}_${item.metadata?.strategy || 'unknown'}`;
      if (!grouped[key]) {
        grouped[key] = [];
      }
      grouped[key].push(item);
    }
    
    const sampledItems = [];
    const groups = Object.values(grouped);
    const itemsPerGroup = Math.ceil(sampleSize / groups.length);
    
    for (const group of groups) {
      const sample = group.slice(0, itemsPerGroup);
      sampledItems.push(...sample);
    }
    
    return sampledItems.slice(0, sampleSize);
  }

  async validateItems(items, config) {
    const limit = pLimit(config.concurrency);
    
    const promises = items.map((item, index) =>
      limit(() => this.validateItem(item, index, config))
    );
    
    // Track progress
    let completed = 0;
    const total = items.length;
    
    for (const promise of promises) {
      await promise;
      completed++;
      
      if (completed % 10 === 0 || completed === total) {
        const percentage = (completed / total * 100).toFixed(1);
        console.log(chalk.gray(`Progress: ${completed}/${total} (${percentage}%)`));
      }
    }
  }

  async validateItem(item, index, config) {
    const startTime = performance.now();
    
    try {
      // 1. Syntax Validation
      const syntaxValid = await this.validateSyntax(item);
      
      // 2. Round-trip Testing (if ABAP code exists)
      let roundTripScore = 0;
      if (config.enableRoundTrip && item.abapCode) {
        roundTripScore = await this.validateRoundTrip(item);
      }
      
      // 3. Semantic Equivalence
      let semanticScore = 0;
      if (config.enableSemantic) {
        semanticScore = await this.validateSemanticEquivalence(item);
      }
      
      // 4. Performance Metrics
      const endTime = performance.now();
      const validationTime = endTime - startTime;
      const memoryUsage = process.memoryUsage().heapUsed;
      
      this.metrics.addPerformanceData(
        item.metadata?.translationTime || 0,
        validationTime,
        memoryUsage
      );
      
      // 5. Overall Quality Score
      const qualityScore = this.calculateOverallQuality(
        syntaxValid ? 100 : 0,
        roundTripScore * 100,
        semanticScore * 100
      );
      
      this.metrics.addQualityScore(
        qualityScore,
        item.metadata?.strategy,
        item.sourceLanguage
      );
      
    } catch (error) {
      console.error(chalk.red(`Error validating item ${index}:`), error.message);
      this.metrics.addSyntaxValidation(false, error.message);
    }
  }

  async validateSyntax(item) {
    if (!item.abapCode) {
      this.metrics.addSyntaxValidation(false, 'No ABAP code to validate');
      return false;
    }
    
    try {
      const parseResult = this.abapParser.parse(item.abapCode);
      
      if (parseResult.ast) {
        this.metrics.addSyntaxValidation(true);
        return true;
      } else {
        this.metrics.addSyntaxValidation(false, 'Failed to parse ABAP code');
        return false;
      }
    } catch (error) {
      this.metrics.addSyntaxValidation(false, error.message);
      return false;
    }
  }

  async validateRoundTrip(item) {
    try {
      const testResult = await this.roundTripTester.testRoundTrip(item.abapCode);
      
      if (testResult.success) {
        // Calculate AST equivalence score
        const originalAST = this.abapParser.parse(item.abapCode).ast;
        const transformedAST = this.abapParser.parse(testResult.transformed).ast;
        
        const equivalenceScore = this.calculateASTEquivalence(originalAST, transformedAST);
        
        this.metrics.addRoundTripTest(true, equivalenceScore);
        return equivalenceScore;
      } else {
        this.metrics.addRoundTripTest(false, 0);
        return 0;
      }
    } catch (error) {
      this.metrics.addRoundTripTest(false, 0);
      return 0;
    }
  }

  calculateASTEquivalence(ast1, ast2) {
    try {
      if (this.astChecker.areEquivalent(ast1, ast2)) {
        return 1.0; // 100% equivalent
      }
      
      // Calculate partial equivalence
      return this.calculatePartialEquivalence(ast1, ast2);
    } catch (error) {
      return 0;
    }
  }

  calculatePartialEquivalence(ast1, ast2) {
    // This implements the 60% AST equivalence strategy
    // by comparing node types, structures, and key properties
    
    if (!ast1 || !ast2) return 0;
    
    let totalNodes = 0;
    let equivalentNodes = 0;
    
    const compareNodes = (node1, node2) => {
      totalNodes++;
      
      // Compare node types
      if (node1.constructor.name === node2.constructor.name) {
        equivalentNodes += 0.3; // 30% for type match
        
        // Compare statement types if available
        if (node1.get && node2.get) {
          if (node1.get().constructor.name === node2.get().constructor.name) {
            equivalentNodes += 0.4; // 40% for statement type match
            
            // Compare token structure (simplified)
            const tokens1 = node1.getTokens ? node1.getTokens() : [];
            const tokens2 = node2.getTokens ? node2.getTokens() : [];
            
            if (tokens1.length === tokens2.length) {
              equivalentNodes += 0.3; // 30% for token structure match
            }
          }
        }
      }
      
      // Recursively compare children
      const children1 = node1.getChildren ? node1.getChildren() : [];
      const children2 = node2.getChildren ? node2.getChildren() : [];
      
      const minChildren = Math.min(children1.length, children2.length);
      for (let i = 0; i < minChildren; i++) {
        compareNodes(children1[i], children2[i]);
      }
    };
    
    compareNodes(ast1, ast2);
    
    return totalNodes > 0 ? equivalentNodes / totalNodes : 0;
  }

  async validateSemanticEquivalence(item) {
    try {
      // Use the existing quality validator for semantic checks
      const validation = await this.qualityValidator.validateDatasetItem(item);
      
      let semanticScore = 0;
      
      // Aggregate semantic metrics
      if (validation.masking?.contextPreservation?.controlFlowPreserved) {
        semanticScore += 0.3;
      }
      
      if (validation.masking?.contextPreservation?.functionSignaturesPreserved) {
        semanticScore += 0.3;
      }
      
      if (validation.translation?.translated && validation.translation?.syntacticValidity) {
        semanticScore += 0.4;
      }
      
      this.metrics.addSemanticEquivalence(semanticScore);
      return semanticScore;
      
    } catch (error) {
      this.metrics.addSemanticEquivalence(0);
      return 0;
    }
  }

  calculateOverallQuality(syntaxScore, roundTripScore, semanticScore) {
    // Weighted average with emphasis on syntax and semantic equivalence
    const weights = {
      syntax: 0.4,
      roundTrip: 0.3,
      semantic: 0.3
    };
    
    return (
      syntaxScore * weights.syntax +
      roundTripScore * weights.roundTrip +
      semanticScore * weights.semantic
    );
  }

  displayResults(summary) {
    console.log(chalk.green('\n=== Validation Results ==='));
    
    // Overall metrics
    console.log(chalk.blue('\nOverall:'));
    console.log(`Total tests: ${summary.totalTests}`);
    console.log(`Overall pass rate: ${(summary.passRate * 100).toFixed(1)}%`);
    
    // Syntax validation
    console.log(chalk.blue('\nSyntax Validation:'));
    console.log(`Pass rate: ${(summary.syntaxValidation.passRate * 100).toFixed(1)}%`);
    console.log(`Passed: ${summary.syntaxValidation.passed}`);
    console.log(`Failed: ${summary.syntaxValidation.failed}`);
    console.log(`Error count: ${summary.syntaxValidation.errorCount}`);
    
    // Round-trip testing
    console.log(chalk.blue('\nRound-trip Testing:'));
    console.log(`Pass rate: ${(summary.roundTripTesting.passRate * 100).toFixed(1)}%`);
    console.log(`Average equivalence: ${(summary.roundTripTesting.averageEquivalence * 100).toFixed(1)}%`);
    console.log(`Min equivalence: ${(summary.roundTripTesting.minEquivalence * 100).toFixed(1)}%`);
    console.log(`Max equivalence: ${(summary.roundTripTesting.maxEquivalence * 100).toFixed(1)}%`);
    
    // Semantic equivalence
    console.log(chalk.blue('\nSemantic Equivalence:'));
    console.log(`Pass rate (≥60%): ${(summary.semanticEquivalence.passRate * 100).toFixed(1)}%`);
    console.log(`Average score: ${(summary.semanticEquivalence.averageScore * 100).toFixed(1)}%`);
    console.log(`Above 60% threshold: ${summary.semanticEquivalence.above60Percent}`);
    
    // Performance
    console.log(chalk.blue('\nPerformance:'));
    if (summary.performance.translation.count > 0) {
      console.log(`Avg translation time: ${summary.performance.translation.average.toFixed(2)}ms`);
    }
    if (summary.performance.validation.count > 0) {
      console.log(`Avg validation time: ${summary.performance.validation.average.toFixed(2)}ms`);
    }
    
    // Quality scores
    console.log(chalk.blue('\nQuality Scores:'));
    console.log(`Overall average: ${summary.qualityScores.overall.average.toFixed(1)}`);
    
    if (Object.keys(summary.qualityScores.byLanguage).length > 0) {
      console.log('\nBy Language:');
      for (const [lang, stats] of Object.entries(summary.qualityScores.byLanguage)) {
        console.log(`  ${lang}: ${stats.average.toFixed(1)} (${stats.count} items)`);
      }
    }
    
    if (Object.keys(summary.qualityScores.byStrategy).length > 0) {
      console.log('\nBy Strategy:');
      for (const [strategy, stats] of Object.entries(summary.qualityScores.byStrategy)) {
        console.log(`  ${strategy}: ${stats.average.toFixed(1)} (${stats.count} items)`);
      }
    }
    
    // Common errors
    if (summary.syntaxValidation.commonErrors.length > 0) {
      console.log(chalk.blue('\nMost Common Errors:'));
      for (const { error, count } of summary.syntaxValidation.commonErrors.slice(0, 5)) {
        console.log(`  ${count}x: ${error.substring(0, 80)}...`);
      }
    }
  }

  async saveReport(summary, outputPath) {
    const report = {
      timestamp: new Date().toISOString(),
      summary,
      detailedMetrics: this.metrics.metrics,
      configuration: this.options
    };
    
    await fs.writeJson(outputPath, report, { spaces: 2 });
    console.log(chalk.green(`\nDetailed report saved to: ${outputPath}`));
  }
}

// ============================================
// Benchmark Runner
// ============================================

class BenchmarkRunner {
  constructor() {
    this.translator = new UniversalTranslator();
    this.validator = new TranslationValidator();
  }

  async runBenchmark(testCases, options = {}) {
    console.log(chalk.blue('Translation Performance Benchmark'));
    console.log(chalk.gray(`Running ${testCases.length} test cases...`));
    console.log();

    const results = {
      testCases: testCases.length,
      totalTime: 0,
      translationTimes: [],
      validationTimes: [],
      successRate: 0,
      qualityScores: []
    };

    for (let i = 0; i < testCases.length; i++) {
      const testCase = testCases[i];
      console.log(chalk.gray(`Running test case ${i + 1}/${testCases.length}: ${testCase.name}`));

      const startTime = performance.now();
      
      try {
        // Translate
        const translationResult = await this.translator.translateCode(
          testCase.sourceCode,
          testCase.language
        );
        
        const translationTime = performance.now() - startTime;
        results.translationTimes.push(translationTime);

        if (translationResult.success) {
          // Validate
          const validationStart = performance.now();
          
          const item = {
            sourceLanguage: testCase.language,
            originalCode: testCase.sourceCode,
            abapCode: translationResult.abapCode,
            metadata: translationResult.metadata
          };
          
          await this.validator.validateItem(item, i, { 
            enableRoundTrip: true,
            enableSemantic: true 
          });
          
          const validationTime = performance.now() - validationStart;
          results.validationTimes.push(validationTime);
          
          results.successRate++;
          results.qualityScores.push(translationResult.metadata?.confidence || 0);
        }
        
      } catch (error) {
        console.error(chalk.red(`Error in test case ${i + 1}:`), error.message);
      }
    }

    results.totalTime = results.translationTimes.reduce((a, b) => a + b, 0) +
                       results.validationTimes.reduce((a, b) => a + b, 0);
    
    results.successRate = results.successRate / testCases.length;

    this.displayBenchmarkResults(results);
    
    return results;
  }

  displayBenchmarkResults(results) {
    console.log(chalk.green('\n=== Benchmark Results ==='));
    
    console.log(`Test cases: ${results.testCases}`);
    console.log(`Success rate: ${(results.successRate * 100).toFixed(1)}%`);
    console.log(`Total time: ${results.totalTime.toFixed(2)}ms`);
    
    if (results.translationTimes.length > 0) {
      const avgTranslation = results.translationTimes.reduce((a, b) => a + b, 0) / results.translationTimes.length;
      console.log(`Avg translation time: ${avgTranslation.toFixed(2)}ms`);
    }
    
    if (results.validationTimes.length > 0) {
      const avgValidation = results.validationTimes.reduce((a, b) => a + b, 0) / results.validationTimes.length;
      console.log(`Avg validation time: ${avgValidation.toFixed(2)}ms`);
    }
    
    if (results.qualityScores.length > 0) {
      const avgQuality = results.qualityScores.reduce((a, b) => a + b, 0) / results.qualityScores.length;
      console.log(`Avg quality score: ${avgQuality.toFixed(2)}`);
    }
  }
}

// ============================================
// CLI Interface
// ============================================

function createCLI() {
  const program = new Command();
  
  program
    .name('validate-translations')
    .description('Validate code-to-ABAP translation quality')
    .version('1.0.0');

  program
    .command('validate')
    .description('Validate translations in a dataset')
    .argument('<dataset>', 'Path to JSONL dataset file')
    .option('-c, --concurrency <n>', 'Concurrent validations', '5')
    .option('-s, --sample-size <n>', 'Sample size (0 = all)', '0')
    .option('-o, --output-report <file>', 'Save detailed report')
    .option('--no-roundtrip', 'Skip round-trip testing')
    .option('--no-semantic', 'Skip semantic validation')
    .option('--no-performance', 'Skip performance metrics')
    .option('--timeout <ms>', 'Validation timeout', '30000')
    .action(async (dataset, options) => {
      const validator = new TranslationValidator({
        concurrency: parseInt(options.concurrency),
        enableRoundTrip: options.roundtrip !== false,
        enableSemantic: options.semantic !== false,
        enablePerformance: options.performance !== false,
        timeout: parseInt(options.timeout)
      });

      const config = {
        sampleSize: parseInt(options.sampleSize) || 0,
        outputReport: options.outputReport
      };

      const result = await validator.validateDataset(dataset, config);
      
      if (!result.success) {
        console.error(chalk.red('Validation failed:'), result.error || result.reason);
        process.exit(1);
      }
    });

  program
    .command('benchmark')
    .description('Run translation performance benchmark')
    .option('--test-cases <file>', 'JSON file with test cases')
    .option('--builtin', 'Use built-in test cases')
    .action(async (options) => {
      const runner = new BenchmarkRunner();
      
      let testCases = [];
      
      if (options.testCases) {
        testCases = await fs.readJson(options.testCases);
      } else if (options.builtin) {
        testCases = getBuiltinTestCases();
      } else {
        console.error(chalk.red('Either --test-cases or --builtin must be specified'));
        process.exit(1);
      }
      
      await runner.runBenchmark(testCases);
    });

  program
    .command('syntax-check')
    .description('Quick syntax validation of ABAP code')
    .argument('<abap-file>', 'Path to ABAP file or JSONL dataset')
    .action(async (abapFile) => {
      const validator = new TranslationValidator();
      
      try {
        if (abapFile.endsWith('.jsonl')) {
          // Validate dataset
          const items = await validator.loadDataset(abapFile);
          
          let passed = 0;
          let failed = 0;
          
          for (const item of items) {
            if (item.abapCode) {
              const valid = await validator.validateSyntax(item);
              if (valid) passed++;
              else failed++;
            }
          }
          
          console.log(chalk.green(`Syntax validation complete:`));
          console.log(`Passed: ${passed}`);
          console.log(`Failed: ${failed}`);
          console.log(`Success rate: ${(passed / (passed + failed) * 100).toFixed(1)}%`);
          
        } else {
          // Single file
          const abapCode = await fs.readFile(abapFile, 'utf8');
          const item = { abapCode };
          
          const valid = await validator.validateSyntax(item);
          
          if (valid) {
            console.log(chalk.green('✓ ABAP syntax is valid'));
          } else {
            console.log(chalk.red('✗ ABAP syntax validation failed'));
            process.exit(1);
          }
        }
        
      } catch (error) {
        console.error(chalk.red('Syntax check failed:'), error.message);
        process.exit(1);
      }
    });

  return program;
}

function getBuiltinTestCases() {
  return [
    {
      name: 'Simple function',
      language: 'javascript',
      sourceCode: `
function add(a, b) {
  return a + b;
}
      `.trim()
    },
    {
      name: 'If-else statement',
      language: 'javascript',
      sourceCode: `
function checkNumber(num) {
  if (num > 0) {
    return "positive";
  } else {
    return "negative or zero";
  }
}
      `.trim()
    },
    {
      name: 'For loop',
      language: 'javascript',
      sourceCode: `
function sumArray(arr) {
  let sum = 0;
  for (let i = 0; i < arr.length; i++) {
    sum += arr[i];
  }
  return sum;
}
      `.trim()
    },
    {
      name: 'Python function',
      language: 'python',
      sourceCode: `
def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)
      `.trim()
    },
    {
      name: 'Go function',
      language: 'go',
      sourceCode: `
func factorial(n int) int {
    if n <= 1 {
        return 1
    }
    return n * factorial(n - 1)
}
      `.trim()
    }
  ];
}

// ============================================
// Main Entry Point
// ============================================

async function main() {
  console.log(chalk.blue.bold('Universal Translation Validation System'));
  console.log(chalk.gray('Validating code-to-ABAP translation quality'));
  console.log();

  if (process.argv.length <= 2) {
    const program = createCLI();
    program.help();
  } else {
    const program = createCLI();
    await program.parseAsync(process.argv);
  }
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch(error => {
    console.error(chalk.red('Fatal error:'), error.message);
    console.error(error.stack);
    process.exit(1);
  });
}

export { TranslationValidator, ValidationMetrics, BenchmarkRunner };