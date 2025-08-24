#!/usr/bin/env node

/**
 * Enhanced Universal Translator with Configurable Prompts and Few-Shot Learning
 * 
 * This version uses:
 * 1. Convention-based prompt files (YAML/JSON)
 * 2. Few-shot examples for better translation
 * 3. Multiple translation strategies
 */

const axios = require('axios');
const Parser = require('tree-sitter');
const JavaScript = require('tree-sitter-javascript');
const Python = require('tree-sitter-python');
const Go = require('tree-sitter-go');
const TypeScript = require('tree-sitter-typescript').typescript;
const fs = require('fs').promises;
const path = require('path');
const yaml = require('js-yaml');

// Azure OpenAI Configuration
const AZURE_CONFIG = {
  endpoint: process.env.AZURE_OPENAI_ENDPOINT || process.env.AZURE_OPENAI_API_URL,
  apiKey: process.env.AZURE_OPENAI_API_KEY,
  apiVersion: process.env.AZURE_OPENAI_API_VERSION || '2025-01-01-preview',
  deployment: process.env.AZURE_OPENAI_DEPLOYMENT || 'gpt-4.1'
};

/**
 * Prompt Manager - Loads and manages configurable prompts
 */
class PromptManager {
  constructor() {
    this.prompts = new Map();
    this.examples = new Map();
    this.conventions = null;
  }

  async initialize() {
    // Load base prompts
    await this.loadPromptFile('prompts/base-translation.yaml');
    
    // Load convention files
    const convention = process.env.ABAP_CONVENTION || 'sap-standard';
    await this.loadConvention(`prompts/conventions/${convention}.yaml`);
    
    // Load pattern mappings
    await this.loadPatterns('prompts/patterns/javascript-to-abap.yaml');
    await this.loadPatterns('prompts/patterns/python-to-abap.yaml');
    await this.loadPatterns('prompts/patterns/go-to-abap.yaml');
    
    // Load few-shot examples
    await this.loadExamples('prompts/examples/');
    
    console.log('✅ Loaded prompt configuration:');
    console.log(`   Convention: ${convention}`);
    console.log(`   Examples: ${this.examples.size} language pairs`);
  }

  async loadPromptFile(filePath) {
    try {
      const content = await fs.readFile(filePath, 'utf8');
      const data = yaml.load(content);
      this.prompts.set(path.basename(filePath, '.yaml'), data);
    } catch (error) {
      console.warn(`   ⚠️ Could not load ${filePath}: ${error.message}`);
    }
  }

  async loadConvention(filePath) {
    try {
      const content = await fs.readFile(filePath, 'utf8');
      this.conventions = yaml.load(content);
    } catch (error) {
      console.warn(`   ⚠️ Could not load convention ${filePath}`);
    }
  }

  async loadPatterns(filePath) {
    try {
      const content = await fs.readFile(filePath, 'utf8');
      const patterns = yaml.load(content);
      const lang = path.basename(filePath).split('-')[0];
      this.prompts.set(`patterns_${lang}`, patterns);
    } catch (error) {
      console.warn(`   ⚠️ Could not load patterns ${filePath}`);
    }
  }

  async loadExamples(dirPath) {
    try {
      const files = await fs.readdir(dirPath);
      for (const file of files) {
        if (file.endsWith('.json')) {
          const content = await fs.readFile(path.join(dirPath, file), 'utf8');
          const examples = JSON.parse(content);
          const lang = file.replace('.json', '');
          this.examples.set(lang, examples);
        }
      }
    } catch (error) {
      console.warn(`   ⚠️ Could not load examples: ${error.message}`);
    }
  }

  /**
   * Build a prompt with few-shot examples
   */
  buildPrompt(sourceLanguage, targetLanguage = 'ABAP', options = {}) {
    const base = this.prompts.get('base-translation') || {};
    const patterns = this.prompts.get(`patterns_${sourceLanguage.toLowerCase()}`) || {};
    const examples = this.examples.get(sourceLanguage.toLowerCase()) || [];
    
    let systemPrompt = base.system_prompt || '';
    let userPrompt = base.user_prompt || '';
    
    // Replace placeholders
    systemPrompt = systemPrompt
      .replace(/{source_language}/g, sourceLanguage)
      .replace(/{target_language}/g, targetLanguage);
    
    // Add language-specific guidelines
    if (base.languages && base.languages[targetLanguage]) {
      systemPrompt += '\n\n' + base.languages[targetLanguage].guidelines;
    }
    
    // Add conventions if loaded
    if (this.conventions) {
      systemPrompt += '\n\nNaming Conventions:\n';
      systemPrompt += this.formatConventions(this.conventions.naming_conventions);
      
      if (this.conventions.best_practices) {
        systemPrompt += '\n\nBest Practices:\n';
        systemPrompt += this.formatBestPractices(this.conventions.best_practices);
      }
    }
    
    // Add few-shot examples
    if (examples.length > 0 && options.includeExamples !== false) {
      systemPrompt += '\n\nHere are some examples of correct translations:\n';
      
      // Select relevant examples (max 3)
      const relevantExamples = this.selectRelevantExamples(examples, options.codeType);
      
      for (const example of relevantExamples) {
        systemPrompt += `\nExample ${example.type}:\n`;
        systemPrompt += `Input ${sourceLanguage}:\n\`\`\`${sourceLanguage.toLowerCase()}\n${example.source}\n\`\`\`\n`;
        systemPrompt += `Output ABAP:\n\`\`\`abap\n${example.target}\n\`\`\`\n`;
        
        if (example.explanation) {
          systemPrompt += `Key points: ${example.explanation}\n`;
        }
      }
    }
    
    // Add pattern-specific instructions
    if (patterns.additional_instructions) {
      systemPrompt += '\n\n' + patterns.additional_instructions;
    }
    
    return { systemPrompt, userPrompt };
  }

  formatConventions(conventions) {
    let formatted = '';
    for (const [category, rules] of Object.entries(conventions)) {
      formatted += `\n${category}:\n`;
      for (const [type, rule] of Object.entries(rules)) {
        if (rule.prefix) {
          formatted += `  - ${type}: use prefix "${rule.prefix}" (e.g., ${rule.example})\n`;
        }
      }
    }
    return formatted;
  }

  formatBestPractices(practices) {
    let formatted = '';
    for (const [category, items] of Object.entries(practices)) {
      formatted += `\n${category}:\n`;
      if (Array.isArray(items)) {
        items.forEach(item => formatted += `  - ${item}\n`);
      }
    }
    return formatted;
  }

  selectRelevantExamples(examples, codeType) {
    // Select up to 3 most relevant examples based on code type
    let relevant = examples;
    
    if (codeType) {
      relevant = examples.filter(e => e.type === codeType || e.tags?.includes(codeType));
    }
    
    // If still too many, prioritize by complexity
    if (relevant.length > 3) {
      relevant = relevant
        .sort((a, b) => (b.priority || 0) - (a.priority || 0))
        .slice(0, 3);
    }
    
    return relevant;
  }
}

/**
 * Enhanced Azure OpenAI Service with better prompt handling
 */
class EnhancedAzureOpenAIService {
  constructor(config, promptManager) {
    this.config = config;
    this.promptManager = promptManager;
    this.requestCount = 0;
    this.cache = new Map();
  }

  /**
   * Main translation method with multiple strategies
   */
  async translate(input, sourceLanguage, options = {}) {
    const strategy = options.strategy || 'auto';
    
    switch (strategy) {
      case 'direct':
        return await this.directTranslation(input, sourceLanguage, options);
      
      case 'ast':
        return await this.astTranslation(input, sourceLanguage, options);
      
      case 'hybrid':
        return await this.hybridTranslation(input, sourceLanguage, options);
      
      case 'auto':
      default:
        return await this.autoTranslation(input, sourceLanguage, options);
    }
  }

  /**
   * Direct source code to ABAP translation
   */
  async directTranslation(sourceCode, sourceLanguage, options = {}) {
    const { systemPrompt, userPrompt } = this.promptManager.buildPrompt(
      sourceLanguage, 'ABAP', { ...options, codeType: 'direct' }
    );
    
    const fullUserPrompt = userPrompt
      .replace(/{source_language}/g, sourceLanguage)
      .replace(/{source_language_lower}/g, sourceLanguage.toLowerCase())
      .replace(/{source_code}/g, sourceCode)
      .replace(/{target_language}/g, 'ABAP');
    
    return await this.callAzureOpenAI(fullUserPrompt, systemPrompt, options.temperature || 0.2);
  }

  /**
   * AST-based translation with structure preservation
   */
  async astTranslation(ast, sourceLanguage, options = {}) {
    const { systemPrompt } = this.promptManager.buildPrompt(
      sourceLanguage, 'ABAP', { ...options, codeType: 'ast' }
    );
    
    // Create a simplified AST representation
    const simplifiedAST = this.simplifyAST(ast);
    
    const userPrompt = `Convert this ${sourceLanguage} code structure to ABAP:

AST Structure:
${JSON.stringify(simplifiedAST, null, 2)}

Original Code Context:
\`\`\`${sourceLanguage.toLowerCase()}
${ast.text || options.sourceCode || '// Code context not available'}
\`\`\`

Generate idiomatic ABAP code that preserves the structure and semantics.`;
    
    return await this.callAzureOpenAI(userPrompt, systemPrompt, 0.1);
  }

  /**
   * Hybrid approach: Use both direct and AST information
   */
  async hybridTranslation(input, sourceLanguage, options = {}) {
    const { systemPrompt } = this.promptManager.buildPrompt(
      sourceLanguage, 'ABAP', { ...options, codeType: 'hybrid' }
    );
    
    const userPrompt = `Translate this ${sourceLanguage} code to ABAP using both the source and structure:

Source Code:
\`\`\`${sourceLanguage.toLowerCase()}
${input.sourceCode}
\`\`\`

Code Structure (AST):
${this.describeStructure(input.ast)}

Key Elements to Preserve:
- Functions/Methods: ${this.extractFunctions(input.ast).join(', ')}
- Variables: ${this.extractVariables(input.ast).join(', ')}
- Control Flow: ${this.describeControlFlow(input.ast)}

Generate ABAP code that accurately translates both syntax and semantics.`;
    
    return await this.callAzureOpenAI(userPrompt, systemPrompt, 0.15);
  }

  /**
   * Automatic strategy selection based on code complexity
   */
  async autoTranslation(input, sourceLanguage, options = {}) {
    const complexity = this.assessComplexity(input);
    
    if (complexity < 3) {
      // Simple code - use direct translation
      return await this.directTranslation(input, sourceLanguage, options);
    } else if (complexity < 7) {
      // Medium complexity - use hybrid
      return await this.hybridTranslation({ sourceCode: input, ast: options.ast }, sourceLanguage, options);
    } else {
      // Complex code - use AST-based
      return await this.astTranslation(options.ast || input, sourceLanguage, options);
    }
  }

  /**
   * Call Azure OpenAI API
   */
  async callAzureOpenAI(userPrompt, systemPrompt, temperature = 0.2) {
    // Check cache
    const cacheKey = `${systemPrompt}::${userPrompt}`;
    if (this.cache.has(cacheKey)) {
      console.log('   📦 Using cached response');
      return this.cache.get(cacheKey);
    }
    
    const url = `${this.config.endpoint}openai/deployments/${this.config.deployment}/chat/completions?api-version=${this.config.apiVersion}`;
    
    try {
      console.log(`   🤖 Azure OpenAI request #${++this.requestCount}`);
      
      const response = await axios.post(url, {
        messages: [
          { role: 'system', content: systemPrompt },
          { role: 'user', content: userPrompt }
        ],
        temperature,
        max_tokens: 3000,
        top_p: 0.95
      }, {
        headers: {
          'api-key': this.config.apiKey,
          'Content-Type': 'application/json'
        }
      });
      
      const result = response.data.choices[0].message.content;
      
      // Extract code from markdown if present
      const cleanedResult = result
        .replace(/```abap\n?/gi, '')
        .replace(/```\n?/g, '')
        .trim();
      
      // Cache the result
      this.cache.set(cacheKey, cleanedResult);
      
      return cleanedResult;
      
    } catch (error) {
      console.error('❌ Azure OpenAI error:', error.response?.data || error.message);
      throw error;
    }
  }

  /**
   * Helper methods for AST analysis
   */
  simplifyAST(ast) {
    if (typeof ast === 'string') {
      return { type: 'code', content: ast };
    }
    
    const simplified = {
      type: ast.type || ast.nodeType,
      children: []
    };
    
    if (ast.name) simplified.name = ast.name;
    if (ast.value) simplified.value = ast.value;
    
    if (ast.children && Array.isArray(ast.children)) {
      simplified.children = ast.children
        .filter(child => !['comment', 'whitespace'].includes(child.type))
        .map(child => this.simplifyAST(child));
    }
    
    return simplified;
  }

  describeStructure(ast) {
    const functions = this.extractFunctions(ast);
    const classes = this.extractClasses(ast);
    const loops = this.countLoops(ast);
    const conditions = this.countConditions(ast);
    
    return `Structure Summary:
- Functions/Methods: ${functions.length}
- Classes: ${classes.length}
- Loops: ${loops}
- Conditionals: ${conditions}`;
  }

  extractFunctions(ast) {
    // Extract function names from AST
    const functions = [];
    
    const traverse = (node) => {
      if (node.type === 'function_declaration' || 
          node.type === 'method_definition' ||
          node.type === 'function') {
        if (node.name) functions.push(node.name);
      }
      if (node.children) {
        node.children.forEach(traverse);
      }
    };
    
    if (ast) traverse(ast);
    return functions;
  }

  extractVariables(ast) {
    // Extract variable names from AST
    const variables = [];
    
    const traverse = (node) => {
      if (node.type === 'variable_declarator' || 
          node.type === 'identifier') {
        if (node.name || node.text) variables.push(node.name || node.text);
      }
      if (node.children) {
        node.children.forEach(traverse);
      }
    };
    
    if (ast) traverse(ast);
    return [...new Set(variables)]; // Unique variables
  }

  extractClasses(ast) {
    const classes = [];
    
    const traverse = (node) => {
      if (node.type === 'class_declaration' || node.type === 'class') {
        if (node.name) classes.push(node.name);
      }
      if (node.children) {
        node.children.forEach(traverse);
      }
    };
    
    if (ast) traverse(ast);
    return classes;
  }

  countLoops(ast) {
    let count = 0;
    
    const traverse = (node) => {
      if (['for_statement', 'while_statement', 'do_statement', 'for', 'while'].includes(node.type)) {
        count++;
      }
      if (node.children) {
        node.children.forEach(traverse);
      }
    };
    
    if (ast) traverse(ast);
    return count;
  }

  countConditions(ast) {
    let count = 0;
    
    const traverse = (node) => {
      if (['if_statement', 'conditional_expression', 'if', 'switch'].includes(node.type)) {
        count++;
      }
      if (node.children) {
        node.children.forEach(traverse);
      }
    };
    
    if (ast) traverse(ast);
    return count;
  }

  describeControlFlow(ast) {
    const loops = this.countLoops(ast);
    const conditions = this.countConditions(ast);
    
    if (loops > 0 && conditions > 0) {
      return `${loops} loops, ${conditions} conditionals`;
    } else if (loops > 0) {
      return `${loops} loops`;
    } else if (conditions > 0) {
      return `${conditions} conditionals`;
    } else {
      return 'linear flow';
    }
  }

  assessComplexity(input) {
    if (typeof input === 'string') {
      // Simple heuristic based on code length and keywords
      const lines = input.split('\n').length;
      const hasClasses = /class\s+\w+/i.test(input);
      const hasAsync = /async|await|promise/i.test(input);
      const hasLoops = /for|while|do\s*{/i.test(input);
      
      let complexity = Math.floor(lines / 20);
      if (hasClasses) complexity += 3;
      if (hasAsync) complexity += 2;
      if (hasLoops) complexity += 1;
      
      return complexity;
    }
    
    // AST-based complexity
    return this.countLoops(input) + this.countConditions(input) + this.extractFunctions(input).length;
  }
}

/**
 * Main Enhanced Translator
 */
class EnhancedUniversalTranslator {
  constructor() {
    this.promptManager = new PromptManager();
    this.llmService = null;
    this.parsers = new Map();
  }

  async initialize() {
    // Load prompts and conventions
    await this.promptManager.initialize();
    
    // Initialize LLM service
    this.llmService = new EnhancedAzureOpenAIService(AZURE_CONFIG, this.promptManager);
    
    // Initialize parsers
    this.initializeParsers();
    
    console.log('✅ Enhanced translator initialized');
  }

  initializeParsers() {
    const jsParser = new Parser();
    jsParser.setLanguage(JavaScript);
    this.parsers.set('javascript', jsParser);
    
    const tsParser = new Parser();
    tsParser.setLanguage(TypeScript);
    this.parsers.set('typescript', tsParser);
    
    const pyParser = new Parser();
    pyParser.setLanguage(Python);
    this.parsers.set('python', pyParser);
    
    const goParser = new Parser();
    goParser.setLanguage(Go);
    this.parsers.set('go', goParser);
  }

  /**
   * Main translation method
   */
  async translate(sourceCode, sourceLanguage, options = {}) {
    console.log(`\n🚀 Enhanced translation: ${sourceLanguage} → ABAP`);
    console.log(`   Strategy: ${options.strategy || 'auto'}`);
    console.log(`   Convention: ${process.env.ABAP_CONVENTION || 'sap-standard'}`);
    
    // Parse to AST if needed
    let ast = null;
    if (options.strategy !== 'direct') {
      const parser = this.parsers.get(sourceLanguage.toLowerCase());
      if (parser) {
        ast = parser.parse(sourceCode);
        console.log(`   📊 AST parsed: ${ast.rootNode.childCount} nodes`);
      }
    }
    
    // Prepare input based on strategy
    let input;
    if (options.strategy === 'ast') {
      input = ast;
    } else if (options.strategy === 'hybrid') {
      input = { sourceCode, ast };
    } else {
      input = sourceCode;
    }
    
    // Translate using LLM
    const abapCode = await this.llmService.translate(input, sourceLanguage, {
      ...options,
      ast,
      sourceCode
    });
    
    console.log('   ✅ Translation complete');
    
    return {
      success: true,
      sourceLanguage,
      targetLanguage: 'ABAP',
      sourceCode,
      abapCode,
      metadata: {
        strategy: options.strategy || 'auto',
        convention: process.env.ABAP_CONVENTION || 'sap-standard',
        llmRequests: this.llmService.requestCount
      }
    };
  }
}

// CLI interface
async function main() {
  const translator = new EnhancedUniversalTranslator();
  await translator.initialize();
  
  // Example usage
  const args = process.argv.slice(2);
  
  if (args.length === 0) {
    console.log(`
Enhanced Universal Translator with Configurable Prompts

Usage:
  node universal-translator-enhanced.js <file> <language> [options]

Options:
  --strategy <direct|ast|hybrid|auto>  Translation strategy (default: auto)
  --convention <name>                  ABAP convention (default: sap-standard)
  --examples                           Include few-shot examples
  --save                               Save output to file

Environment Variables:
  ABAP_CONVENTION     Convention to use (sap-standard, clean-abap)
  AZURE_OPENAI_*      Azure OpenAI configuration

Examples:
  node universal-translator-enhanced.js input.js javascript --strategy hybrid
  ABAP_CONVENTION=clean-abap node universal-translator-enhanced.js app.py python
`);
    return;
  }
  
  const filePath = args[0];
  const language = args[1];
  const options = {
    strategy: args.includes('--strategy') ? args[args.indexOf('--strategy') + 1] : 'auto',
    includeExamples: args.includes('--examples')
  };
  
  const sourceCode = await fs.readFile(filePath, 'utf8');
  const result = await translator.translate(sourceCode, language, options);
  
  if (result.success) {
    console.log('\n' + '='.repeat(60));
    console.log(result.abapCode);
    console.log('='.repeat(60));
    
    if (args.includes('--save')) {
      const outputPath = filePath.replace(/\.\w+$/, '.abap');
      await fs.writeFile(outputPath, result.abapCode);
      console.log(`\n💾 Saved to: ${outputPath}`);
    }
  }
}

if (require.main === module) {
  main().catch(console.error);
}

module.exports = { EnhancedUniversalTranslator, PromptManager, EnhancedAzureOpenAIService };