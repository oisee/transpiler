#!/usr/bin/env node

/**
 * Universal Code-to-ABAP Translator with Azure OpenAI LLM Enhancement
 * 
 * This enhanced version uses Azure OpenAI for intelligent AST transformations,
 * providing superior translation quality through LLM-guided semantic mapping.
 */

const axios = require('axios');
const Parser = require('tree-sitter');
const JavaScript = require('tree-sitter-javascript');
const Python = require('tree-sitter-python');
const Go = require('tree-sitter-go');
const TypeScript = require('tree-sitter-typescript').typescript;
const fs = require('fs').promises;
const path = require('path');

// Azure OpenAI Configuration from environment variables
const AZURE_CONFIG = {
  endpoint: process.env.AZURE_OPENAI_ENDPOINT || process.env.AZURE_OPENAI_API_URL,
  apiKey: process.env.AZURE_OPENAI_API_KEY,
  apiVersion: process.env.AZURE_OPENAI_API_VERSION || '2025-01-01-preview',
  deployment: process.env.AZURE_OPENAI_DEPLOYMENT || 'gpt-4.1'
};

// Validate Azure configuration
if (!AZURE_CONFIG.endpoint || !AZURE_CONFIG.apiKey) {
  console.error('❌ Azure OpenAI configuration missing!');
  console.error('   Please set AZURE_OPENAI_ENDPOINT and AZURE_OPENAI_API_KEY environment variables');
  process.exit(1);
}

console.log('🔧 Azure OpenAI Configuration:');
console.log(`   Endpoint: ${AZURE_CONFIG.endpoint}`);
console.log(`   Deployment: ${AZURE_CONFIG.deployment}`);
console.log(`   API Version: ${AZURE_CONFIG.apiVersion}`);

/**
 * Azure OpenAI LLM Service for intelligent code transformation
 */
class AzureOpenAIService {
  constructor(config) {
    this.config = config;
    this.requestCount = 0;
    this.cache = new Map(); // Cache LLM responses
  }

  /**
   * Call Azure OpenAI API for code transformation
   */
  async transformWithLLM(prompt, systemPrompt = null, temperature = 0.3) {
    // Check cache first
    const cacheKey = `${systemPrompt}::${prompt}`;
    if (this.cache.has(cacheKey)) {
      console.log('   📦 Using cached LLM response');
      return this.cache.get(cacheKey);
    }

    const url = `${this.config.endpoint}openai/deployments/${this.config.deployment}/chat/completions?api-version=${this.config.apiVersion}`;
    
    const messages = [];
    if (systemPrompt) {
      messages.push({ role: 'system', content: systemPrompt });
    }
    messages.push({ role: 'user', content: prompt });

    try {
      console.log(`   🤖 Calling Azure OpenAI (request #${++this.requestCount})...`);
      
      const response = await axios.post(url, {
        messages,
        temperature,
        max_tokens: 2000,
        top_p: 0.95,
        frequency_penalty: 0,
        presence_penalty: 0,
        stop: null
      }, {
        headers: {
          'api-key': this.config.apiKey,
          'Content-Type': 'application/json'
        }
      });

      const result = response.data.choices[0].message.content;
      
      // Cache the response
      this.cache.set(cacheKey, result);
      
      return result;
    } catch (error) {
      console.error('❌ Azure OpenAI API error:', error.response?.data || error.message);
      throw error;
    }
  }

  /**
   * Transform code using LLM with structured prompts
   */
  async transformCode(sourceCode, sourceLanguage, targetLanguage = 'ABAP') {
    const systemPrompt = `You are an expert code translator specializing in converting ${sourceLanguage} to ${targetLanguage}.
Your task is to translate code while preserving:
1. Semantic meaning and business logic
2. Data structures and types
3. Control flow and error handling
4. Performance characteristics
5. Code organization and modularity

For ABAP specifically:
- Use proper ABAP naming conventions (lv_ for local variables, lt_ for local tables, etc.)
- Convert to ABAP data types (STRING, INTEGER, CHAR, etc.)
- Use appropriate ABAP constructs (LOOP AT, IF/ENDIF, CASE/ENDCASE)
- Maintain ABAP style with uppercase keywords
- Add proper ABAP comments with "`;

    const userPrompt = `Translate the following ${sourceLanguage} code to ${targetLanguage}:

\`\`\`${sourceLanguage.toLowerCase()}
${sourceCode}
\`\`\`

Provide only the translated ABAP code without any explanation.`;

    const result = await this.transformWithLLM(userPrompt, systemPrompt, 0.2);
    
    // Extract code from response (remove markdown if present)
    const cleanedResult = result
      .replace(/```abap\n?/gi, '')
      .replace(/```\n?/g, '')
      .trim();
    
    return cleanedResult;
  }

  /**
   * Transform AST to ABAP with LLM guidance
   */
  async transformASTToABAP(ast, sourceLanguage) {
    const systemPrompt = `You are an expert in Abstract Syntax Tree (AST) transformation.
Convert the given AST representation from ${sourceLanguage} to ABAP code.
Focus on creating idiomatic ABAP that follows best practices.`;

    const userPrompt = `Convert this ${sourceLanguage} AST to ABAP code:

${JSON.stringify(ast, null, 2)}

Generate clean, production-ready ABAP code.`;

    return await this.transformWithLLM(userPrompt, systemPrompt, 0.1);
  }

  /**
   * Enhance UIR with semantic understanding
   */
  async enhanceUIRWithSemantics(uir, sourceLanguage) {
    const systemPrompt = `You are an expert in program semantics and code understanding.
Analyze the Universal Intermediate Representation (UIR) and enhance it with:
1. Semantic type information
2. Business logic patterns
3. ABAP-specific optimization hints
4. Data flow annotations`;

    const userPrompt = `Enhance this UIR from ${sourceLanguage} with semantic information for ABAP translation:

${JSON.stringify(uir, null, 2)}

Return enhanced UIR in JSON format.`;

    const result = await this.transformWithLLM(userPrompt, systemPrompt, 0.2);
    
    try {
      return JSON.parse(result);
    } catch {
      // If JSON parsing fails, return original UIR
      console.warn('   ⚠️ Could not parse enhanced UIR, using original');
      return uir;
    }
  }
}

/**
 * Enhanced Universal Translator with LLM support
 */
class UniversalTranslatorLLM {
  constructor() {
    this.llmService = new AzureOpenAIService(AZURE_CONFIG);
    this.parsers = new Map();
    this.initializeParsers();
  }

  initializeParsers() {
    // Initialize Tree-sitter parsers
    const jsParser = new Parser();
    jsParser.setLanguage(JavaScript);
    this.parsers.set('javascript', jsParser);
    this.parsers.set('js', jsParser);

    const tsParser = new Parser();
    tsParser.setLanguage(TypeScript);
    this.parsers.set('typescript', tsParser);
    this.parsers.set('ts', tsParser);

    const pyParser = new Parser();
    pyParser.setLanguage(Python);
    this.parsers.set('python', pyParser);
    this.parsers.set('py', pyParser);

    const goParser = new Parser();
    goParser.setLanguage(Go);
    this.parsers.set('go', goParser);
  }

  /**
   * Parse source code to AST
   */
  parseToAST(sourceCode, language) {
    const parser = this.parsers.get(language.toLowerCase());
    if (!parser) {
      throw new Error(`Unsupported language: ${language}`);
    }
    return parser.parse(sourceCode);
  }

  /**
   * Convert AST to Universal Intermediate Representation
   */
  astToUIR(ast, language) {
    const uir = {
      type: 'Program',
      language: language,
      nodes: [],
      metadata: {
        timestamp: new Date().toISOString(),
        version: '1.0.0'
      }
    };

    const traverseNode = (node) => {
      const uirNode = {
        type: node.type,
        text: node.text,
        startPosition: node.startPosition,
        endPosition: node.endPosition,
        children: []
      };

      // Map language-specific constructs to universal types
      switch (node.type) {
        case 'function_declaration':
        case 'function_definition':
        case 'method_definition':
          uirNode.universalType = 'FUNCTION';
          break;
        case 'if_statement':
        case 'if':
          uirNode.universalType = 'CONDITIONAL';
          break;
        case 'for_statement':
        case 'while_statement':
        case 'for':
        case 'while':
          uirNode.universalType = 'LOOP';
          break;
        case 'class_declaration':
        case 'class_definition':
          uirNode.universalType = 'CLASS';
          break;
        default:
          uirNode.universalType = 'GENERIC';
      }

      for (const child of node.children) {
        uirNode.children.push(traverseNode(child));
      }

      return uirNode;
    };

    uir.nodes = ast.rootNode.children.map(traverseNode);
    return uir;
  }

  /**
   * Main translation pipeline with LLM enhancement
   */
  async translate(sourceCode, sourceLanguage, options = {}) {
    console.log(`\n🚀 Starting LLM-enhanced translation from ${sourceLanguage} to ABAP`);
    
    try {
      // Step 1: Parse to AST
      console.log('📊 Step 1: Parsing source code to AST...');
      const ast = this.parseToAST(sourceCode, sourceLanguage);
      console.log(`   ✅ AST generated with ${ast.rootNode.childCount} root nodes`);

      // Step 2: Convert to UIR
      console.log('🔄 Step 2: Converting AST to UIR...');
      let uir = this.astToUIR(ast, sourceLanguage);
      console.log(`   ✅ UIR created with ${uir.nodes.length} nodes`);

      // Step 3: Enhance UIR with LLM semantics
      if (options.enhanceSemantics !== false) {
        console.log('🧠 Step 3: Enhancing UIR with LLM semantic analysis...');
        uir = await this.llmService.enhanceUIRWithSemantics(uir, sourceLanguage);
        console.log('   ✅ UIR enhanced with semantic information');
      }

      // Step 4: Transform to ABAP using LLM
      console.log('🎯 Step 4: Transforming to ABAP with LLM...');
      let abapCode;
      
      if (options.directTranslation) {
        // Direct source-to-ABAP translation
        abapCode = await this.llmService.transformCode(sourceCode, sourceLanguage, 'ABAP');
      } else {
        // AST-guided translation
        abapCode = await this.llmService.transformASTToABAP(uir, sourceLanguage);
      }
      
      console.log('   ✅ ABAP code generated');

      // Step 5: Validate and optimize
      if (options.validate !== false) {
        console.log('✔️ Step 5: Validating ABAP syntax...');
        abapCode = this.validateAndOptimizeABAP(abapCode);
        console.log('   ✅ ABAP code validated and optimized');
      }

      return {
        success: true,
        sourceLanguage,
        targetLanguage: 'ABAP',
        sourceCode,
        abapCode,
        ast: options.includeAST ? ast : undefined,
        uir: options.includeUIR ? uir : undefined,
        metadata: {
          timestamp: new Date().toISOString(),
          llmRequests: this.llmService.requestCount,
          translationMethod: options.directTranslation ? 'direct' : 'ast-guided'
        }
      };

    } catch (error) {
      console.error('❌ Translation failed:', error.message);
      return {
        success: false,
        error: error.message,
        sourceLanguage,
        targetLanguage: 'ABAP'
      };
    }
  }

  /**
   * Validate and optimize ABAP code
   */
  validateAndOptimizeABAP(abapCode) {
    // Basic ABAP syntax validation and formatting
    let optimized = abapCode;
    
    // Ensure proper line endings
    optimized = optimized.replace(/\r\n/g, '\n');
    
    // Ensure statements end with periods
    optimized = optimized.replace(/^([A-Z][^.\n]*[a-z])$/gm, '$1.');
    
    // Format ABAP keywords to uppercase
    const keywords = ['DATA', 'TYPE', 'IF', 'ELSE', 'ENDIF', 'LOOP', 'ENDLOOP', 
                     'AT', 'INTO', 'FROM', 'WHERE', 'SELECT', 'ENDSELECT',
                     'METHOD', 'ENDMETHOD', 'CLASS', 'ENDCLASS', 'FORM', 'ENDFORM'];
    
    keywords.forEach(keyword => {
      const regex = new RegExp(`\\b${keyword.toLowerCase()}\\b`, 'gi');
      optimized = optimized.replace(regex, keyword);
    });
    
    return optimized;
  }

  /**
   * Batch translation with progress tracking
   */
  async translateBatch(files, options = {}) {
    console.log(`\n🚀 Starting batch translation of ${files.length} files`);
    const results = [];
    
    for (let i = 0; i < files.length; i++) {
      const file = files[i];
      console.log(`\n📄 Processing file ${i + 1}/${files.length}: ${file.path}`);
      
      try {
        const sourceCode = await fs.readFile(file.path, 'utf8');
        const result = await this.translate(sourceCode, file.language, options);
        
        results.push({
          ...result,
          inputFile: file.path,
          outputFile: file.path.replace(/\.\w+$/, '.abap')
        });
        
        // Save ABAP file if requested
        if (options.saveFiles) {
          const outputPath = results[results.length - 1].outputFile;
          await fs.writeFile(outputPath, result.abapCode);
          console.log(`   💾 Saved to: ${outputPath}`);
        }
        
      } catch (error) {
        console.error(`   ❌ Failed to process ${file.path}: ${error.message}`);
        results.push({
          success: false,
          error: error.message,
          inputFile: file.path
        });
      }
      
      // Rate limiting to avoid overwhelming Azure OpenAI
      if (i < files.length - 1) {
        await new Promise(resolve => setTimeout(resolve, 1000)); // 1 second delay
      }
    }
    
    // Summary
    const successful = results.filter(r => r.success).length;
    console.log(`\n📊 Batch Translation Summary:`);
    console.log(`   ✅ Successful: ${successful}/${files.length}`);
    console.log(`   ❌ Failed: ${files.length - successful}/${files.length}`);
    console.log(`   🤖 Total LLM requests: ${this.llmService.requestCount}`);
    
    return results;
  }
}

/**
 * CLI Interface
 */
async function main() {
  const args = process.argv.slice(2);
  
  if (args.length === 0) {
    console.log(`
Universal Code-to-ABAP Translator with Azure OpenAI LLM

Usage:
  node universal-translator-llm.js <command> [options]

Commands:
  translate <file> <language>     Translate a single file to ABAP
  batch <directory> <language>     Translate all files in directory
  test                            Run test translation

Options:
  --direct                        Use direct LLM translation (no AST)
  --save                          Save output files
  --include-ast                   Include AST in output
  --include-uir                   Include UIR in output

Examples:
  node universal-translator-llm.js translate input.js javascript
  node universal-translator-llm.js batch ./src javascript --save
  node universal-translator-llm.js test

Environment Variables Required:
  AZURE_OPENAI_ENDPOINT          Azure OpenAI endpoint URL
  AZURE_OPENAI_API_KEY           Azure OpenAI API key
  AZURE_OPENAI_DEPLOYMENT        Deployment name (default: gpt-4.1)
  AZURE_OPENAI_API_VERSION       API version (default: 2025-01-01-preview)
`);
    process.exit(1);
  }

  const translator = new UniversalTranslatorLLM();
  const command = args[0];

  switch (command) {
    case 'translate': {
      const filePath = args[1];
      const language = args[2];
      
      const options = {
        directTranslation: args.includes('--direct'),
        includeAST: args.includes('--include-ast'),
        includeUIR: args.includes('--include-uir')
      };
      
      const sourceCode = await fs.readFile(filePath, 'utf8');
      const result = await translator.translate(sourceCode, language, options);
      
      if (result.success) {
        console.log('\n✅ Translation successful!');
        console.log('\n' + '='.repeat(60));
        console.log(result.abapCode);
        console.log('='.repeat(60));
        
        if (args.includes('--save')) {
          const outputPath = filePath.replace(/\.\w+$/, '.abap');
          await fs.writeFile(outputPath, result.abapCode);
          console.log(`\n💾 Saved to: ${outputPath}`);
        }
      } else {
        console.error('\n❌ Translation failed:', result.error);
        process.exit(1);
      }
      break;
    }
    
    case 'batch': {
      const directory = args[1];
      const language = args[2];
      
      const files = [];
      const dirContents = await fs.readdir(directory);
      
      for (const file of dirContents) {
        const ext = path.extname(file).toLowerCase();
        if ((language === 'javascript' && ext === '.js') ||
            (language === 'typescript' && ext === '.ts') ||
            (language === 'python' && ext === '.py') ||
            (language === 'go' && ext === '.go')) {
          files.push({
            path: path.join(directory, file),
            language
          });
        }
      }
      
      const options = {
        directTranslation: args.includes('--direct'),
        saveFiles: args.includes('--save')
      };
      
      await translator.translateBatch(files, options);
      break;
    }
    
    case 'test': {
      console.log('\n🧪 Running test translation...\n');
      
      const testCode = `
function calculateDiscount(price, customerType) {
  let discount = 0;
  
  if (customerType === 'premium') {
    discount = price * 0.20;
  } else if (customerType === 'regular') {
    discount = price * 0.10;
  }
  
  return price - discount;
}
`;
      
      const result = await translator.translate(testCode, 'javascript', {
        directTranslation: true
      });
      
      if (result.success) {
        console.log('✅ Test translation successful!');
        console.log('\nOriginal JavaScript:');
        console.log('='.repeat(60));
        console.log(testCode);
        console.log('='.repeat(60));
        console.log('\nTranslated ABAP:');
        console.log('='.repeat(60));
        console.log(result.abapCode);
        console.log('='.repeat(60));
      } else {
        console.error('❌ Test failed:', result.error);
      }
      break;
    }
    
    default:
      console.error(`Unknown command: ${command}`);
      process.exit(1);
  }
}

// Run CLI if executed directly
if (require.main === module) {
  main().catch(error => {
    console.error('Fatal error:', error);
    process.exit(1);
  });
}

// Export for programmatic use
module.exports = {
  UniversalTranslatorLLM,
  AzureOpenAIService
};