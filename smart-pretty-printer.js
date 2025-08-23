#!/usr/bin/env node

/**
 * Smart ABAP Pretty Printer with Correct Spacing Rules
 * Preserves ABAP syntax by understanding where spaces are required vs forbidden
 */

const abaplint = require("@abaplint/core");
const fs = require("fs");

// ABAP Token Spacing Rules
const SPACING_RULES = {
  // NO SPACE BEFORE these tokens
  noSpaceBefore: [
    '.',      // Period - NEVER space before
    ',',      // Comma - NEVER space before  
    ')',      // Close paren
    ']',      // Close bracket
    '}',      // Close brace (string template)
    '->',     // Object operator
    '=>',     // Class operator
    '~',      // Component selector
  ],
  
  // NO SPACE AFTER these tokens
  noSpaceAfter: [
    '(',      // Open paren
    '[',      // Open bracket  
    '{',      // Open brace (string template)
    '->',     // Object operator
    '=>',     // Class operator
    '~',      // Component selector
    '@',      // Escaped identifier
    '|',      // String template delimiter (when starting)
    '!',      // Pragma prefix
    '#',      // Pragma prefix alternative
    '+',      // When used for offset (e.g., field+1(2))
  ],
  
  // NO SPACE AROUND (neither before nor after)
  noSpaceAround: [
    '-',      // When in identifiers like IS-INITIAL
  ],
  
  // SPECIAL CASES that need context
  contextual: {
    'DATA': (nextToken) => {
      // DATA( inline declaration - no space before (
      return nextToken !== '(';
    },
    'VALUE': (nextToken) => {
      // VALUE( ) constructor - no space before (
      return nextToken !== '(';
    },
    'NEW': (nextToken) => {
      // NEW #( ) or NEW class( ) - no space before (
      return nextToken !== '(' && nextToken !== '#';
    },
    'CAST': (nextToken) => {
      // CAST type( ) - no space before (
      return nextToken !== '(';
    },
    'CONV': (nextToken) => {
      // CONV type( ) - no space before (
      return nextToken !== '(';
    },
    'CORRESPONDING': (nextToken) => {
      // CORRESPONDING #( ) - no space before (
      return nextToken !== '(';
    },
    'REDUCE': (nextToken) => {
      // REDUCE type( ) - no space before (
      return nextToken !== '(';
    },
    'FILTER': (nextToken) => {
      // FILTER #( ) - no space before (
      return nextToken !== '(';
    },
    'REF': (nextToken) => {
      // REF #( ) - no space before (
      return nextToken !== '(';
    },
    ':': (nextToken, prevToken) => {
      // Colon in DATA: or METHODS: - space after but not before
      return false; // No space after colon in declarations
    },
    ',': (nextToken) => {
      // Comma at end of line in colon list - no space after if followed by period
      return nextToken !== '.';
    },
    '-': (nextToken, prevToken) => {
      // Minus in expressions vs identifiers
      if (prevToken && /^[a-zA-Z_]/.test(prevToken) && /^[a-zA-Z_]/.test(nextToken)) {
        // Part of identifier like IS-INITIAL
        return false;
      }
      return true; // Space in arithmetic
    },
    '+': (nextToken, prevToken) => {
      // Plus in offset notation field+offset(length)
      if (/^\d/.test(nextToken) && prevToken && /^[a-zA-Z_]/.test(prevToken)) {
        return false; // No space for offset
      }
      return true; // Space in arithmetic
    }
  },
  
  // Built-in functions that need spaces in parameters
  builtInFunctions: [
    'lines', 'strlen', 'xstrlen', 'numofchar', 'dbmaxlen',
    'abs', 'sign', 'ceil', 'floor', 'trunc', 'frac',
    'cos', 'sin', 'tan', 'acos', 'asin', 'atan',
    'sqrt', 'exp', 'log', 'log10',
    'to_upper', 'to_lower', 'to_mixed', 'from_mixed',
    'reverse', 'shift_left', 'shift_right',
    'condense', 'concat_lines_of', 'escape',
    'match', 'matches', 'contains', 'find'
  ]
};

class SmartABAPPrettyPrinter {
  constructor(options = {}) {
    this.options = {
      indent: "  ",
      uppercaseKeywords: true,
      preserveColonLists: true,
      ...options
    };
    this.reset();
  }
  
  reset() {
    this.indentLevel = 0;
    this.output = [];
    this.inColonList = false;
    this.colonListBase = null;
  }
  
  print(parseResult) {
    this.reset();
    const statements = parseResult.statements;
    
    for (let i = 0; i < statements.length; i++) {
      const stmt = statements[i];
      const formattedStmt = this.formatStatement(stmt, statements[i - 1], statements[i + 1]);
      this.output.push(formattedStmt);
    }
    
    return this.output.join("\n");
  }
  
  formatStatement(stmt, prevStmt, nextStmt) {
    const stmtType = stmt.get().constructor.name;
    const tokens = stmt.getTokens().map(t => t.getStr());
    
    // Adjust indentation for block closers
    if (this.isBlockCloser(stmtType)) {
      this.indentLevel = Math.max(0, this.indentLevel - 1);
    }
    
    // Handle special indentation for ELSE, ELSEIF, WHEN, CATCH
    let currentIndent = this.indentLevel;
    if (this.isIntermediateBlock(stmtType)) {
      currentIndent = Math.max(0, this.indentLevel - 1);
    }
    
    // Format the tokens with smart spacing
    let formattedTokens = this.joinTokensSmart(tokens);
    
    // Fix comma-period issue: if statement ends with comma, replace with period
    if (formattedTokens.endsWith(',')) {
      formattedTokens = formattedTokens.slice(0, -1); // Remove comma
    }
    if (formattedTokens.endsWith(',.')) {
      formattedTokens = formattedTokens.slice(0, -2); // Remove comma-period
    }
    
    const indentStr = this.options.indent.repeat(currentIndent);
    let result = indentStr + formattedTokens;
    
    // Ensure statement ends with period
    if (!result.trim().endsWith('.')) {
      result = result.trim() + '.';
    }
    
    // Increase indentation after block openers
    if (this.isBlockOpener(stmtType)) {
      this.indentLevel++;
    }
    
    // Re-increase after intermediate blocks
    if (this.isIntermediateBlock(stmtType)) {
      this.indentLevel++;
    }
    
    return result;
  }
  
  joinTokensSmart(tokens) {
    if (tokens.length === 0) return '';
    
    let result = '';
    
    for (let i = 0; i < tokens.length; i++) {
      const token = tokens[i];
      const prevToken = i > 0 ? tokens[i - 1] : null;
      const nextToken = i < tokens.length - 1 ? tokens[i + 1] : null;
      
      // Add the token
      result += token;
      
      // Determine if we need a space after this token
      if (i < tokens.length - 1) { // Not the last token
        if (this.needsSpaceAfter(token, nextToken, prevToken)) {
          result += ' ';
        }
      }
    }
    
    return result;
  }
  
  needsSpaceAfter(current, next, prev) {
    // Special case: never add space before period
    if (next === '.') {
      return false;
    }
    
    // Special case: never add space before comma
    if (next === ',') {
      return false;
    }
    
    // Check if current token should have no space after
    if (SPACING_RULES.noSpaceAfter.includes(current)) {
      return false;
    }
    
    // Check if next token should have no space before
    if (SPACING_RULES.noSpaceBefore.includes(next)) {
      return false;
    }
    
    // Check contextual rules for current token
    if (SPACING_RULES.contextual[current]) {
      const needsSpace = SPACING_RULES.contextual[current](next, prev);
      if (!needsSpace) {
        return false;
      }
    }
    
    // Special patterns
    
    // Pattern: DATA(var) - no space between DATA and (
    if (current === 'DATA' && next === '(') {
      return false;
    }
    
    // Pattern: VALUE(param) - no space
    if (current === 'VALUE' && next === '(') {
      return false;
    }
    
    // Pattern: METHOD( ) calls - no space before (
    // But for -> or => method calls, already handled
    if (next === '(' && (prev === '->' || prev === '=>')) {
      return false; // No space after -> or => before (
    }
    if (this.isMethodName(current) && next === '(') {
      return false;
    }
    
    // Pattern: -> or => operators
    if (current === '-' && next === '>') {
      return false;
    }
    if (current === '=' && next === '>') {
      return false;
    }
    
    // Pattern: Component selector ~
    if (current === '~' || next === '~') {
      return false;
    }
    
    // Pattern: String template |text|
    if (current === '|' || (prev === '|' && !next.startsWith('|'))) {
      // No space after opening | or inside template
      if (next === '{' || next === '}') {
        return true; // But space before { and after }
      }
      return !next.startsWith('|');
    }
    
    // Pattern: Offset notation field+1(2)
    if (current === '+' && /^\d/.test(next) && prev && /^[a-zA-Z_]/.test(prev)) {
      return false;
    }
    
    // Pattern: IS-INITIAL, IS-ASSIGNED etc AND component-field references
    if (current === '-' && prev && next) {
      // IS-INITIAL pattern
      if (prev === 'IS' && this.isABAPPredicate(next)) {
        return false;
      }
      // Component reference like ls_line-field
      if (/^[a-zA-Z_][a-zA-Z0-9_]*$/.test(prev) && /^[a-zA-Z_][a-zA-Z0-9_]*$/.test(next)) {
        return false;
      }
    }
    if (current === 'IS' && next === '-') {
      return false;
    }
    
    // Pattern: Brackets and array access
    if (current === '[' || next === ']') {
      return false;
    }
    
    // Pattern: Parentheses
    if (current === '(' || next === ')') {
      return false;
    }
    
    // Pattern: Colon in declarations
    if (current === ':') {
      return true; // Space after colon
    }
    
    // Pattern: Comma - usually needs space after (except at line end)
    if (current === ',') {
      return next !== '.'; // No space if followed by period
    }
    
    // Default: add space between tokens
    return true;
  }
  
  isMethodName(token) {
    // Check if token looks like a method name (lowercase or mixed case)
    return /^[a-z_][a-zA-Z0-9_]*$/.test(token) && 
           !this.isABAPKeyword(token);
  }
  
  isABAPKeyword(token) {
    const keywords = [
      'DATA', 'TYPE', 'TYPES', 'CONSTANTS', 'FIELD-SYMBOLS',
      'IF', 'ELSE', 'ELSEIF', 'ENDIF', 'CASE', 'WHEN', 'ENDCASE',
      'DO', 'ENDDO', 'WHILE', 'ENDWHILE', 'LOOP', 'ENDLOOP',
      'TRY', 'CATCH', 'ENDTRY', 'RAISE',
      'CLASS', 'ENDCLASS', 'METHOD', 'ENDMETHOD', 'INTERFACE', 'ENDINTERFACE',
      'FORM', 'ENDFORM', 'FUNCTION', 'ENDFUNCTION',
      'SELECT', 'ENDSELECT', 'FROM', 'WHERE', 'INTO', 'UP', 'TO',
      'AND', 'OR', 'NOT', 'EQ', 'NE', 'LT', 'LE', 'GT', 'GE',
      'IS', 'IN', 'BETWEEN', 'LIKE', 'AS',
      'MOVE', 'TO', 'WRITE', 'READ', 'TABLE', 'APPEND', 'INSERT', 'DELETE', 'MODIFY',
      'CALL', 'PERFORM', 'SUBMIT', 'LEAVE', 'EXIT', 'CONTINUE', 'CHECK',
      'IMPORTING', 'EXPORTING', 'CHANGING', 'RETURNING', 'RAISING',
      'BEGIN', 'END', 'OF', 'WITH', 'KEY', 'UNIQUE', 'NON-UNIQUE',
      'PUBLIC', 'PROTECTED', 'PRIVATE', 'SECTION',
      'STATIC', 'ABSTRACT', 'FINAL', 'CREATE', 'INHERITING'
    ];
    
    return keywords.includes(token.toUpperCase());
  }
  
  isABAPPredicate(token) {
    const predicates = [
      'INITIAL', 'BOUND', 'ASSIGNED', 'SUPPLIED', 'REQUESTED'
    ];
    return predicates.includes(token.toUpperCase());
  }
  
  isBlockOpener(stmtType) {
    return [
      'If', 'Loop', 'While', 'Do', 'Case',
      'Try', 'Form', 'Method', 'Class', 'Interface',
      'Function', 'Select'
    ].includes(stmtType);
  }
  
  isBlockCloser(stmtType) {
    return [
      'EndIf', 'EndLoop', 'EndWhile', 'EndDo', 'EndCase',
      'EndTry', 'EndForm', 'EndMethod', 'EndClass', 'EndInterface',
      'EndFunction', 'EndSelect'
    ].includes(stmtType);
  }
  
  isIntermediateBlock(stmtType) {
    return [
      'Else', 'ElseIf', 'When', 'Catch', 'Cleanup'
    ].includes(stmtType);
  }
}

// Test cases
const TEST_CASES = [
  {
    name: "DATA with colon list",
    input: `DATA: lv_first TYPE string,
      lv_second TYPE i,
      lv_third TYPE c LENGTH 10.`
  },
  {
    name: "Inline DATA declaration",
    input: `DATA(lv_result) = get_value( ).
DATA(lv_message) = |Hello { lv_name }!|.`
  },
  {
    name: "Method calls",
    input: `lo_object->method( param ).
cl_class=>static_method( VALUE #( field = 'test' ) ).`
  },
  {
    name: "String templates",
    input: `lv_text = |Count: { lv_count }|.
lv_msg = |{ lv_name } has { lines( lt_table ) } items|.`
  },
  {
    name: "IS-INITIAL predicate",
    input: `IF lv_value IS INITIAL.
  WRITE 'Empty'.
ENDIF.`
  },
  {
    name: "Offset and length",
    input: `lv_char = lv_string+5(1).
lv_sub = lv_text+lv_offset(lv_length).`
  },
  {
    name: "Component selector",
    input: `lv_value = ls_struc-field.
lv_data = <fs_wa>-component.
lt_table = lo_obj->get_table( )->*-entries.`
  }
];

// Test the smart pretty printer
function testSmartPrinter() {
  console.log("🔧 SMART ABAP PRETTY PRINTER TEST");
  console.log("=" .repeat(70));
  
  const printer = new SmartABAPPrettyPrinter();
  
  for (const testCase of TEST_CASES) {
    console.log(`\n📝 Test: ${testCase.name}`);
    console.log("-".repeat(40));
    console.log("Input:");
    console.log(testCase.input);
    
    try {
      // Parse the input - create new registry each time
      const parser = new abaplint.Registry();
      const file = new abaplint.MemoryFile("test.prog.abap", testCase.input);
      parser.addFile(file);
      parser.parse();
      
      const obj = parser.getFirstObject();
      const abapFile = obj?.getABAPFiles()[0];
      
      if (abapFile) {
        const parseResult = {
          statements: abapFile.getStatements()
        };
        
        // Pretty print with smart spacing
        const output = printer.print(parseResult);
        console.log("\nOutput:");
        console.log(output);
        
        // Check if we can re-parse successfully
        const file2 = new abaplint.MemoryFile("test2.prog.abap", output);
        const parser2 = new abaplint.Registry();
        parser2.addFile(file2);
        parser2.parse();
        
        const issues = parser2.findIssues().filter(i => 
          i.getSeverity() === abaplint.Severity.Error
        );
        
        if (issues.length === 0) {
          console.log("✅ Re-parsing successful!");
        } else {
          console.log("⚠️  Re-parsing issues:");
          issues.forEach(i => console.log(`  - ${i.getMessage()}`));
        }
      }
    } catch (error) {
      console.log(`❌ Error: ${error.message}`);
    }
  }
}

// Export for use in other modules
module.exports = { SmartABAPPrettyPrinter };

// Run tests if executed directly
if (require.main === module) {
  testSmartPrinter();
}