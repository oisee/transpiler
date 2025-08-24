# Universal Code-to-ABAP Translation Examples

This directory contains comprehensive examples demonstrating the universal code-to-ABAP translation system implementing Article 019 architecture.

## Directory Structure

```
examples/
├── javascript/           # JavaScript source code examples
├── python/              # Python source code examples  
├── go/                  # Go source code examples
├── java/                # Java source code examples
├── typescript/          # TypeScript source code examples
├── translations/        # Hand-crafted ABAP translations for reference
├── masked-examples/     # Article 020 masking strategy examples
└── README.md           # This file
```

## Example Categories

### 1. Source Code Examples

Each language directory contains realistic code examples demonstrating:

- **Basic Functions**: Simple function declarations, parameters, return values
- **Control Structures**: if/else, loops, switch statements, exception handling
- **Data Structures**: Arrays, objects, maps, structs
- **Object-Oriented**: Classes, methods, inheritance
- **Advanced Features**: Async/await, generics, closures

### 2. Translation Examples

The `translations/` directory contains hand-crafted ABAP translations showing:

- **Direct Translation**: How specific constructs map to ABAP
- **Idiomatic ABAP**: Best practices for generated code
- **Performance Optimization**: Efficient ABAP patterns
- **Comment Preservation**: Documentation in translated code

### 3. Masked Training Examples

The `masked-examples/` directory demonstrates Article 020's 4-level masking strategy:

#### Level 1: Expression-level Masking
- **File**: `level1-expression-masking.jsonl`
- **Focus**: Literals, variables, simple expressions
- **Difficulty**: Beginner to Intermediate
- **Purpose**: Teach basic syntax and expression patterns

#### Level 2: Statement-level Masking  
- **File**: `level2-statement-masking.jsonl`
- **Focus**: Complete statements while preserving control flow
- **Difficulty**: Intermediate to Advanced
- **Purpose**: Teach statement structure and logic flow

#### Level 3: Block-level Masking
- **File**: `level3-block-masking.jsonl`  
- **Focus**: Function bodies, loop bodies, conditional blocks
- **Difficulty**: Advanced
- **Purpose**: Teach algorithm implementation and code organization

#### Level 4: Method/Function-level Masking
- **File**: `level4-function-masking.jsonl`
- **Focus**: Entire functions while preserving signatures
- **Difficulty**: Expert
- **Purpose**: Teach complete function implementation

## Using the Examples

### 1. Manual Translation Testing

```bash
# Translate a single file
node universal-translator.js translate --file examples/javascript/basic-functions.js --output output.abap

# Translate entire directory
node universal-translator.js translate --dir examples/javascript --output translations/
```

### 2. Dataset Generation

```bash
# Generate training dataset from examples
node dataset-generator.js generate --input examples/javascript --output dataset.jsonl

# Generate with specific masking level
node dataset-generator.js generate --input examples/python --output dataset.jsonl --level 2
```

### 3. Quality Validation

```bash
# Validate existing dataset
node dataset-generator.js validate --input masked-examples/level1-expression-masking.jsonl --output validation-report.json
```

## Example Code Patterns

### JavaScript to ABAP Patterns

| JavaScript | ABAP | Notes |
|------------|------|-------|
| `function name(param)` | `FORM name USING p_param TYPE string` | Functions become FORMs |
| `let variable = value` | `DATA variable TYPE string VALUE value` | Variable declarations |
| `if (condition)` | `IF condition` | Control structures |
| `console.log(message)` | `WRITE message` | Output statements |
| `array.push(item)` | `APPEND item TO array` | Array operations |

### Python to ABAP Patterns

| Python | ABAP | Notes |
|--------|------|-------|
| `def name(param):` | `FORM name USING p_param TYPE string` | Functions become FORMs |
| `variable = value` | `DATA variable TYPE string VALUE value` | Variable assignments |
| `if condition:` | `IF condition` | Indentation to keywords |
| `print(message)` | `WRITE message` | Print statements |
| `list.append(item)` | `APPEND item TO list` | List operations |

### Go to ABAP Patterns

| Go | ABAP | Notes |
|----|------|-------|
| `func name(param string)` | `FORM name USING p_param TYPE string` | Functions become FORMs |
| `var variable string` | `DATA variable TYPE string` | Variable declarations |
| `if condition {` | `IF condition` | Braces to keywords |
| `fmt.Println(message)` | `WRITE message` | Print statements |
| `append(slice, item)` | `APPEND item TO slice` | Slice operations |

## Quality Metrics

The examples are designed to achieve:

- **Translation Accuracy**: >90% syntactic correctness
- **Semantic Preservation**: >85% logic equivalence  
- **ABAP Idiomaticity**: Modern ABAP patterns
- **Performance**: Efficient generated code
- **Maintainability**: Readable and documented output

## Training Data Format

Each masked example follows this JSONL format:

```json
{
  "id": "unique_identifier",
  "level": 1,
  "strategy": "expression",
  "sourceLanguage": "javascript", 
  "originalCode": "function example() { ... }",
  "maskedCode": "function example() { <MASK_1_1> }",
  "maskMap": {
    "<MASK_1_1>": {
      "type": "statement",
      "original": "return true;",
      "context": { ... }
    }
  },
  "abapCode": "FORM example.\n  \" Generated ABAP\nENDFORM.",
  "metadata": {
    "difficulty": "beginner",
    "complexity": { ... },
    "qualityScore": 95.2
  }
}
```

## Contributing

To add new examples:

1. **Source Code**: Add realistic code examples to appropriate language directories
2. **Translations**: Provide hand-crafted ABAP translations for complex examples  
3. **Masking**: Generate masked training pairs using the dataset generator
4. **Validation**: Ensure examples pass quality validation tests

## Best Practices

### Writing Source Examples

1. **Realistic Code**: Use production-like examples, not toy problems
2. **Common Patterns**: Focus on frequently used language constructs
3. **Progressive Complexity**: Start simple, build to advanced concepts
4. **Documentation**: Include clear comments explaining the purpose

### Creating ABAP Translations

1. **Idiomatic ABAP**: Use modern ABAP syntax and patterns
2. **Performance**: Consider ABAP runtime characteristics
3. **Maintainability**: Write readable, well-structured code
4. **Standards**: Follow SAP coding guidelines

### Generating Training Data

1. **Balanced Dataset**: Include examples from all difficulty levels
2. **Context Preservation**: Maintain semantic meaning in masked examples
3. **Quality Validation**: Ensure all examples pass round-trip tests
4. **Diversity**: Cover various programming paradigms and use cases

## Troubleshooting

### Common Translation Issues

1. **Memory Management**: ABAP has different memory model
2. **Object Orientation**: ABAP classes vs. other language objects
3. **Async Operations**: ABAP doesn't have native async/await
4. **Dynamic Typing**: ABAP is statically typed

### Quality Issues

1. **Low Translation Accuracy**: Improve parser rules and mappings
2. **Poor ABAP Quality**: Review transformation templates
3. **Failed Validation**: Check round-trip compatibility
4. **Context Loss**: Improve masking strategy preservation

For more details, see the main project documentation.