# Universal Code-to-ABAP Translation Scripts

This directory contains production-ready scripts for downloading, processing, and validating large-scale code datasets for ABAP translation training.

## Scripts Overview

### 1. download-datasets.sh
**Purpose**: Download popular open-source code repositories and packages

**Features**:
- Downloads from npm (JavaScript), PyPI (Python), GitHub (Go, Java, TypeScript)
- Rate limiting and respectful API usage
- Configurable language selection
- Progress tracking and resumption
- Automatic cleanup and organization

**Usage**:
```bash
# Download all languages
./download-datasets.sh

# Download specific languages
./download-datasets.sh --javascript --python --go

# Download with custom options
./download-datasets.sh --javascript --no-cleanup
```

**Output**: Organized source code files in `../data/` directory

### 2. process-datasets.js
**Purpose**: Convert downloaded source code into training-ready dataset format

**Features**:
- Batch processing with configurable concurrency
- Progress tracking and checkpoint resumption
- Quality validation and filtering
- Multiple output formats (JSONL, JSON, CSV)
- Memory-efficient streaming for large datasets

**Usage**:
```bash
# Process all downloaded data
node process-datasets.js process ../data dataset.jsonl

# Process with custom settings
node process-datasets.js process ../data dataset.jsonl \
  --concurrency 10 \
  --batch-size 200 \
  --quality-threshold 70

# Analyze existing dataset
node process-datasets.js analyze dataset.jsonl --output analysis.json
```

**Output**: Training dataset in JSONL format with masked examples

### 3. validate-translations.js
**Purpose**: Validate translation quality through comprehensive testing

**Features**:
- Syntax validation of generated ABAP
- Round-trip testing (ABAP → AST → ABAP)
- Semantic equivalence checking (60% threshold)
- Performance benchmarking
- Detailed quality metrics and reporting

**Usage**:
```bash
# Validate complete dataset
node validate-translations.js validate dataset.jsonl --output-report validation-report.json

# Quick syntax check
node validate-translations.js syntax-check generated.abap

# Run performance benchmark
node validate-translations.js benchmark --builtin
```

**Output**: Validation reports and quality metrics

## Workflow

### Complete Pipeline

```bash
# 1. Download datasets
./scripts/download-datasets.sh --javascript --python --go

# 2. Process into training format
node scripts/process-datasets.js process data/ training-dataset.jsonl \
  --concurrency 5 \
  --quality-threshold 60

# 3. Validate quality
node scripts/validate-translations.js validate training-dataset.jsonl \
  --output-report validation-report.json \
  --sample-size 1000

# 4. Analyze results
node scripts/process-datasets.js analyze training-dataset.jsonl \
  --output dataset-analysis.json
```

### Incremental Processing

```bash
# Resume interrupted processing
node scripts/process-datasets.js process data/ training-dataset.jsonl
# Automatically resumes from last checkpoint

# Add new languages to existing dataset
./scripts/download-datasets.sh --typescript --java
node scripts/process-datasets.js process data/ training-dataset-extended.jsonl
```

### Quality Assurance

```bash
# Validate specific language or strategy
node scripts/validate-translations.js validate dataset.jsonl \
  --sample-size 500 \
  --no-performance

# Check syntax of generated ABAP
node scripts/validate-translations.js syntax-check dataset.jsonl

# Performance benchmarking
node scripts/validate-translations.js benchmark --builtin
```

## Configuration

### Download Script Configuration

Environment variables:
```bash
export DATA_DIR="../data"              # Download directory
export MAX_CONCURRENCY=5              # Concurrent downloads
export DOWNLOAD_TIMEOUT=30            # Timeout in seconds
export RATE_LIMIT_DELAY=2             # Delay between requests
```

### Processing Configuration

Common options:
```bash
--concurrency 5                       # Parallel file processors
--batch-size 100                      # Files per batch
--quality-threshold 60                # Minimum quality score
--max-file-size 1048576               # 1MB file size limit
--no-validation                       # Skip quality validation
--no-resume                          # Don't use checkpoints
```

### Validation Configuration

Quality thresholds:
```bash
--sample-size 1000                    # Items to validate (0 = all)
--timeout 30000                       # Timeout per validation (ms)
--no-roundtrip                       # Skip round-trip testing
--no-semantic                        # Skip semantic validation
```

## Output Formats

### Dataset Format (JSONL)

Each line contains a training example:

```json
{
  "id": "unique_identifier",
  "sourceLanguage": "javascript",
  "originalCode": "function add(a, b) { return a + b; }",
  "maskedCode": "function add(<MASK_1_1>, <MASK_1_2>) { return <MASK_1_3>; }",
  "maskMap": {
    "<MASK_1_1>": {
      "type": "identifier",
      "original": "a",
      "context": {...}
    }
  },
  "abapCode": "FORM add USING p_a TYPE string p_b TYPE string...",
  "translationSuccess": true,
  "metadata": {
    "strategy": "expression",
    "level": 1,
    "translationTime": 150,
    "confidence": 0.95
  },
  "quality": {
    "overall": 87.5,
    "masking": 90,
    "translation": 85,
    "roundTrip": 88,
    "valid": true
  }
}
```

### Validation Report Format

```json
{
  "timestamp": "2024-01-01T12:00:00Z",
  "summary": {
    "totalTests": 1000,
    "passRate": 0.847,
    "syntaxValidation": {
      "passRate": 0.923,
      "errorCount": 77
    },
    "roundTripTesting": {
      "passRate": 0.856,
      "averageEquivalence": 0.743
    },
    "semanticEquivalence": {
      "passRate": 0.678,
      "above60Percent": 678
    }
  }
}
```

## Performance Optimization

### Memory Management

For large datasets:
```bash
# Use smaller batch sizes
--batch-size 50

# Reduce concurrency
--concurrency 2

# Enable streaming mode
--stream-processing
```

### Disk Usage

Monitor disk space:
```bash
# Check dataset size
du -sh data/

# Cleanup temporary files
./download-datasets.sh --cleanup-only
```

### Processing Speed

Optimize for speed:
```bash
# Increase concurrency (if memory allows)
--concurrency 10

# Skip validation during processing
--no-validation

# Use larger batches
--batch-size 500
```

## Quality Metrics

### Translation Quality

- **Syntax Validation**: >90% ABAP code should parse successfully
- **Round-trip Testing**: >80% should maintain AST equivalence
- **Semantic Equivalence**: >60% should preserve core semantics
- **Overall Quality**: Target >75% average quality score

### Dataset Quality

- **Coverage**: All major language constructs represented
- **Balance**: Even distribution across difficulty levels
- **Diversity**: Multiple programming paradigms and patterns
- **Size**: Minimum 10k examples per language for effective training

### Performance Benchmarks

- **Translation Speed**: <200ms average per code snippet
- **Validation Speed**: <100ms average per example
- **Memory Usage**: <2GB peak memory for processing
- **Disk Efficiency**: <10MB per 1000 training examples

## Troubleshooting

### Common Issues

1. **Download Failures**
   ```bash
   # Check network connectivity
   curl -I https://api.github.com
   
   # Retry with different rate limits
   ./download-datasets.sh --retry-failed
   ```

2. **Processing Errors**
   ```bash
   # Check logs
   cat data/logs/processing.log
   
   # Resume from checkpoint
   node process-datasets.js process data/ dataset.jsonl
   ```

3. **Validation Failures**
   ```bash
   # Check specific errors
   node validate-translations.js syntax-check dataset.jsonl
   
   # Reduce sample size
   --sample-size 100
   ```

4. **Memory Issues**
   ```bash
   # Monitor memory usage
   node --max-old-space-size=8192 process-datasets.js ...
   
   # Use smaller batches
   --batch-size 25 --concurrency 2
   ```

### Error Recovery

- **Interrupted Downloads**: Scripts automatically resume from last successful download
- **Processing Failures**: Checkpoint system allows resumption from any point
- **Validation Errors**: Continue processing remaining items, report failures

### Performance Tuning

1. **CPU-bound**: Increase `--concurrency`
2. **Memory-bound**: Decrease `--batch-size`
3. **I/O-bound**: Balance concurrency and batch size
4. **Network-bound**: Adjust rate limiting in download script

## Integration

### With Existing Systems

```javascript
// Import processing functions
import { DatasetProcessor, DatasetAnalyzer } from './scripts/process-datasets.js';
import { TranslationValidator } from './scripts/validate-translations.js';

// Use in your application
const processor = new DatasetProcessor(config);
const result = await processor.processDatasets(inputDir, outputPath);
```

### With CI/CD Pipelines

```yaml
# GitHub Actions example
- name: Download datasets
  run: ./scripts/download-datasets.sh --javascript --python

- name: Process datasets
  run: node scripts/process-datasets.js process data/ dataset.jsonl --quality-threshold 70

- name: Validate quality
  run: node scripts/validate-translations.js validate dataset.jsonl --sample-size 1000
```

### With Training Pipelines

```bash
# Generate training data
./generate-training-data.sh

# Split into train/test/validation
python split-dataset.py dataset.jsonl --train 0.8 --test 0.1 --val 0.1

# Train model
python train-model.py --dataset train-dataset.jsonl
```

For more details on specific components, see the main project documentation and individual script help messages.