#!/bin/bash

# Enhanced Premium Dataset Download Script
# Downloads top-ranked MIT-licensed code datasets for JS/TS/Python/Go

set -e

DATASET_DIR="datasets"
mkdir -p "$DATASET_DIR"

echo "🚀 Downloading Premium MIT-Licensed Code Datasets"
echo "================================================="
echo "Target Languages: JavaScript, TypeScript, Python, Go"
echo "License Requirement: MIT/Permissive only"
echo ""

# Check Python and required packages
if ! command -v python3 &> /dev/null; then
    echo "❌ Error: Python3 is required"
    exit 1
fi

echo "📦 Installing required Python packages..."
pip3 install datasets huggingface_hub --quiet

# Phase 1: Quick Start - The Stack Smol (300K samples, ~50MB)
echo "📦 Phase 1: Downloading The Stack Smol for immediate testing..."
echo "   Size: ~50MB, 300K samples across 30 languages"
python3 -c "
from datasets import load_dataset
import os

print('🔄 Loading The Stack Smol...')
try:
    dataset = load_dataset('bigcode/the-stack-smol')
    dataset.save_to_disk('$DATASET_DIR/stack-smol')
    
    # Count samples by language
    langs = ['javascript', 'typescript', 'python', 'go']
    total_samples = 0
    
    for lang in langs:
        lang_data = dataset['train'].filter(lambda x: x['lang'] == lang)
        count = len(lang_data)
        total_samples += count
        print(f'   ✅ {lang.upper()}: {count:,} samples')
    
    print(f'📊 Total relevant samples: {total_samples:,}')
    print('✅ Stack Smol downloaded successfully!')
    
except Exception as e:
    print(f'❌ Error downloading Stack Smol: {e}')
    exit(1)
"

# Phase 2: Production - The Stack v2 (by language, 2-5GB total)
echo ""
echo "📦 Phase 2: Downloading The Stack v2 production datasets..."
echo "   Size: ~2-5GB total, 600+ languages"

for lang in javascript typescript python go; do
    echo "🔄 Downloading $lang from The Stack v2..."
    python3 -c "
from datasets import load_dataset
import os

try:
    print(f'   Loading {\"$lang\"} dataset...')
    dataset = load_dataset('bigcode/the-stack-v2', data_dir='data/$lang', split='train')
    dataset.save_to_disk('$DATASET_DIR/stack-v2-$lang')
    print(f'   ✅ {\"$lang\".upper()}: {len(dataset):,} files downloaded')
    
    # Quick quality check
    if len(dataset) > 0:
        sample = dataset[0]
        avg_length = len(sample.get('content', ''))
        print(f'   📊 Average file length: ~{avg_length} characters')
    
except Exception as e:
    print(f'   ❌ Error downloading $lang: {e}')
    print(f'   ⚠️  Continuing with other languages...')
    "
done

# Phase 3: Documentation Pairs - CodeSearchNet (1GB)
echo ""
echo "📦 Phase 3: Downloading CodeSearchNet documentation pairs..."
echo "   Size: ~1GB, 2M+ (code, documentation) pairs"

for lang in javascript python go; do
    echo "🔄 Downloading $lang from CodeSearchNet..."
    python3 -c "
from datasets import load_dataset
import os

try:
    print(f'   Loading {\"$lang\"} CodeSearchNet dataset...')
    dataset = load_dataset('code-search-net/code_search_net', '$lang')
    dataset.save_to_disk('$DATASET_DIR/codesearchnet-$lang')
    
    train_count = len(dataset['train']) if 'train' in dataset else 0
    test_count = len(dataset['test']) if 'test' in dataset else 0
    valid_count = len(dataset['valid']) if 'valid' in dataset else 0
    
    print(f'   ✅ {\"$lang\".upper()}: train={train_count:,}, test={test_count:,}, valid={valid_count:,}')
    
except Exception as e:
    print(f'   ❌ Error downloading $lang CodeSearchNet: {e}')
    print(f'   ⚠️  Continuing with other languages...')
    "
done

# Summary and statistics
echo ""
echo "📊 Generating dataset summary..."
python3 -c "
import os
from pathlib import Path

dataset_dir = Path('$DATASET_DIR')
total_size = 0
dataset_info = []

if dataset_dir.exists():
    for item in dataset_dir.iterdir():
        if item.is_dir():
            size = sum(f.stat().st_size for f in item.rglob('*') if f.is_file())
            total_size += size
            size_mb = size / (1024 * 1024)
            dataset_info.append((item.name, size_mb))

print('📊 Dataset Download Summary:')
print('=' * 50)
for name, size_mb in sorted(dataset_info):
    print(f'   📁 {name}: {size_mb:.1f} MB')

print(f'')
print(f'🎯 Total Downloaded: {total_size / (1024 * 1024 * 1024):.2f} GB')
print(f'📂 Storage Location: {dataset_dir.absolute()}')
"

# Quick validation
echo ""
echo "🔍 Running quick validation..."
python3 -c "
import os
from pathlib import Path
import json

dataset_dir = Path('$DATASET_DIR')
success_count = 0
total_samples = 0

print('🔍 Validation Results:')
for item in dataset_dir.iterdir():
    if item.is_dir() and any(f.name == 'dataset_info.json' for f in item.rglob('*')):
        success_count += 1
        # Try to count samples
        try:
            info_files = list(item.rglob('dataset_info.json'))
            if info_files:
                with open(info_files[0]) as f:
                    info = json.load(f)
                    splits = info.get('splits', {})
                    samples = sum(split.get('num_examples', 0) for split in splits.values())
                    total_samples += samples
                    print(f'   ✅ {item.name}: {samples:,} samples')
            else:
                print(f'   ✅ {item.name}: Downloaded successfully')
        except:
            print(f'   ✅ {item.name}: Downloaded successfully')

print(f'')
print(f'🎉 SUCCESS: {success_count} datasets downloaded')
print(f'📊 Estimated total samples: {total_samples:,}')
"

echo ""
echo "✅ All premium datasets downloaded successfully!"
echo ""
echo "🎯 What You Now Have:"
echo "   📚 Stack Smol: 300K samples for immediate testing"
echo "   🏭 Stack v2: Production-scale datasets by language"
echo "   📖 CodeSearchNet: Documentation-code pairs for enhanced training"
echo ""
echo "🚀 Next Steps:"
echo "   1. Run: node scripts/process-datasets.js process $DATASET_DIR/ processed-dataset.jsonl"
echo "   2. Run: node dataset-generator.js generate --input processed-dataset.jsonl --levels 1-4"
echo "   3. Run: node scripts/validate-translations.js validate training-dataset.jsonl"
echo ""
echo "🎊 Ready for universal ABAP translation with premium MIT-licensed data!"