#!/bin/bash

# LLM-Enhanced Dataset Processing Script
# Uses Azure OpenAI for high-quality code translations

set -e

echo "🤖 LLM-Enhanced Universal Code Translation"
echo "=========================================="
echo "Using Azure OpenAI for intelligent transformations"
echo ""

# Check Azure configuration
if [ -z "$AZURE_OPENAI_ENDPOINT" ] || [ -z "$AZURE_OPENAI_API_KEY" ]; then
    echo "❌ Error: Azure OpenAI configuration missing!"
    echo "   Please set AZURE_OPENAI_ENDPOINT and AZURE_OPENAI_API_KEY"
    exit 1
fi

echo "✅ Azure OpenAI configured:"
echo "   Endpoint: $AZURE_OPENAI_ENDPOINT"
echo "   Deployment: ${AZURE_OPENAI_DEPLOYMENT:-gpt-4.1}"
echo ""

# Parse arguments
INPUT_DIR="${1:-datasets}"
OUTPUT_DIR="${2:-translations-llm}"
LANGUAGE="${3:-auto}"

mkdir -p "$OUTPUT_DIR"

echo "📂 Configuration:"
echo "   Input: $INPUT_DIR"
echo "   Output: $OUTPUT_DIR"
echo "   Language: $LANGUAGE"
echo ""

# Process each language
for lang in javascript python go typescript; do
    echo "🔄 Processing $lang files..."
    
    # Find all files for this language
    case $lang in
        javascript)
            FILES=$(find "$INPUT_DIR" -name "*.js" -type f 2>/dev/null | head -10)
            ;;
        typescript)
            FILES=$(find "$INPUT_DIR" -name "*.ts" -type f 2>/dev/null | head -10)
            ;;
        python)
            FILES=$(find "$INPUT_DIR" -name "*.py" -type f 2>/dev/null | head -10)
            ;;
        go)
            FILES=$(find "$INPUT_DIR" -name "*.go" -type f 2>/dev/null | head -10)
            ;;
    esac
    
    if [ -z "$FILES" ]; then
        echo "   ⚠️ No $lang files found, skipping..."
        continue
    fi
    
    # Count files
    FILE_COUNT=$(echo "$FILES" | wc -l | tr -d ' ')
    echo "   📊 Found $FILE_COUNT $lang files (processing max 10 for demo)"
    
    # Create language output directory
    LANG_OUTPUT="$OUTPUT_DIR/$lang"
    mkdir -p "$LANG_OUTPUT"
    
    # Process each file with LLM
    PROCESSED=0
    for FILE in $FILES; do
        if [ -f "$FILE" ]; then
            BASENAME=$(basename "$FILE")
            OUTPUT_FILE="$LANG_OUTPUT/${BASENAME%.*}.abap"
            
            echo "   📄 Translating: $BASENAME"
            
            # Use LLM-enhanced translator
            node universal-translator-llm.js translate "$FILE" "$lang" --direct --save > /dev/null 2>&1
            
            if [ $? -eq 0 ]; then
                # Move generated ABAP file to output directory
                GENERATED_ABAP="${FILE%.*}.abap"
                if [ -f "$GENERATED_ABAP" ]; then
                    mv "$GENERATED_ABAP" "$OUTPUT_FILE"
                    echo "      ✅ Saved to: $OUTPUT_FILE"
                    PROCESSED=$((PROCESSED + 1))
                else
                    echo "      ⚠️ Translation completed but file not found"
                fi
            else
                echo "      ❌ Translation failed"
            fi
            
            # Rate limiting to avoid overwhelming Azure OpenAI
            sleep 2
        fi
    done
    
    echo "   📊 Processed $PROCESSED $lang files"
    echo ""
done

# Generate summary report
echo "📊 Generating translation summary..."
python3 -c "
import os
import json
from pathlib import Path

output_dir = Path('$OUTPUT_DIR')
summary = {
    'languages': {},
    'total_files': 0,
    'total_size_kb': 0
}

for lang_dir in output_dir.iterdir():
    if lang_dir.is_dir():
        abap_files = list(lang_dir.glob('*.abap'))
        if abap_files:
            total_size = sum(f.stat().st_size for f in abap_files)
            summary['languages'][lang_dir.name] = {
                'files': len(abap_files),
                'size_kb': total_size / 1024,
                'avg_size_kb': (total_size / 1024) / len(abap_files) if abap_files else 0
            }
            summary['total_files'] += len(abap_files)
            summary['total_size_kb'] += total_size / 1024

# Print summary
print('\\n' + '=' * 60)
print('📊 LLM Translation Summary:')
print('=' * 60)
for lang, stats in summary['languages'].items():
    print(f'   {lang.upper()}:')
    print(f'      Files: {stats[\"files\"]}')
    print(f'      Total: {stats[\"size_kb\"]:.1f} KB')
    print(f'      Average: {stats[\"avg_size_kb\"]:.1f} KB/file')

print(f'\\n   🎯 TOTAL:')
print(f'      Files: {summary[\"total_files\"]}')
print(f'      Size: {summary[\"total_size_kb\"]:.1f} KB')

# Save summary
with open(output_dir / 'translation_summary.json', 'w') as f:
    json.dump(summary, f, indent=2)
print(f'\\n   💾 Summary saved to: {output_dir}/translation_summary.json')
"

echo ""
echo "✅ LLM-enhanced translation complete!"
echo ""
echo "🚀 Next Steps:"
echo "   1. Review translations in: $OUTPUT_DIR"
echo "   2. Generate training data: node dataset-generator.js generate --input $OUTPUT_DIR --levels 1-4"
echo "   3. Validate quality: node scripts/validate-translations.js validate $OUTPUT_DIR"
echo ""
echo "🤖 Azure OpenAI provided high-quality semantic translations!"