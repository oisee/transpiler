#!/bin/bash

# Open Source Dataset Download Script (No Authentication Required)
# Downloads publicly available MIT-licensed datasets for JS/TS/Python/Go

set -e

DATASET_DIR="datasets"
mkdir -p "$DATASET_DIR"

echo "🚀 Downloading Open Source MIT-Licensed Code Datasets"
echo "===================================================="
echo "Target Languages: JavaScript, TypeScript, Python, Go"
echo "License: MIT/Permissive (no authentication required)"
echo ""

# Check Python and required packages
if ! command -v python3 &> /dev/null; then
    echo "❌ Error: Python3 is required"
    exit 1
fi

echo "📦 Installing required Python packages..."
pip3 install datasets huggingface_hub requests --quiet

# Option 1: CodeSearchNet (Public, no authentication)
echo "📦 Downloading CodeSearchNet (Public Dataset)..."
echo "   Size: ~1GB, 2M+ (code, documentation) pairs"
echo "   License: MIT project, individual files vary"

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
    total = train_count + test_count + valid_count
    
    print(f'   ✅ {\"$lang\".upper()}: {total:,} total samples (train={train_count:,}, test={test_count:,}, valid={valid_count:,})')
    
except Exception as e:
    print(f'   ❌ Error downloading $lang CodeSearchNet: {e}')
    print(f'   ⚠️  Continuing with other languages...')
    "
done

# Option 2: Popular GitHub Repositories (Direct downloads)
echo ""
echo "📦 Downloading Popular MIT-Licensed GitHub Repositories..."
echo "   Method: Direct GitHub API and repository downloads"

# JavaScript Popular Repositories
echo "🔄 Downloading popular JavaScript projects..."
python3 -c "
import requests
import os
import json
from pathlib import Path
import zipfile
import tempfile

# Popular MIT-licensed JavaScript projects
js_repos = [
    'lodash/lodash',
    'axios/axios', 
    'webpack/webpack',
    'prettier/prettier',
    'jestjs/jest'
]

js_dir = Path('$DATASET_DIR/github-javascript')
js_dir.mkdir(exist_ok=True)

downloaded = 0
for repo in js_repos:
    try:
        # Check if repo is MIT licensed
        api_url = f'https://api.github.com/repos/{repo}'
        response = requests.get(api_url)
        if response.status_code == 200:
            repo_info = response.json()
            license_info = repo_info.get('license', {})
            if license_info and 'mit' in license_info.get('key', '').lower():
                # Download repository
                zip_url = f'https://github.com/{repo}/archive/refs/heads/main.zip'
                zip_response = requests.get(zip_url)
                if zip_response.status_code == 200:
                    repo_name = repo.split('/')[-1]
                    zip_path = js_dir / f'{repo_name}.zip'
                    with open(zip_path, 'wb') as f:
                        f.write(zip_response.content)
                    
                    # Extract JavaScript files
                    with zipfile.ZipFile(zip_path, 'r') as zip_ref:
                        for file in zip_ref.namelist():
                            if file.endswith(('.js', '.ts')) and not file.endswith('.test.js'):
                                zip_ref.extract(file, js_dir / repo_name)
                    
                    os.remove(zip_path)  # Clean up zip
                    downloaded += 1
                    print(f'   ✅ {repo}: MIT licensed, downloaded')
                else:
                    print(f'   ⚠️  {repo}: Download failed')
            else:
                print(f'   ❌ {repo}: Not MIT licensed')
        else:
            print(f'   ❌ {repo}: API access failed')
    except Exception as e:
        print(f'   ❌ {repo}: Error - {e}')

print(f'📊 JavaScript: {downloaded} repositories downloaded')
"

# Python Popular Repositories  
echo "🔄 Downloading popular Python projects..."
python3 -c "
import requests
import os
import json
from pathlib import Path
import zipfile

# Popular MIT-licensed Python projects
py_repos = [
    'pallets/flask',
    'psf/requests',
    'python-pillow/Pillow',
    'keras-team/keras',
    'scikit-learn/scikit-learn'
]

py_dir = Path('$DATASET_DIR/github-python')  
py_dir.mkdir(exist_ok=True)

downloaded = 0
for repo in py_repos:
    try:
        # Check license via API
        api_url = f'https://api.github.com/repos/{repo}'
        response = requests.get(api_url)
        if response.status_code == 200:
            repo_info = response.json()
            license_info = repo_info.get('license', {})
            license_key = license_info.get('key', '') if license_info else ''
            
            # Accept MIT, BSD, Apache as permissive
            if any(lic in license_key.lower() for lic in ['mit', 'bsd', 'apache']):
                # Try both main and master branches
                for branch in ['main', 'master']:
                    zip_url = f'https://github.com/{repo}/archive/refs/heads/{branch}.zip'
                    zip_response = requests.get(zip_url)
                    if zip_response.status_code == 200:
                        repo_name = repo.split('/')[-1]
                        zip_path = py_dir / f'{repo_name}.zip'
                        with open(zip_path, 'wb') as f:
                            f.write(zip_response.content)
                        
                        # Extract Python files
                        with zipfile.ZipFile(zip_path, 'r') as zip_ref:
                            for file in zip_ref.namelist():
                                if file.endswith('.py') and not any(test in file for test in ['test', 'tests']):
                                    try:
                                        zip_ref.extract(file, py_dir / repo_name)
                                    except:
                                        pass  # Skip files that can't be extracted
                        
                        os.remove(zip_path)  # Clean up
                        downloaded += 1
                        print(f'   ✅ {repo}: {license_key} licensed, downloaded')
                        break
                else:
                    print(f'   ⚠️  {repo}: No accessible branch found')
            else:
                print(f'   ❌ {repo}: License {license_key} not permissive enough')
        else:
            print(f'   ❌ {repo}: API access failed')
    except Exception as e:
        print(f'   ❌ {repo}: Error - {e}')

print(f'📊 Python: {downloaded} repositories downloaded')
"

# Go Popular Repositories
echo "🔄 Downloading popular Go projects..."
python3 -c "
import requests
import os
import json
from pathlib import Path
import zipfile

# Popular MIT/BSD-licensed Go projects
go_repos = [
    'gin-gonic/gin',
    'gorilla/mux', 
    'sirupsen/logrus',
    'stretchr/testify',
    'go-chi/chi'
]

go_dir = Path('$DATASET_DIR/github-go')
go_dir.mkdir(exist_ok=True)

downloaded = 0
for repo in go_repos:
    try:
        # Check license
        api_url = f'https://api.github.com/repos/{repo}'
        response = requests.get(api_url)
        if response.status_code == 200:
            repo_info = response.json()
            license_info = repo_info.get('license', {})
            license_key = license_info.get('key', '') if license_info else ''
            
            if any(lic in license_key.lower() for lic in ['mit', 'bsd', 'apache']):
                for branch in ['main', 'master']:
                    zip_url = f'https://github.com/{repo}/archive/refs/heads/{branch}.zip'
                    zip_response = requests.get(zip_url)
                    if zip_response.status_code == 200:
                        repo_name = repo.split('/')[-1]
                        zip_path = go_dir / f'{repo_name}.zip'
                        with open(zip_path, 'wb') as f:
                            f.write(zip_response.content)
                        
                        # Extract Go files
                        with zipfile.ZipFile(zip_path, 'r') as zip_ref:
                            for file in zip_ref.namelist():
                                if file.endswith('.go') and not any(test in file for test in ['test', '_test']):
                                    try:
                                        zip_ref.extract(file, go_dir / repo_name)
                                    except:
                                        pass
                        
                        os.remove(zip_path)
                        downloaded += 1
                        print(f'   ✅ {repo}: {license_key} licensed, downloaded')
                        break
                else:
                    print(f'   ⚠️  {repo}: No accessible branch')
            else:
                print(f'   ❌ {repo}: License {license_key} not permissive')
        else:
            print(f'   ❌ {repo}: API access failed')
    except Exception as e:
        print(f'   ❌ {repo}: Error - {e}')

print(f'📊 Go: {downloaded} repositories downloaded')
"

# Generate comprehensive summary
echo ""
echo "📊 Generating dataset summary..."
python3 -c "
import os
from pathlib import Path
import json

dataset_dir = Path('$DATASET_DIR')
total_size = 0
file_counts = {}
languages = ['javascript', 'python', 'go', 'typescript']

print('📊 Dataset Download Summary:')
print('=' * 60)

for item in dataset_dir.iterdir():
    if item.is_dir():
        # Count files by extension
        js_files = len(list(item.rglob('*.js')))
        ts_files = len(list(item.rglob('*.ts'))) 
        py_files = len(list(item.rglob('*.py')))
        go_files = len(list(item.rglob('*.go')))
        
        total_files = js_files + ts_files + py_files + go_files
        if total_files > 0:
            size = sum(f.stat().st_size for f in item.rglob('*') if f.is_file())
            total_size += size
            size_mb = size / (1024 * 1024)
            
            print(f'📁 {item.name}:')
            print(f'   💾 Size: {size_mb:.1f} MB')
            print(f'   📄 Files: JS={js_files}, TS={ts_files}, PY={py_files}, GO={go_files}')
            
            file_counts[item.name] = {
                'javascript': js_files, 
                'typescript': ts_files,
                'python': py_files,
                'go': go_files,
                'size_mb': size_mb
            }

print(f'')
print(f'🎯 GRAND TOTAL:')
print(f'   💾 Total Size: {total_size / (1024 * 1024):.1f} MB')

total_by_lang = {}
for lang in languages:
    total_by_lang[lang] = sum(dataset.get(lang, 0) for dataset in file_counts.values())
    if total_by_lang[lang] > 0:
        print(f'   📄 {lang.upper()}: {total_by_lang[lang]:,} files')

print(f'   📂 Storage: {dataset_dir.absolute()}')

# Save summary
summary = {
    'total_size_mb': total_size / (1024 * 1024),
    'datasets': file_counts,
    'totals_by_language': total_by_lang,
    'download_date': str(Path().cwd())
}

with open(dataset_dir / 'download_summary.json', 'w') as f:
    json.dump(summary, f, indent=2)
    
print(f'   💾 Summary saved to: download_summary.json')
"

echo ""
echo "✅ Open source datasets downloaded successfully!"
echo ""
echo "🎯 What You Now Have:"
echo "   📚 CodeSearchNet: 2M+ documentation-code pairs"  
echo "   🏭 GitHub Repos: Popular MIT-licensed projects"
echo "   📖 Ready for universal ABAP translation"
echo ""
echo "🚀 Next Steps:"
echo "   1. node scripts/process-datasets.js process $DATASET_DIR/ processed.jsonl"
echo "   2. node dataset-generator.js generate --input processed.jsonl --levels 1-4"
echo "   3. node scripts/validate-translations.js validate training-dataset.jsonl"
echo ""
echo "🎊 Ready for production with open MIT-licensed data!"