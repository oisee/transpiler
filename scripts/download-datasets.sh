#!/bin/bash

# Universal Code-to-ABAP Translation System
# Dataset Downloader Script
# 
# Downloads popular open-source datasets for training:
# - JavaScript from npm registry
# - Python from PyPI 
# - Go from GitHub
# - Java from Maven Central
# - And other language repositories

set -e

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DATA_DIR="${SCRIPT_DIR}/../data"
TEMP_DIR="${DATA_DIR}/temp"
LOG_FILE="${DATA_DIR}/download.log"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging function
log() {
    echo -e "${BLUE}[$(date '+%Y-%m-%d %H:%M:%S')]${NC} $1" | tee -a "$LOG_FILE"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1" | tee -a "$LOG_FILE"
}

success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1" | tee -a "$LOG_FILE"
}

warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1" | tee -a "$LOG_FILE"
}

# Check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Setup directories
setup_directories() {
    log "Setting up directories..."
    
    mkdir -p "$DATA_DIR"/{javascript,python,go,java,typescript,c,cpp,rust,ruby}
    mkdir -p "$TEMP_DIR"
    mkdir -p "$DATA_DIR/logs"
    
    success "Directories created"
}

# Check dependencies
check_dependencies() {
    log "Checking dependencies..."
    
    local missing_deps=()
    
    if ! command_exists curl; then
        missing_deps+=("curl")
    fi
    
    if ! command_exists git; then
        missing_deps+=("git")
    fi
    
    if ! command_exists unzip; then
        missing_deps+=("unzip")
    fi
    
    if ! command_exists tar; then
        missing_deps+=("tar")
    fi
    
    if ! command_exists jq; then
        warning "jq is not installed. JSON parsing will be limited."
    fi
    
    if [ ${#missing_deps[@]} -ne 0 ]; then
        error "Missing required dependencies: ${missing_deps[*]}"
        error "Please install missing dependencies and run again"
        exit 1
    fi
    
    success "All dependencies satisfied"
}

# Download JavaScript packages from npm
download_javascript() {
    log "Downloading popular JavaScript packages..."
    
    local js_packages=(
        "lodash"
        "react"
        "vue"
        "angular"
        "express"
        "axios"
        "moment"
        "underscore"
        "jquery"
        "chalk"
        "commander"
        "inquirer"
        "fs-extra"
        "glob"
        "yargs"
        "cheerio"
        "request"
        "socket.io"
        "mongoose"
        "webpack"
    )
    
    local js_dir="$DATA_DIR/javascript"
    
    for package in "${js_packages[@]}"; do
        log "Downloading $package..."
        
        local temp_file="$TEMP_DIR/${package}.tar.gz"
        local extract_dir="$TEMP_DIR/${package}"
        
        # Get package info from npm registry
        local registry_url="https://registry.npmjs.org/${package}/latest"
        
        if curl -s "$registry_url" | jq -e '.tarball' >/dev/null 2>&1; then
            local tarball_url=$(curl -s "$registry_url" | jq -r '.dist.tarball')
            
            # Download tarball
            if curl -L -o "$temp_file" "$tarball_url" 2>/dev/null; then
                # Extract
                mkdir -p "$extract_dir"
                tar -xzf "$temp_file" -C "$extract_dir" --strip-components=1 2>/dev/null || true
                
                # Copy source files
                find "$extract_dir" -name "*.js" -not -path "*/node_modules/*" -not -path "*/test/*" -not -path "*/tests/*" | \
                    head -50 | \
                    while read -r file; do
                        local basename=$(basename "$file")
                        cp "$file" "$js_dir/${package}_${basename}" 2>/dev/null || true
                    done
                
                # Cleanup
                rm -rf "$extract_dir" "$temp_file"
                
                success "Downloaded $package"
            else
                warning "Failed to download $package"
            fi
        else
            warning "Package $package not found or invalid"
        fi
        
        # Rate limiting
        sleep 1
    done
}

# Download Python packages from PyPI
download_python() {
    log "Downloading popular Python packages..."
    
    local python_packages=(
        "requests"
        "numpy"
        "pandas"
        "flask"
        "django"
        "tensorflow"
        "scikit-learn"
        "matplotlib"
        "beautifulsoup4"
        "selenium"
        "pytest"
        "black"
        "fastapi"
        "sqlalchemy"
        "pydantic"
        "click"
        "rich"
        "typer"
        "httpx"
        "aiohttp"
    )
    
    local py_dir="$DATA_DIR/python"
    
    for package in "${python_packages[@]}"; do
        log "Downloading $package..."
        
        local package_url="https://pypi.org/pypi/${package}/json"
        
        if curl -s "$package_url" | jq -e '.urls[0].url' >/dev/null 2>&1; then
            # Get source distribution URL
            local source_url=$(curl -s "$package_url" | jq -r '.urls[] | select(.packagetype=="sdist") | .url' | head -1)
            
            if [ -n "$source_url" ] && [ "$source_url" != "null" ]; then
                local temp_file="$TEMP_DIR/${package}.tar.gz"
                local extract_dir="$TEMP_DIR/${package}"
                
                # Download source
                if curl -L -o "$temp_file" "$source_url" 2>/dev/null; then
                    # Extract
                    mkdir -p "$extract_dir"
                    tar -xzf "$temp_file" -C "$extract_dir" --strip-components=1 2>/dev/null || true
                    
                    # Copy Python files
                    find "$extract_dir" -name "*.py" -not -path "*/test*/*" -not -path "*/example*/*" | \
                        head -30 | \
                        while read -r file; do
                            local basename=$(basename "$file")
                            cp "$file" "$py_dir/${package}_${basename}" 2>/dev/null || true
                        done
                    
                    # Cleanup
                    rm -rf "$extract_dir" "$temp_file"
                    
                    success "Downloaded $package"
                else
                    warning "Failed to download $package"
                fi
            else
                warning "No source distribution found for $package"
            fi
        else
            warning "Package $package not found on PyPI"
        fi
        
        # Rate limiting
        sleep 1
    done
}

# Download Go packages from GitHub
download_go() {
    log "Downloading popular Go packages..."
    
    local go_repos=(
        "gin-gonic/gin"
        "gorilla/mux"
        "sirupsen/logrus"
        "spf13/cobra"
        "spf13/viper"
        "gin-gonic/gin"
        "go-kit/kit"
        "hashicorp/terraform"
        "kubernetes/kubernetes"
        "docker/docker"
        "prometheus/prometheus"
        "grafana/grafana"
        "influxdata/influxdb"
        "etcd-io/etcd"
        "hashicorp/consul"
        "mitchellh/mapstructure"
        "stretchr/testify"
        "golang/go"
        "uber-go/zap"
        "go-redis/redis"
    )
    
    local go_dir="$DATA_DIR/go"
    
    for repo in "${go_repos[@]}"; do
        log "Downloading $repo..."
        
        local repo_name=$(echo "$repo" | tr '/' '_')
        local clone_dir="$TEMP_DIR/go_${repo_name}"
        
        # Clone repository
        if git clone "https://github.com/${repo}.git" "$clone_dir" --depth 1 2>/dev/null; then
            # Copy Go files
            find "$clone_dir" -name "*.go" -not -path "*/vendor/*" -not -path "*_test.go" -not -path "*/testdata/*" | \
                head -50 | \
                while read -r file; do
                    local basename=$(basename "$file")
                    local rel_path=$(realpath --relative-to="$clone_dir" "$file" 2>/dev/null || echo "$basename")
                    local safe_name=$(echo "$rel_path" | tr '/' '_')
                    cp "$file" "$go_dir/${repo_name}_${safe_name}" 2>/dev/null || true
                done
            
            # Cleanup
            rm -rf "$clone_dir"
            
            success "Downloaded $repo"
        else
            warning "Failed to clone $repo"
        fi
        
        # Rate limiting to be nice to GitHub
        sleep 2
    done
}

# Download Java packages from Maven Central via GitHub
download_java() {
    log "Downloading popular Java packages..."
    
    local java_repos=(
        "spring-projects/spring-boot"
        "spring-projects/spring-framework"
        "apache/kafka"
        "elastic/elasticsearch"
        "apache/spark"
        "google/guava"
        "apache/commons-lang"
        "junit-team/junit5"
        "mockito/mockito"
        "apache/maven"
        "gradle/gradle"
        "ReactiveX/RxJava"
        "square/okhttp"
        "square/retrofit"
        "apache/lucene"
        "hibernate/hibernate-orm"
        "mybatis/mybatis-3"
        "redisson/redisson"
        "netty/netty"
        "eclipse-vertx/vert.x"
    )
    
    local java_dir="$DATA_DIR/java"
    
    for repo in "${java_repos[@]}"; do
        log "Downloading $repo..."
        
        local repo_name=$(echo "$repo" | tr '/' '_')
        local clone_dir="$TEMP_DIR/java_${repo_name}"
        
        # Clone repository
        if git clone "https://github.com/${repo}.git" "$clone_dir" --depth 1 2>/dev/null; then
            # Copy Java files
            find "$clone_dir" -name "*.java" -not -path "*/test/*" -not -path "*/target/*" | \
                head -40 | \
                while read -r file; do
                    local basename=$(basename "$file")
                    local rel_path=$(realpath --relative-to="$clone_dir" "$file" 2>/dev/null || echo "$basename")
                    local safe_name=$(echo "$rel_path" | tr '/' '_')
                    cp "$file" "$java_dir/${repo_name}_${safe_name}" 2>/dev/null || true
                done
            
            # Cleanup
            rm -rf "$clone_dir"
            
            success "Downloaded $repo"
        else
            warning "Failed to clone $repo"
        fi
        
        # Rate limiting
        sleep 2
    done
}

# Download TypeScript packages
download_typescript() {
    log "Downloading popular TypeScript packages..."
    
    local ts_repos=(
        "microsoft/TypeScript"
        "angular/angular"
        "microsoft/vscode"
        "nestjs/nest"
        "typeorm/typeorm"
        "prisma/prisma"
        "apollographql/apollo-server"
        "storybookjs/storybook"
        "microsoft/playwright"
        "reduxjs/redux-toolkit"
        "trpc/trpc"
        "chakra-ui/chakra-ui"
        "ant-design/ant-design"
        "microsoft/fluentui"
        "ionic-team/ionic-framework"
        "nrwl/nx"
        "vitejs/vite"
        "webpack/webpack"
        "rollup/rollup"
        "esbuild-js/esbuild"
    )
    
    local ts_dir="$DATA_DIR/typescript"
    
    for repo in "${ts_repos[@]}"; do
        log "Downloading $repo..."
        
        local repo_name=$(echo "$repo" | tr '/' '_')
        local clone_dir="$TEMP_DIR/ts_${repo_name}"
        
        # Clone repository
        if git clone "https://github.com/${repo}.git" "$clone_dir" --depth 1 2>/dev/null; then
            # Copy TypeScript files
            find "$clone_dir" -name "*.ts" -not -path "*/node_modules/*" -not -path "*/test/*" -not -path "*/tests/*" | \
                grep -v "\.d\.ts$" | \
                head -40 | \
                while read -r file; do
                    local basename=$(basename "$file")
                    local rel_path=$(realpath --relative-to="$clone_dir" "$file" 2>/dev/null || echo "$basename")
                    local safe_name=$(echo "$rel_path" | tr '/' '_')
                    cp "$file" "$ts_dir/${repo_name}_${safe_name}" 2>/dev/null || true
                done
            
            # Cleanup
            rm -rf "$clone_dir"
            
            success "Downloaded $repo"
        else
            warning "Failed to clone $repo"
        fi
        
        # Rate limiting
        sleep 2
    done
}

# Download additional language samples
download_additional_languages() {
    log "Downloading additional language samples..."
    
    # C/C++ repositories
    local c_repos=(
        "git/git"
        "torvalds/linux"
        "redis/redis"
        "nginx/nginx"
        "curl/curl"
    )
    
    local c_dir="$DATA_DIR/c"
    
    for repo in "${c_repos[@]}"; do
        log "Downloading C/C++ from $repo..."
        
        local repo_name=$(echo "$repo" | tr '/' '_')
        local clone_dir="$TEMP_DIR/c_${repo_name}"
        
        if git clone "https://github.com/${repo}.git" "$clone_dir" --depth 1 2>/dev/null; then
            # Copy C/C++ files
            find "$clone_dir" -name "*.c" -o -name "*.cpp" -o -name "*.cc" -o -name "*.cxx" | \
                head -20 | \
                while read -r file; do
                    local basename=$(basename "$file")
                    cp "$file" "$c_dir/${repo_name}_${basename}" 2>/dev/null || true
                done
            
            rm -rf "$clone_dir"
            success "Downloaded C/C++ from $repo"
        else
            warning "Failed to clone $repo"
        fi
        
        sleep 2
    done
    
    # Rust repositories
    local rust_repos=(
        "rust-lang/rust"
        "tokio-rs/tokio"
        "serde-rs/serde"
        "actix/actix-web"
        "tauri-apps/tauri"
    )
    
    local rust_dir="$DATA_DIR/rust"
    
    for repo in "${rust_repos[@]}"; do
        log "Downloading Rust from $repo..."
        
        local repo_name=$(echo "$repo" | tr '/' '_')
        local clone_dir="$TEMP_DIR/rust_${repo_name}"
        
        if git clone "https://github.com/${repo}.git" "$clone_dir" --depth 1 2>/dev/null; then
            # Copy Rust files
            find "$clone_dir" -name "*.rs" -not -path "*/target/*" | \
                head -30 | \
                while read -r file; do
                    local basename=$(basename "$file")
                    cp "$file" "$rust_dir/${repo_name}_${basename}" 2>/dev/null || true
                done
            
            rm -rf "$clone_dir"
            success "Downloaded Rust from $repo"
        else
            warning "Failed to clone $repo"
        fi
        
        sleep 2
    done
    
    # Ruby repositories
    local ruby_repos=(
        "rails/rails"
        "jekyll/jekyll"
        "rapid7/metasploit-framework"
        "chef/chef"
        "hashicorp/vagrant"
    )
    
    local ruby_dir="$DATA_DIR/ruby"
    
    for repo in "${ruby_repos[@]}"; do
        log "Downloading Ruby from $repo..."
        
        local repo_name=$(echo "$repo" | tr '/' '_')
        local clone_dir="$TEMP_DIR/ruby_${repo_name}"
        
        if git clone "https://github.com/${repo}.git" "$clone_dir" --depth 1 2>/dev/null; then
            # Copy Ruby files
            find "$clone_dir" -name "*.rb" -not -path "*/test/*" -not -path "*/spec/*" | \
                head -30 | \
                while read -r file; do
                    local basename=$(basename "$file")
                    cp "$file" "$ruby_dir/${repo_name}_${basename}" 2>/dev/null || true
                done
            
            rm -rf "$clone_dir"
            success "Downloaded Ruby from $repo"
        else
            warning "Failed to clone $repo"
        fi
        
        sleep 2
    done
}

# Generate summary report
generate_summary() {
    log "Generating download summary..."
    
    local summary_file="$DATA_DIR/download_summary.txt"
    
    {
        echo "Dataset Download Summary"
        echo "======================="
        echo "Download Date: $(date)"
        echo "Download Location: $DATA_DIR"
        echo ""
        
        for lang_dir in "$DATA_DIR"/{javascript,python,go,java,typescript,c,rust,ruby}; do
            if [ -d "$lang_dir" ]; then
                local lang=$(basename "$lang_dir")
                local file_count=$(find "$lang_dir" -type f | wc -l)
                local total_size=$(du -sh "$lang_dir" 2>/dev/null | cut -f1)
                
                echo "$lang: $file_count files, $total_size"
            fi
        done
        
        echo ""
        echo "Total disk usage:"
        du -sh "$DATA_DIR" 2>/dev/null | cut -f1
        
    } > "$summary_file"
    
    cat "$summary_file"
    success "Summary written to $summary_file"
}

# Cleanup function
cleanup() {
    log "Cleaning up temporary files..."
    rm -rf "$TEMP_DIR"
    success "Cleanup completed"
}

# Main function
main() {
    echo -e "${BLUE}Universal Code-to-ABAP Translation System${NC}"
    echo -e "${BLUE}Dataset Downloader${NC}"
    echo ""
    
    # Handle command line arguments
    local languages=()
    local skip_cleanup=false
    
    while [[ $# -gt 0 ]]; do
        case $1 in
            --javascript|--js)
                languages+=("javascript")
                shift
                ;;
            --python|--py)
                languages+=("python")
                shift
                ;;
            --go|--golang)
                languages+=("go")
                shift
                ;;
            --java)
                languages+=("java")
                shift
                ;;
            --typescript|--ts)
                languages+=("typescript")
                shift
                ;;
            --all-languages)
                languages+=("additional")
                shift
                ;;
            --no-cleanup)
                skip_cleanup=true
                shift
                ;;
            --help|-h)
                echo "Usage: $0 [OPTIONS]"
                echo ""
                echo "Options:"
                echo "  --javascript, --js     Download JavaScript packages"
                echo "  --python, --py         Download Python packages"
                echo "  --go, --golang         Download Go packages"
                echo "  --java                 Download Java packages"
                echo "  --typescript, --ts     Download TypeScript packages"
                echo "  --all-languages        Download additional languages (C/C++, Rust, Ruby)"
                echo "  --no-cleanup           Don't remove temporary files"
                echo "  --help, -h             Show this help"
                echo ""
                echo "If no language is specified, all languages will be downloaded."
                exit 0
                ;;
            *)
                error "Unknown option: $1"
                exit 1
                ;;
        esac
    done
    
    # If no specific languages requested, download all
    if [ ${#languages[@]} -eq 0 ]; then
        languages=("javascript" "python" "go" "java" "typescript" "additional")
    fi
    
    # Start download process
    log "Starting dataset download process..."
    
    setup_directories
    check_dependencies
    
    # Download based on requested languages
    for lang in "${languages[@]}"; do
        case $lang in
            javascript)
                download_javascript
                ;;
            python)
                download_python
                ;;
            go)
                download_go
                ;;
            java)
                download_java
                ;;
            typescript)
                download_typescript
                ;;
            additional)
                download_additional_languages
                ;;
        esac
    done
    
    generate_summary
    
    if [ "$skip_cleanup" != "true" ]; then
        cleanup
    fi
    
    success "Dataset download completed successfully!"
    log "Check $DATA_DIR for downloaded source code files"
    log "Use process-datasets.js to convert to training format"
}

# Handle script interruption
trap cleanup EXIT INT TERM

# Run main function
main "$@"