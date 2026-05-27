#!/usr/bin/env bash

# Exit on error (except for commands we handle)
set -e

# Verify we are in a Git repository
if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    echo "Error: This script must be run inside a Git repository."
    exit 1
fi

# Parse options
FAST_MODE=false
while [[ "$#" -gt 0 ]]; do
    case $1 in
        --fast) FAST_MODE=true ;;
        *) echo "Unknown option: $1"; exit 1 ;;
    esac
    shift
done

# Store original branch/commit to restore at the end
original_ref=$(git symbolic-ref --short -q HEAD || git rev-parse HEAD)

if [ "$FAST_MODE" = "true" ]; then
    echo "Running in FAST mode (using git ls-tree without checking out)..."
else
    # Ensure working directory has no uncommitted changes in tracked files
    if ! git diff-files --quiet || ! git diff-index --cached --quiet HEAD; then
        echo "Error: Working directory has unstaged or staged changes to tracked files."
        echo "Please commit, stash, or discard them before running this script, or use --fast mode."
        exit 1
    fi

    # Function to clean up and restore original branch/commit
    cleanup() {
        echo ""
        echo "Restoring original repository state (checking out $original_ref)..."
        git checkout -q "$original_ref"
    }
    trap cleanup EXIT
fi

echo "Checking counts from 2026 down to 2008..."
echo ""
printf "%-6s | %-10s | %-12s\n" "Year" "Commit" "JSON Feeds"
echo "-------------------------------------"

for year in {2026..2008}; do
    # Get the commit closest to (and before) Jan 1st of the year
    commit=$(git rev-list -n 1 --before="${year}-01-01 00:00:00" "$original_ref" 2>/dev/null || true)
    
    if [ -z "$commit" ]; then
        printf "%-6s | %-10s | %-12s\n" "$year" "N/A" "No commits"
        continue
    fi
    
    short_commit=$(git rev-parse --short "$commit")

    if [ "$FAST_MODE" = "true" ]; then
        # Count JSON files via git ls-tree without checkout
        count=$(git ls-tree -r --name-only "$commit" feeds/ 2>/dev/null | grep "\.json$" | wc -l || echo 0)
    else
        # Checkout the commit quietly
        git checkout -q "$commit"
        
        # Count JSON files in feeds/ directory
        if [ -d "feeds" ]; then
            count=$(find feeds -type f -name "*.json" | wc -l)
        else
            count=0
        fi
    fi
    
    printf "%-6s | %-10s | %-12s\n" "$year" "$short_commit" "$count"
done
