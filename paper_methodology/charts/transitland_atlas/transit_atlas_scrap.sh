#!/usr/bin/env bash

# Exit on error (except for commands we handle)
set -e

# Verify we are in a Git repository
if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    echo "Error: This script must be run inside a Git repository."
    exit 1
fi

# Parse options
CHECKOUT_MODE=false
CSV_FILE=""
while [[ "$#" -gt 0 ]]; do
    case $1 in
        --checkout) CHECKOUT_MODE=true ;;
        --csv) 
            if [ -z "$2" ]; then
                echo "Error: --csv option requires a file path argument."
                exit 1
            fi
            CSV_FILE="$2"
            shift 
            ;;
        *) echo "Unknown option: $1"; exit 1 ;;
    esac
    shift
done

# Store original branch/commit to restore at the end
original_ref=$(git symbolic-ref --short -q HEAD || git rev-parse HEAD)

if [ "$CHECKOUT_MODE" = "true" ]; then
    # Ensure working directory has no uncommitted changes in tracked files (ignoring this script)
    unstaged_changes=$(git diff --name-only | grep -v "scripts/count-feeds-by-year.sh" || true)
    staged_changes=$(git diff --cached --name-only | grep -v "scripts/count-feeds-by-year.sh" || true)
    
    if [ -n "$unstaged_changes" ] || [ -n "$staged_changes" ]; then
        echo "Error: Working directory has unstaged or staged changes to other tracked files."
        echo "Please commit, stash, or discard them before running this script."
        exit 1
    fi

    # Function to clean up and restore original branch/commit
    cleanup() {
        echo ""
        echo "Restoring original repository state (checking out $original_ref)..."
        git checkout -q "$original_ref"
    }
    trap cleanup EXIT
    
    echo "Running in CHECKOUT mode (physically checking out commits)..."
else
    echo "Running in FAST mode (using git archive and git ls-tree)..."
fi

if [ -n "$CSV_FILE" ]; then
    echo "Writing results to CSV file: $CSV_FILE"
    echo "year,commit,json_files,total_feeds" > "$CSV_FILE"
fi

echo "Checking counts from 2026 down to 2008..."
echo ""
printf "%-6s | %-10s | %-12s | %-12s\n" "Year" "Commit" "JSON Files" "Total Feeds"
echo "---------------------------------------------------------"

for year in {2026..2008}; do
    # Get the commit closest to (and before) Jan 1st of the year
    commit=$(git rev-list -n 1 --before="${year}-01-01 00:00:00" "$original_ref" 2>/dev/null || true)
    
    if [ -z "$commit" ]; then
        printf "%-6s | %-10s | %-12s | %-12s\n" "$year" "N/A" "No commits" "N/A"
        if [ -n "$CSV_FILE" ]; then
            echo "$year,,0,0" >> "$CSV_FILE"
        fi
        continue
    fi
    
    short_commit=$(git rev-parse --short "$commit")

    if [ "$CHECKOUT_MODE" = "true" ]; then
        # Checkout the commit quietly
        # We redirect stderr to ignore warnings about the script itself being removed/overwritten
        git checkout -q "$commit" 2>/dev/null || true
        
        # Count JSON files and sum feeds elements
        if [ -d "feeds" ]; then
            files_count=$(find feeds -type f -name "*.json" | wc -l)
            feeds_count=$(find feeds -type f -name "*.json" -exec jq '.feeds | length' {} + 2>/dev/null | awk '{sum+=$1} END {print sum+0}')
        else
            files_count=0
            feeds_count=0
        fi
    else
        # Count JSON files via git ls-tree
        files_count=$(git ls-tree -r --name-only "$commit" feeds/ 2>/dev/null | grep "\.json$" | wc -l || echo 0)
        # Sum elements in "feeds" array using git archive
        feeds_count=$(git archive --format=tar "$commit" feeds/ 2>/dev/null | tar -xO 2>/dev/null | jq '.feeds | length' 2>/dev/null | awk '{sum+=$1} END {print sum+0}')
    fi
    
    printf "%-6s | %-10s | %-12s | %-12s\n" "$year" "$short_commit" "$files_count" "$feeds_count"
    
    if [ -n "$CSV_FILE" ]; then
        echo "$year,$short_commit,$files_count,$feeds_count" >> "$CSV_FILE"
    fi
done
