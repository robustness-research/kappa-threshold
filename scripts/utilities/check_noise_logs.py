#!/usr/bin/env python3
"""
Check if all noise injection logs end with the expected completion message.
"""

import os
from pathlib import Path

# Define the expected ending message
EXPECTED_ENDING = """****************
RESULTS RECORDED
****************"""

def check_log_file(log_path):
    """
    Check if a log file ends with the expected message.
    
    Args:
        log_path: Path to the log file
        
    Returns:
        tuple: (is_complete, file_name)
    """
    try:
        with open(log_path, 'r', encoding='utf-8') as f:
            content = f.read()
            
        # Check if the file ends with the expected message (strip trailing whitespace)
        is_complete = content.rstrip().endswith(EXPECTED_ENDING.rstrip())
        
        return is_complete, log_path.name
    except Exception as e:
        return None, log_path.name, str(e)

def main():
    # Define the log directory
    log_dir = Path(__file__).parent / "logs" / "noise_injection"
    
    if not log_dir.exists():
        print(f"Error: Directory {log_dir} does not exist")
        return
    
    # Get all .log files
    log_files = sorted(log_dir.glob("*.log"))
    
    if not log_files:
        print(f"No log files found in {log_dir}")
        return
    
    print(f"Checking {len(log_files)} log files in {log_dir}\n")
    print("=" * 70)
    
    # Track results
    complete_files = []
    incomplete_files = []
    error_files = []
    
    # Check each log file
    for log_path in log_files:
        result = check_log_file(log_path)
        
        if len(result) == 3:  # Error case
            is_complete, file_name, error = result
            error_files.append((file_name, error))
            print(f"❌ ERROR reading {file_name}: {error}")
        else:
            is_complete, file_name = result
            if is_complete:
                complete_files.append(file_name)
                print(f"✓ {file_name}")
            else:
                incomplete_files.append(file_name)
                print(f"✗ {file_name} - INCOMPLETE")
    
    # Print summary
    print("\n" + "=" * 70)
    print("\nSUMMARY:")
    print(f"  Total files checked: {len(log_files)}")
    print(f"  Complete: {len(complete_files)}")
    print(f"  Incomplete: {len(incomplete_files)}")
    print(f"  Errors: {len(error_files)}")
    
    if incomplete_files:
        print("\n⚠️  INCOMPLETE FILES:")
        for file_name in incomplete_files:
            print(f"    - {file_name}")
    
    if error_files:
        print("\n❌ FILES WITH ERRORS:")
        for file_name, error in error_files:
            print(f"    - {file_name}: {error}")
    
    if not incomplete_files and not error_files:
        print("\n✓ All log files are complete!")
    
    return len(incomplete_files) + len(error_files)

if __name__ == "__main__":
    exit_code = main()
    exit(exit_code if exit_code is not None else 0)
