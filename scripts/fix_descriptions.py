#!/usr/bin/env python3
"""
Fix image descriptions in Org files for inline display.
Converts [[file:path][description]] to [[file:path]]
"""

import re
import sys

def fix_org_image_descriptions(org_file):
    """Remove descriptions from image links in Org files."""
    
    with open(org_file, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # Find all image links with descriptions
    pattern = r'(\[\[file:[^]]+\])\[[^]]+\](\])'
    
    def remove_description(match):
        return match.group(1) + match.group(2)
    
    # Replace all
    fixed_content = re.sub(pattern, remove_description, content)
    
    # Count changes
    original_count = len(re.findall(pattern, content))
    fixed_count = len(re.findall(pattern, fixed_content))
    
    if original_count == fixed_count:
        print(f"✓ No changes needed for {org_file}")
        return False
    
    # Write back
    with open(org_file, 'w', encoding='utf-8') as f:
        f.write(fixed_content)
    
    changes = original_count - fixed_count
    print(f"✓ Fixed {changes} image links in {org_file}")
    return True

def fix_multiple_files(file_pattern):
    """Fix multiple Org files."""
    import glob
    
    files = glob.glob(file_pattern)
    if not files:
        print(f"No files found matching: {file_pattern}")
        return
    
    fixed_count = 0
    for org_file in files:
        if fix_org_image_descriptions(org_file):
            fixed_count += 1
    
    print(f"\n✓ Total: Fixed {fixed_count} out of {len(files)} files")

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Fix image descriptions in Org files")
        print("=" * 50)
        print("Converts: [[file:path][description]] → [[file:path]]")
        print("\nUsage:")
        print("  python fix_descriptions.py <file.org>")
        print("  python fix_descriptions.py '*.org' (for multiple files)")
        print("\nExample:")
        print("  python fix_descriptions.py Lab1-Student_FINAL.org")
        print("  python fix_descriptions.py '*.org'")
        sys.exit(1)
    
    arg = sys.argv[1]
    if '*' in arg:
        fix_multiple_files(arg)
    else:
        fix_org_image_descriptions(arg)
