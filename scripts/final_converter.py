#!/usr/bin/env python3
"""
FINAL CONVERTER: Jupyter to Org with inline-displaying images
Combines conversion + description removal in one step
"""

import nbformat
import json
import os
import base64
import re
from pathlib import Path

def convert_ipynb_to_org_final(ipynb_file, org_file=None):
    """Final converter: Creates Org files where ALL images display inline."""
    
    if org_file is None:
        org_file = ipynb_file.replace('.ipynb', '_FINAL.org')
    
    base_name = Path(ipynb_file).stem
    image_dir = f"{base_name}_images"
    os.makedirs(image_dir, exist_ok=True)
    
    # Clean old images
    for f in os.listdir(image_dir):
        if f.startswith(base_name):
            os.remove(os.path.join(image_dir, f))
    
    # Read notebook
    with open(ipynb_file, 'r', encoding='utf-8') as f:
        notebook = nbformat.read(f, as_version=4)
    
    org_lines = [f"#+TITLE: {base_name}\n"]
    img_counter = 0
    
    for cell_idx, cell in enumerate(notebook['cells']):
        cell_type = cell['cell_type']
        
        if cell_idx > 0:
            org_lines.append("")
        
        if cell_type == 'markdown':
            source = ''.join(cell['source'])
            
            # Process ALL images in markdown
            source = process_markdown_cell(source, base_name, image_dir, img_counter)
            img_counter = source[1]  # Get updated counter
            source = source[0]       # Get processed text
            
            # Convert markdown to org
            source = markdown_to_org_final(source)
            org_lines.append(source)
            
        elif cell_type == 'code':
            # Code block
            source_code = ''.join(cell['source'])
            org_lines.append(f"#+begin_src python\n{source_code}\n#+end_src")
            
            # Process outputs
            if cell.get('outputs'):
                org_lines.append("#+RESULTS:")
                img_counter = process_code_outputs(cell['outputs'], org_lines, 
                                                   base_name, image_dir, img_counter)
    
    # Join and write
    content = '\n'.join(org_lines)
    
    # CRITICAL: Remove descriptions from ALL image links
    # Convert [[file:path][desc]] to [[file:path]]
    content = re.sub(r'(\[\[file:[^]]+\])\[[^]]+\](\])', r'\1\2', content)
    
    with open(org_file, 'w', encoding='utf-8') as f:
        f.write(content)
    
    print(f"✓ Created: {org_file}")
    print(f"✓ Images in: {image_dir}/")
    print("✓ ALL images will display inline (no descriptions in links)")
    
    return org_file

def process_markdown_cell(source, base_name, image_dir, img_counter):
    """Process markdown cell, extract images WITHOUT descriptions."""
    
    # Pattern for data URL images
    pattern = r'!\[([^\]]*)\]\(data:image/(png|jpeg|jpg|gif);base64,([a-zA-Z0-9+/=]+)\)'
    
    def save_image(match):
        nonlocal img_counter
        # alt_text = match.group(1)  # We DON'T use description
        ext = match.group(2)
        b64_data = match.group(3)
        
        img_counter += 1
        img_filename = f"{base_name}_{img_counter:02d}.{ext}"
        img_path = os.path.join(image_dir, img_filename)
        
        try:
            image_bytes = base64.b64decode(b64_data)
            with open(img_path, 'wb') as f:
                f.write(image_bytes)
            # Return PLAIN link without description
            return f'\n[[file:{img_path}]]\n'
        except:
            return '\n<!-- Image extraction failed -->\n'
    
    new_source = re.sub(pattern, save_image, source)
    return new_source, img_counter

def process_code_outputs(outputs, org_lines, base_name, image_dir, img_counter):
    """Process code outputs, create PLAIN image links."""
    for output in outputs:
        if output['output_type'] in ['display_data', 'execute_result']:
            data = output.get('data', {})
            for mime_type in data:
                if 'image' in mime_type:
                    img_counter += 1
                    ext = mime_type.split('/')[1]
                    if ext == 'svg+xml':
                        ext = 'svg'
                    
                    img_filename = f"{base_name}_{img_counter:02d}.{ext}"
                    img_path = os.path.join(image_dir, img_filename)
                    
                    img_data = data[mime_type]
                    try:
                        if isinstance(img_data, str):
                            if img_data.startswith('data:'):
                                img_data = img_data.split(',')[1]
                            image_bytes = base64.b64decode(img_data)
                        elif isinstance(img_data, list):
                            image_bytes = base64.b64decode(''.join(img_data))
                        
                        with open(img_path, 'wb') as f:
                            f.write(image_bytes)
                        
                        # PLAIN link without description
                        org_lines.append(f"[[file:{img_path}]]")
                    except:
                        org_lines.append("<!-- Output image failed -->")
                    break
    return img_counter

def markdown_to_org_final(text):
    """Convert markdown to org syntax."""
    # Headers
    text = re.sub(r'^# (.*)$', r'* \1', text, flags=re.MULTILINE)
    text = re.sub(r'^## (.*)$', r'** \1', text, flags=re.MULTILINE)
    text = re.sub(r'^### (.*)$', r'*** \1', text, flags=re.MULTILINE)
    
    # Bold/italic
    text = text.replace('**', '*').replace('__', '*')
    
    # Code blocks
    text = re.sub(r'```python\s*(.*?)```', r'#+begin_src python\n\1\n#+end_src', 
                  text, flags=re.DOTALL)
    
    # Lists
    text = re.sub(r'^\s*[-*+]\s+', r'  - ', text, flags=re.MULTILINE)
    
    return text

if __name__ == "__main__":
    import sys
    if len(sys.argv) < 2:
        print("FINAL Jupyter to Org Converter")
        print("=" * 50)
        print("Creates Org files where ALL images display inline")
        print("Usage: python final_converter.py <notebook.ipynb>")
        print("\nExample: python final_converter.py Lab3-Student.ipynb")
        sys.exit(1)
    
    convert_ipynb_to_org_final(sys.argv[1])
