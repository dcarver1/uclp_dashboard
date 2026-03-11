import os
import re

files = ["ui.R", "server.R", "global.R"]

for f in files:
    if not os.path.exists(f): continue
    with open(f, "r") as file:
        content = file.read()
    
    # Remove comments
    content = re.sub(r'#.*', '', content)
    # Remove string literals
    content = re.sub(r'".*?"', '""', content)
    content = re.sub(r"'.*?'", "''", content)
    
    calls = re.findall(r'\b([a-zA-Z._][a-zA-Z0-9._]*)\s*\(', content)
    pkg_calls = re.findall(r'\b([a-zA-Z._][a-zA-Z0-9._]*)::([a-zA-Z._][a-zA-Z0-9._]*)\s*\(', content)
    
    unique_calls = set(calls)
    pkg_dict = {}
    for pkg, func in pkg_calls:
        pkg_dict[func] = pkg
        
    print(f"File: {f}")
    
    funcs_pkg = [f"{pkg}::{func}" for func, pkg in pkg_dict.items()]
    print("  Implicit functions:", sorted([c for c in unique_calls if c not in ['c', 'if', 'else', 'for', 'while', 'function', 'return', 'paste', 'paste0', 'cat', 'print', 'tryCatch', 'list', 'seq_along', 'names', 'gsub', 'grep', 'grepl', 'round', 'min', 'max', 'mean', 'sum', 'unique', 'any', 'all', 'ifelse', 'as.character', 'as.numeric', 'Sys.time', 'Sys.Date']]))
    print()
