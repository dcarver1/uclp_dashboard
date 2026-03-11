import os
import re

r_dir = "R"
files = [f for f in os.listdir(r_dir) if f.endswith(".R")]

for f in files:
    with open(os.path.join(r_dir, f), "r") as file:
        content = file.read()
    
    # Remove comments
    content = re.sub(r'#.*', '', content)
    # Remove string literals to avoid matching inside strings
    content = re.sub(r'".*?"', '""', content)
    content = re.sub(r"'.*?'", "''", content)
    
    # Find all function calls: word followed by (
    # Also find package::function
    calls = re.findall(r'\b([a-zA-Z._][a-zA-Z0-9._]*)\s*\(', content)
    pkg_calls = re.findall(r'\b([a-zA-Z._][a-zA-Z0-9._]*)::([a-zA-Z._][a-zA-Z0-9._]*)\s*\(', content)
    
    unique_calls = set(calls)
    pkg_dict = {}
    for pkg, func in pkg_calls:
        pkg_dict[func] = pkg
        
    print(f"File: {f}")
    
    funcs_no_pkg = [c for c in unique_calls if c not in pkg_dict.values() and c not in pkg_dict.keys()]
    funcs_pkg = [f"{pkg}::{func}" for func, pkg in pkg_dict.items()]
    
    print("  Explicit packages:", set(pkg_dict.values()))
    print("  Explicit functions:", funcs_pkg)
    print("  Implicit functions:", sorted([c for c in unique_calls if c not in ['c', 'if', 'else', 'for', 'while', 'function', 'return', 'paste', 'paste0', 'cat', 'print', 'lapply', 'sapply', 'is.na', 'is.null', 'length', 'stop', 'message', 'tryCatch', 'list', 'seq_along', 'names', 'gsub', 'grep', 'grepl', 'round', 'min', 'max', 'mean', 'sum', 'unique', 'any', 'all', 'ifelse', 'as.character', 'as.numeric', 'Sys.time', 'Sys.Date', 'difftime', 'format', 'system.time', 'flush.console', 'conditionMessage', 'inherits', 'attr', 'unlist', 'setNames', 'require', 'library', 'suppressMessages', 'options', 'setwd']]))
    print()
