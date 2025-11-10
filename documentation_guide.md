# ADA_Standards Library - Complete Documentation Guide

This guide provides a comprehensive overview of the ADA_Standards library's architecture, usage patterns, and best practices.

## 📚 Table of Contents

1. [Architecture Overview](#architecture-overview)
2. [Core Concepts](#core-concepts)
3. [Complete Workflow](#complete-workflow)
4. [API Reference Quick Guide](#api-reference-quick-guide)
5. [Common Patterns](#common-patterns)
6. [Performance Considerations](#performance-considerations)
7. [Troubleshooting](#troubleshooting)

## Architecture Overview

### Design Philosophy

The library uses a **three-phase approach**:

1. **Extraction Phase**: Regex-based pattern matching identifies Ada constructs
2. **Building Phase**: Stack-based algorithm establishes parent-child relationships
3. **Post-Processing Phase**: Enriches the tree with derived data

### Key Design Decisions

- **Regex over Full Parsing**: Lightweight and fast for analysis tasks
- **Position Preservation**: All cleaning operations maintain character positions
- **Flat-then-Hierarchical**: Extract flat list first, build tree second
- **indextree Arena**: Efficient tree storage with O(1) node access

## Core Concepts

### 1. NodeData - The Universal Node

`NodeData` is designed to represent **any** Ada construct through flexible fields:

```rust
pub struct NodeData {
    // Common fields (always used)
    name: String,           // Identifier
    node_type: String,      // Classification
    start_line: Option<usize>,
    end_line: Option<usize>,
    start_index: Option<usize>,
    end_index: Option<usize>,
    
    // Specialized fields (used by specific node types)
    is_body: Option<bool>,         // For packages, procedures
    arguments: Option<Vec<ArgumentData>>, // For subprograms
    conditions: Option<ConditionExpr>,    // For control flow
    cases: Option<Vec<String>>,           // For case statements
    // ... and many more
}
```

**Key Insight**: Not all fields are relevant for all node types. A `VariableDeclaration` won't use `arguments`, and an `IfStatement` won't use `tuple_values`.

### 2. Expression Trees

Conditions are parsed into hierarchical trees that respect precedence:

```
"X > 10 and Y < 20"
         │
         ▼
      [AND]
       / \
      /   \
   [>]     [<]
   / \     / \
  X  10   Y  20
```

Each node in the tree is an `Expression` enum variant:
- `Expression::Binary` - Binary operations
- `Expression::Unary` - Unary operations
- `Expression::Membership` - Membership tests
- `Expression::Literal` - Atomic values

### 3. The Arena Pattern

The AST uses `indextree::Arena` for efficient tree storage:

```rust
pub struct AST {
    arena: Arena<NodeData>,  // Owns all nodes
    root_id: NodeId,         // Entry point
    // ...
}
```

**Benefits**:
- O(1) node access by ID
- Automatic memory management
- Safe parent-child traversal
- No recursive allocation overhead

## Complete Workflow

### Step-by-Step Process

```rust
use ADA_Standards::{AST, ASTError};
use std::fs;

fn analyze_ada_file(path: &str) -> Result<(), ASTError> {
    // STEP 1: Read source file
    let raw_code = fs::read_to_string(path)
        .expect("Failed to read file");
    
    // STEP 2: Clean the code
    // - Removes comments (replaced with spaces)
    // - Cleans string contents (replaced with spaces)
    // - Normalizes tabs to spaces
    let cleaned_code = AST::clean_code(&raw_code);
    
    // STEP 3: Extract all nodes
    // Runs all extractor functions to find constructs
    let nodes = AST::extract_all_nodes(&cleaned_code)?;
    
    // STEP 4: Create AST instance
    // Nodes are stored but tree isn't built yet
    let mut ast = AST::new(nodes);
    
    // STEP 5: Build the tree
    // - Sorts nodes by position
    // - Populates arena
    // - Associates end statements
    // - Establishes parent-child relationships
    ast.build(&cleaned_code)?;
    
    // STEP 6: Post-processing (optional but recommended)
    // - Extracts case alternatives
    // - Parses exit-when conditions
    ast.populate_cases(&cleaned_code)?;
    ast.populate_simple_loop_conditions(&cleaned_code)?;
    
    // STEP 7: Analyze!
    // Now you can traverse and query the tree
    ast.print_tree();
    
    Ok(())
}
```

### Why This Order?

1. **Clean First**: Prevents regex from matching inside strings/comments
2. **Extract Before Build**: Allows sorting and validation
3. **Build Before Post-Process**: Tree structure needed for context
4. **Post-Process Last**: Requires accurate node positions

## API Reference Quick Guide

### AST Construction

| Method | Purpose | When to Use |
|--------|---------|-------------|
| `AST::clean_code(text)` | Preprocess source | Always, before extraction |
| `AST::extract_all_nodes(text)` | Find all constructs | Standard workflow |
| `AST::extract_packages(text)` | Find only packages | Custom extraction |
| `AST::new(nodes)` | Create AST | After extraction |
| `ast.build(text)` | Build tree | After new(), before analysis |

### Post-Processing

| Method | Purpose | Required? |
|--------|---------|-----------|
| `populate_cases(text)` | Extract when clauses | For case analysis |
| `populate_simple_loop_conditions(text)` | Extract exit-when | For loop analysis |

### Querying

| Method | Purpose | Returns |
|--------|---------|---------|
| `find_node_by_name_and_type(name, type)` | Search by criteria | `Option<NodeId>` |
| `root_id()` | Get root node | `NodeId` |
| `arena()` | Access node storage | `&Arena<NodeData>` |

### Traversal Patterns

```rust
// Depth-first traversal from root
for node_id in ast.root_id().descendants(ast.arena()) {
    let node = ast.arena().get(node_id).unwrap().get();
    println!("{}", node.name);
}

// Get direct children only
for child_id in some_node_id.children(ast.arena()) {
    // Process child
}

// Get parent
if let Some(parent_id) = node_id.ancestors(ast.arena()).nth(1) {
    // Process parent (nth(0) is self, nth(1) is parent)
}
```

## Common Patterns

### Pattern 1: Find All Procedures

```rust
fn find_all_procedures(ast: &AST) -> Vec<NodeId> {
    ast.root_id()
        .descendants(ast.arena())
        .filter(|&id| {
            let node = ast.arena().get(id).unwrap().get();
            node.node_type == "ProcedureNode"
        })
        .collect()
}
```

### Pattern 2: Check Coding Standard

```rust
fn check_procedure_naming(ast: &AST) {
    for node_id in ast.root_id().descendants(ast.arena()) {
        let node = ast.arena().get(node_id).unwrap().get();
        
        if node.node_type == "ProcedureNode" && node.is_body == Some(true) {
            // Check if name starts with uppercase
            if !node.name.chars().next().unwrap().is_uppercase() {
                eprintln!("Warning: Procedure '{}' should start with uppercase", node.name);
            }
        }
    }
}
```

### Pattern 3: Calculate Metrics

```rust
fn calculate_complexity(ast: &AST, node_id: NodeId) -> usize {
    let mut complexity = 1; // Base complexity
    
    for child_id in node_id.descendants(ast.arena()) {
        let child = ast.arena().get(child_id).unwrap().get();
        
        match child.node_type.as_str() {
            "IfStatement" | "ElsifStatement" => complexity += 1,
            "CaseStatement" => {
                if let Some(cases) = &child.cases {
                    complexity += cases.len();
                }
            }
            "WhileLoop" | "ForLoop" => complexity += 1,
            _ => {}
        }
    }
    
    complexity
}
```

### Pattern 4: Extract Documentation

```rust
fn extract_procedure_signatures(ast: &AST) {
    for node_id in ast.root_id().descendants(ast.arena()) {
        let node = ast.arena().get(node_id).unwrap().get();
        
        if node.node_type == "ProcedureNode" && node.is_body == Some(false) {
            print!("procedure {}", node.name);
            
            if let Some(args) = &node.arguments {
                print!(" (");
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 { print!("; "); }
                    print!("{} : {} {}", arg.name, arg.mode, arg.data_type);
                }
                print!(")");
            }
            println!(";");
        }
    }
}
```

## Performance Considerations

### Memory Usage

- **NodeData Size**: ~800 bytes per node (many Options)
- **Arena Overhead**: Minimal (just pointer indirection)
- **Typical File**: 1000 lines ≈ 100-200 nodes ≈ 100KB

### Time Complexity

| Operation | Complexity | Notes |
|-----------|------------|-------|
| clean_code | O(n) | Single regex pass |
| extract_all_nodes | O(n × m) | n=chars, m=patterns |
| build | O(n log n) | Sort + linear scan |
| find_node | O(n) | Linear search |
| descendants | O(n) | Tree traversal |

### Optimization Tips

1. **Reuse Cleaned Code**: Don't call `clean_code()` multiple times
2. **Extract Once**: Call `extract_all_nodes()` once, not individual extractors multiple times
3. **Build Once**: The tree is immutable after building (except post-processing)
4. **Cache Lookups**: If searching repeatedly, cache NodeIds

## Troubleshooting

### Common Issues

#### Issue: "NodeId not found in arena"

**Cause**: Using a NodeId from a previous AST instance
**Solution**: Rebuild the AST or store node data, not IDs

#### Issue: "Unmatched end statements"

**Cause**: Malformed code or regex not matching properly
**Solution**: Check that code is valid Ada; warnings are usually harmless

#### Issue: "Missing nodes in tree"

**Cause**: Nodes not being pushed onto stack during build
**Solution**: Check if node has `end_line` set (should be None for containers)

#### Issue: "Wrong parent-child relationships"

**Cause**: Incorrect line numbering or end line association
**Solution**: Verify `clean_code()` was called and line counts are accurate

### Debugging Techniques

```rust
// Print the entire tree
ast.print_tree();

// Print detailed node info
for node_id in ast.root_id().descendants(ast.arena()) {
    let node = ast.arena().get(node_id).unwrap().get();
    node.print_info();
}

// Print expression tree
if let Some(cond) = &node.conditions {
    if let Some(root) = &cond.albero {
        AST::leggitree(root, 0, "Root: ");
    }
}

// Check extraction counts
let nodes = AST::extract_all_nodes(&cleaned)?;
println!("Found {} nodes", nodes.len());
for node in &nodes {
    println!("  {} at line {}", node.node_type, node.start_line.unwrap_or(0));
}
```

## Best Practices

### Do's ✅

- **Always clean code first** before extraction
- **Check for errors** after each major step
- **Use the arena reference** for traversal (don't clone nodes unnecessarily)
- **Cache frequently-used NodeIds** if doing repeated searches
- **Test with real Ada code** to validate your analysis logic

### Don'ts ❌

- **Don't modify nodes after building** (arena is the source of truth)
- **Don't assume all fields are populated** (use pattern matching or if-let)
- **Don't search linearly repeatedly** (O(n²) for large files)
- **Don't forget post-processing** if you need cases or loop conditions
- **Don't rely on node_ids vector** after build (use arena instead)

## Extending the Library

### Adding a New Extractor

```rust
pub fn extract_my_construct(code_text: &str) -> Result<Vec<NodeData>, ASTError> {
    let pattern = Reg::new(r"my_pattern_here").map_err(|_| ASTError::RegexError)?;
    let mut nodes = Vec::new();
    
    for cap in pattern.captures_iter(code_text) {
        let captures = cap.map_err(|_| ASTError::RegexError)?;
        
        let keyword = captures.name("keyword").ok_or(ASTError::InvalidCapture)?;
        let start_line = code_text[..keyword.start()].matches('\n').count() + 1;
        let start_index = keyword.start();
        
        let node = NodeData::new(
            "MyNode".to_string(),
            "MyNodeType".to_string(),
            Some(start_line),
            Some(start_index),
            false,
        );
        
        nodes.push(node);
    }
    
    Ok(nodes)
}
```

### Adding a New Node Type

1. Add a new `node_type` string constant
2. Update `get_end_keyword()` if it has an end statement
3. Add extractor function
4. Call from `extract_all_nodes()`
5. Document the new fields used in `NodeData`

## Conclusion

The ADA_Standards library provides a flexible, efficient framework for Ada code analysis. By understanding its three-phase architecture and leveraging the indextree arena pattern, you can build powerful linting, metrics, and refactoring tools.

For more examples, see the test suite in `tests/test.rs`, which demonstrates end-to-end usage on a complex Ada file.
