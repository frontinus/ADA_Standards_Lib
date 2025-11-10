# ADA_Standards

[![Rust](https://github.com/frontinus/ADA_Standards_Lib/actions/workflows/rust.yml/badge.svg)](https://github.com/frontinus/ADA_Standards_Lib/actions/workflows/rust.yml)
[![Latest version](https://img.shields.io/crates/v/ADA_Standards.svg)](https://crates.io/crates/ADA_Standards)
[![Documentation](https://docs.rs/ADA_Standards/badge.svg)](https://docs.rs/ADA_Standards)
[![License](https://img.shields.io/crates/l/ADA_Standards.svg)](https://github.com/frontinus/ada-analyzer#license)

A powerful, lightweight regex-based Ada parser written in Rust. Extract packages, procedures, types, control flow, and more from Ada source code into a traversable Abstract Syntax Tree (AST) for analysis, linting, and coding standards enforcement.

## ✨ Features

### Code Preprocessing
- **Smart Code Cleaning**: Strips comments and string literals while preserving code structure and line numbers
- **Tab Normalization**: Converts tabs to spaces for consistent parsing

### Comprehensive Ada Construct Extraction
- **Packages**: Specs and bodies, including nested packages
- **Subprograms**: Procedures and functions (specs, bodies, and generic instantiations)
- **Tasks & Entries**: Concurrent programming constructs with guard conditions
- **Types**: Records, arrays, derived types, subtypes, enumerations, and representation clauses
- **Control Flow**: `if`/`elsif`/`else`, `case`, `loop`, `while`, `for`, `exit when`, `declare` blocks
- **Variables**: Complete declaration parsing with types and default values

### Intelligent Parsing
- **Tree-Based AST**: Uses `indextree` for efficient parent-child relationships
- **Structured Parameters**: Parses procedure/function arguments into typed `ArgumentData` structs
- **Expression Trees**: Converts conditions into hierarchical `ConditionExpr` with support for:
  - Binary operators: `and`, `or`, `and then`, `or else`, `xor`
  - Comparison: `<`, `>`, `<=`, `>=`, `=`, `/=`
  - Membership: `in`, `not in`
  - Unary: `not`
  - Proper precedence and parenthesis handling
- **Automatic End Association**: Matches `end` statements to their corresponding blocks

### Analysis-Ready Output
- **Precise Location Tracking**: Line numbers, character indices, and column positions
- **Metadata Capture**: Distinguishes specs from bodies, captures type kinds, loop directions, etc.
- **Post-Processing**: Populate `case` alternatives and `exit when` conditions after initial parse

## 📦 Installation

Add to your `Cargo.toml`:

```toml
[dependencies]
ADA_Standards = "1.2.2"
```

Or use cargo:

```bash
cargo add ADA_Standards
```

## 🚀 Quick Start

```rust
use ADA_Standards::{AST, ASTError};
use std::fs;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // 1. Read Ada source file
    let code_text = fs::read_to_string("my_ada_file.adb")?;

    // 2. Clean the code (removes comments, preserves structure)
    let cleaned_code = AST::clean_code(&code_text);

    // 3. Extract all nodes
    let nodes = AST::extract_all_nodes(&cleaned_code)?;

    // 4. Build the AST
    let mut ast = AST::new(nodes);
    ast.build(&cleaned_code)?;

    // 5. Run post-processing
    ast.populate_cases(&cleaned_code)?;
    ast.populate_simple_loop_conditions(&cleaned_code)?;

    // 6. Analyze the tree
    ast.print_tree();

    // Find a specific procedure
    if let Some(proc_id) = ast.find_node_by_name_and_type("My_Procedure", "ProcedureNode") {
        println!("\nAnalyzing 'My_Procedure':");
        
        // Access node data
        let proc_node = ast.arena().get(proc_id).unwrap().get();
        println!("  Lines: {} to {}", 
            proc_node.start_line.unwrap(), 
            proc_node.end_line.unwrap()
        );
        
        // Check if it's a body or spec
        if proc_node.is_body == Some(true) {
            println!("  This is a procedure body");
        }
        
        // Inspect parameters
        if let Some(args) = &proc_node.arguments {
            println!("  Parameters:");
            for arg in args {
                println!("    - {}: {} (mode: {})", 
                    arg.name, arg.data_type, arg.mode
                );
            }
        }
        
        // Traverse children
        println!("\n  Children:");
        for child_id in proc_id.children(ast.arena()) {
            let child = ast.arena().get(child_id).unwrap().get();
            println!("    - {} ({})", child.name, child.node_type);
        }
    }

    Ok(())
}
```

## 📖 Usage Examples

### Extract Specific Constructs

```rust
// Extract only packages
let packages = AST::extract_packages(&cleaned_code)?;
for pkg in &packages {
    println!("Package: {}", pkg.name);
    if pkg.is_body == Some(true) {
        println!("  (body)");
    }
}

// Extract procedures and functions
let subprograms = AST::extract_procedures_functions(&cleaned_code)?;

// Extract control flow
let loops = AST::extract_simple_loops(&cleaned_code)?;
let while_loops = AST::extract_while_loops(&cleaned_code)?;
let for_loops = AST::extract_for_loops(&cleaned_code)?;
```

### Analyze Conditions

```rust
// Parse a condition expression
let condition = AST::parse_condition_expression("X > 10 and Y < 20");

// Access the expression tree
if let Some(root) = &condition.albero {
    AST::leggitree(root, 0, "Root: "); // Print tree structure
}

// Access the flat list
for expr in &condition.list {
    match expr {
        Expression::Binary(bin) => {
            println!("Binary op: {:?}", bin.op);
        }
        Expression::Literal(lit) => {
            println!("Literal: {}", lit);
        }
        _ => {}
    }
}
```

### Check Coding Standards

```rust
use indextree::NodeId;

// Example: Find procedures without documentation comments
fn check_undocumented_procedures(ast: &AST, code: &str) {
    for node_id in ast.root_id().descendants(ast.arena()) {
        let node = ast.arena().get(node_id).unwrap().get();
        
        if node.node_type == "ProcedureNode" && node.is_body == Some(true) {
            if let Some(start_line) = node.start_line {
                // Check if previous line is a comment
                let lines: Vec<&str> = code.lines().collect();
                if start_line > 1 {
                    let prev_line = lines[start_line - 2].trim();
                    if !prev_line.starts_with("--") {
                        println!("Warning: Procedure '{}' at line {} lacks documentation", 
                            node.name, start_line);
                    }
                }
            }
        }
    }
}
```

### Traverse the AST

```rust
// Visit all nodes in tree order
for node_id in ast.root_id().descendants(ast.arena()) {
    let node = ast.arena().get(node_id).unwrap().get();
    let depth = node_id.ancestors(ast.arena()).count() - 1;
    
    println!("{}{} - {}", 
        "  ".repeat(depth),
        node.node_type,
        node.name
    );
}

// Find all children of a node
if let Some(pkg_id) = ast.find_node_by_name_and_type("MyPackage", "PackageNode") {
    for child_id in pkg_id.children(ast.arena()) {
        let child = ast.arena().get(child_id).unwrap().get();
        println!("Child: {} ({})", child.name, child.node_type);
    }
}

// Get parent of a node
if let Some(parent_id) = some_node_id.ancestors(ast.arena()).nth(1) {
    let parent = ast.arena().get(parent_id).unwrap().get();
    println!("Parent: {}", parent.name);
}
```

## 🔍 Key Data Structures

### `NodeData`
Represents a single Ada construct with fields like:
- `name`: Identifier (e.g., "MyProcedure")
- `node_type`: Type of construct (e.g., "ProcedureNode", "IfStatement")
- `start_line`, `end_line`: Location in source
- `is_body`: Whether it's a body or spec
- `arguments`: Parsed parameters
- `conditions`: Parsed expressions
- `cases`: Case alternatives (for `case` statements)

### `AST`
The main tree structure with methods:
- `new()`: Create from node list
- `build()`: Construct parent-child relationships
- `populate_cases()`: Extract `when` clauses
- `populate_simple_loop_conditions()`: Extract `exit when` conditions
- `find_node_by_name_and_type()`: Search helper

### `ConditionExpr`
Parsed condition with:
- `list`: Flat list of all sub-expressions
- `albero`: Root of expression tree

## 📊 Supported Constructs

| Construct | Node Type | Notes |
|-----------|-----------|-------|
| Package spec/body | `PackageNode` | Nested packages supported |
| Procedure spec/body | `ProcedureNode` | Generic instantiations |
| Function spec/body | `FunctionNode` | Return types parsed |
| Task spec/body | `TaskNode` | |
| Entry spec/body | `EntryNode` | Guard conditions supported |
| Type declaration | `TypeDeclaration` | Records, arrays, derived, enums |
| Subtype | `TypeDeclaration` | Category: "subtype" |
| Representation clause | `TypeDeclaration` | `for...use record`, `for...use at` |
| Variable | `VariableDeclaration` | Multiple per line, with defaults |
| If/elsif/else | `IfStatement`, `ElsifStatement`, `ElseStatement` | |
| Case | `CaseStatement` | `when` clauses extracted |
| Simple loop | `SimpleLoop` | `exit when` parsed |
| While loop | `WhileLoop` | Condition parsed |
| For loop | `ForLoop` | Range/reverse/discrete types |
| Declare block | `DeclareNode` | |

## 🧪 Testing

The project includes comprehensive tests:

```bash
# Run all tests
cargo test

# Run with output
cargo test -- --nocapture

# Run specific test
cargo test test_full_integration_on_blob_ada
```

The test suite validates:
- Individual extractors (packages, procedures, loops, etc.)
- Expression parser with complex precedence
- Code cleaning (comments, strings, tabs)
- Full end-to-end parsing of a large Ada file (`blop.ada`)
- Tree structure verification

## 🎯 Use Cases

- **Linting**: Check coding standards (naming conventions, documentation, complexity)
- **Metrics**: Calculate cyclomatic complexity, lines of code, nesting depth
- **Refactoring**: Identify code smells, unused declarations
- **Documentation**: Auto-generate interface docs from specs
- **Migration**: Analyze legacy code for modernization
- **Education**: Teach Ada syntax and structure

## ⚙️ Architecture

1. **Regex Extraction**: Pattern matching identifies Ada constructs
2. **Node Creation**: Each match becomes a `NodeData` with metadata
3. **Sorting**: Nodes sorted by start position
4. **Tree Building**: Stack-based algorithm establishes parent-child relationships
5. **End Association**: Matches `end` statements to opening blocks
6. **Post-Processing**: Populates derived data (cases, conditions)

## 🤝 Contributing

Contributions are welcome! To contribute:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Make your changes with tests
4. Run the test suite (`cargo test`)
5. Commit your changes (`git commit -m 'Add amazing feature'`)
6. Push to the branch (`git push origin feature/amazing-feature`)
7. Open a Pull Request

Please ensure:
- All tests pass
- New features have corresponding tests
- Code follows Rust conventions
- Documentation is updated

## 📝 License

Licensed under the MIT License. See [LICENSE-MIT](LICENSE-MIT) for details.

## 👤 Author

**Francesco Abate**

- Computer Engineer specializing in software, embedded programming, cybersecurity, and AI
- Currently mastering Rust and seeking opportunities in the field
- Website: [https://frontinus.github.io/](https://frontinus.github.io/)
- Email: francesco1.abate@yahoo.com
- LinkedIn: [Connect with me](https://www.linkedin.com/in/francesco-abate-79601719b/)

## 🙏 Acknowledgments

Built with:

- [indextree](https://crates.io/crates/indextree) - Tree data structure
- [regex](https://crates.io/crates/regex) - Pattern matching
- [fancy-regex](https://crates.io/crates/fancy-regex) - Advanced regex features
- [lazy_static](https://crates.io/crates/lazy_static) - Static initialization

## 📚 Further Reading

- [API Documentation](https://docs.rs/ADA_Standards)
- [Ada Reference Manual](http://www.ada-auth.org/standards/rm12_w_tc1/html/RM-TOC.html)
- [Ada Style Guide](https://en.wikibooks.org/wiki/Ada_Style_Guide)

---

**Note**: This is a parser for analysis purposes, not a full Ada compiler. It's designed to be fast and flexible for tooling, but may not handle all edge cases of the Ada language specification.

---
