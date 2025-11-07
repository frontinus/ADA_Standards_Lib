# ADA_Standards

[![Rust](https://github.com/frontinus/ADA_Standards_Lib/actions/workflows/rust.yml/badge.svg)](https://github.com/frontinus/ADA_Standards_Lib/actions/workflows/rust.yml)
[![Latest version](https://img.shields.io/crates/v/ADA_Standards.svg)](https://crates.io/crates/ADA_Standards)
[![Documentation](https://docs.rs/ADA_Standards/badge.svg)](https://docs.rs/ADA_Standards)
[![License](https://img.shields.io/crates/l/ADA_Standards.svg)](https://github.com/frontinus/ada-analyzer#license)

A lightweight, regex-based Ada parser in Rust. Extracts packages, procedures, types, and more into an Abstract Syntax Tree (AST) for analysis, linting, and standards-checking.

## Features

* **Code Cleaning:** Includes a `clean_code` utility to strip comments while correctly preserving strings and file structure.
* **Comprehensive Extraction:** Parses dozens of Ada constructs, including:
    * `package` (spec and body)
    * `procedure` and `function` (spec, body, and `is new` instantiations)
    * `task` (spec and body) and `entry` (spec and body)
    * `type` and `subtype` (records, arrays, derived types, etc.)
    * Representation clauses (`for ... use at ...` and `for ... use record`)
    * `if` / `elsif` / `else` statements
    * `case` statements
    * `loop`, `while`, and `for` loops
    * `declare` blocks
    * Variable declarations (e.g., `A, B : Integer := 10;`)
* **Intelligent Parsing:**
    * Builds an `indextree`-based AST with correct parent-child hierarchy.
    * Parses procedure/function parameters into a structured `Vec<ArgumentData>`.
    * Parses `if`, `while`, and `exit when` conditions into a structured `ConditionExpr`.
    * Populates `case` nodes with their `when` clauses.

## Getting Started

**ADA_Standards** is available on [crates.io](https://crates.io/crates/ADA_Standards). You can add it to your project with:

```bash
cargo add ADA_Standards
```

Or, add the following to your Cargo.toml, replacing 1.1.1 with the latest version:

```toml
[dependencies]
ADA_Standards = "1.2.0" 
```

...and see the [docs](https://docs.rs/ADA_Standards) for how to use it.

## Example

```rust
use ADA_Standards::{AST, ASTError};
use std::fs;
use std::path::Path;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // 1. Read Ada source file
    let file_path = "path/to/your/ada_file.adb"; // Try it with blop.ada!
    let code_text = fs::read_to_string(file_path)?;

    // 2. Clean the code (removes comments, preserves strings and layout)
    let cleaned_code = AST::clean_code(&code_text);

    // 3. Extract all nodes from the code
    let nodes = AST::extract_all_nodes(&cleaned_code)?;

    // 4. Create a new AST and build the tree structure
    let mut ast = AST::new(nodes);
    ast.build(&cleaned_code)?; // This runs the end-of-block association

    // 5. Run post-processing to populate details
    ast.populate_cases(&cleaned_code)?;
    ast.populate_simple_loop_conditions(&cleaned_code)?;

    // 6. Analyze the tree!
    println!("Successfully built AST!");
    ast.print_tree();

    // Example: Find the 'myproc' procedure and list its direct children
    if let Some(estocazz_id) = ast.find_node_by_name_and_type("myproc", "ProcedureNode") {
        println!("\nChildren of 'myproc':");
        for child_id in estocazz_id.children(ast.arena()) {
            let child_node = ast.arena().get(child_id).unwrap().get();
            println!(
                "  - {} (Type: {}, Line: {:?})",
                child_node.name,
                child_node.node_type,
                child_node.start_line
            );
        }
    }
    
    Ok(())
}

```

## License

Licensed under

 * MIT license ([LICENSE-MIT](LICENSE-MIT) or https://opensource.org/licenses/MIT)

### Contribution

You are free to contribute by forking and creating a pull request !

### Author

My name is Francesco Abate, I'm a computer engineer with experience in the fields of  software and embedded programming, cybersecurity and AI, I am currently working on my mastery of the Rust language and seeking a job in that field!

Feel free to visit my website: [Francesco Abate](https://frontinus.github.io/)

You can also contact me @ francesco1.abate@yahoo.com or on linkedin 