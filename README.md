ADA_ANALYZER.rs
==============

A library for analyzing Ada code to check if coding standards are respected in Rust.

Using this library, you can parse Ada code and programmatically verify its adherence to defined coding standards. This helps in ensuring code quality, consistency, and compliance within Ada projects.

[![Rust](https://github.com/frontinus/ADA_Standards_Lib/actions/workflows/rust.yml/badge.svg)](https://github.com/frontinus/ADA_Standards_Lib/actions/workflows/rust.yml)
[![Latest version](https://img.shields.io/crates/v/ADA_Standards.svg)](https://crates.io/crates/ADA_Standards)
[![Documentation](https://docs.rs/ADA_Standards/badge.svg)](https://docs.rs/ADA_Standards)
[![License](https://img.shields.io/crates/l/ADA_Standards.svg)](https://github.com/your-github-username/ada-analyzer#license)




# Getting Started

[ADA_Standards is available on crates.io](https://crates.io/crates/ADA_Standards).
It is recommended to look there for the newest released version, as well as links to the newest builds of the docs.

At the point of the last update of this README, the latest published version could be used like this:

Add the following dependency to your Cargo manifest...

```toml
[dependencies]
ADA_Standards = "0.3.0" 
```

...and see the [docs](https://docs.rs/ADA_Standards) for how to use it.

# Example

```rust
use ADA_Standards::{AST, NodeData, Expression, ConditionExpr, UnaryExpression, BinaryExpression, MembershipExpression, Unaries, Binaries, Memberships};
use std::env;


let args: Vec<String> = env::args().collect();


let file_path = &args[1];
let code_text = fs::read_to_string(file_path)?;
let cleaned_code = AST::clean_code(code_text);
let mut node_data_vec: Vec<NodeData> = Vec::new();
node_data_vec =extract_statement_nodes(cleaned_code,node_data_vec);
ast = AST::new(node_data_vec);
ast.build();
ast.print_tree();

let conditions = AST::parse_condition_expression(cleaned_code);

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