ADA_ANALYZER.rs
==============

A library for analyzing Ada code to check if coding standards are respected in Rust.

Using this library, you can parse Ada code and programmatically verify its adherence to defined coding standards. This helps in ensuring code quality, consistency, and compliance within Ada projects.

[![Rust](https://github.com/frontinus/ADA_Standards_Lib/actions/workflows/rust.yml/badge.svg)](https://github.com/frontinus/ADA_Standards_Lib/actions/workflows/rust.yml)
[![Latest version](https://img.shields.io/crates/v/ADA_Standards.svg)](https://crates.io/crates/ADA_Standards)
[![Documentation](https://docs.rs/ADA_Standards/badge.svg)](https://docs.rs/ADA_Standards)
[![License](https://img.shields.io/crates/l/ADA_Standards.svg)](https://github.com/your-github-username/ada-analyzer#license)

## Minimum supported `rustc`

`1.65.0+` (Adjust this based on your actual requirements)

This version is explicitly tested in CI and may only be bumped in new minor versions. Any changes to the supported minimum version will be called out in the release notes.


# Getting Started

[ADA_Standards is available on crates.io](https://crates.io/crates/ADA_Standards).
It is recommended to look there for the newest released version, as well as links to the newest builds of the docs.

At the point of the last update of this README, the latest published version could be used like this:

Add the following dependency to your Cargo manifest...

```toml
[dependencies]
ada-analyzer = "0.1.0" 

# Example

```rust
use lazy_static::lazy_static;
use std::collections::HashMap;

lazy_static! {
    static ref HASHMAP: HashMap<u32, &'static str> = {
        let mut m = HashMap::new();
        m.insert(0, "foo");
        m.insert(1, "bar");
        m.insert(2, "baz");
        m
    };
}

fn main() {
    // First access to `HASHMAP` initializes it
    println!("The entry for `0` is \"{}\".", HASHMAP.get(&0).unwrap());

    // Any further access to `HASHMAP` just returns the computed value
    println!("The entry for `1` is \"{}\".", HASHMAP.get(&1).unwrap());
}
```

# Standard library

It is now possible to easily replicate this crate's functionality in Rust's standard library with [`std::sync::LazyLock`](https://doc.rust-lang.org/std/sync/struct.LazyLock.html). The example above could also be written as:

```rust
use std::collections::HashMap;
use std::sync::LazyLock;

static HASHMAP: LazyLock<HashMap<u32, &str>> = LazyLock::new(|| {
    let mut m = HashMap::new();
    m.insert(0, "foo");
    m.insert(1, "bar");
    m.insert(2, "baz");
    m
});

fn main() {
    // First access to `HASHMAP` initializes it
    println!("The entry for `0` is \"{}\".", HASHMAP.get(&0).unwrap());

    // Any further access to `HASHMAP` just returns the computed value
    println!("The entry for `1` is \"{}\".", HASHMAP.get(&1).unwrap());
}
```

## License

Licensed under 


 * MIT license ([LICENSE-MIT](LICENSE-MIT) or https://opensource.org/licenses/MIT)

at your option.

### Contribution

You are free to contribute by forking and creating a pull request !


### Author


My name is Francesco Abate, I'm a computer engineer with experience in the fields of  software and embedded programming, cybersecurity and AI, I am currently working on my mastery of the Rust language and seeking a job in that field!

Feel free to visit my website: [Francesco Abate](https://frontinus.github.io/)