#![allow(non_snake_case)]
//! # ADA_Standards
//!
//! [![crates.io](https://img.shields.io/crates/v/ADA_Standards.svg)](https://crates.io/crates/ADA_Standards)
//! [![docs.rs](https://docs.rs/ADA_Standards/badge.svg)](https://docs.rs/ADA_Standards)
//!
//! A comprehensive library for parsing and analyzing Ada source code to enforce coding standards
//! and identify potential issues through Abstract Syntax Tree (AST) analysis.
//!
//! ## Overview
//!
//! The `ADA_Standards` library provides a lightweight, regex-based approach to parsing Ada code.
//! It extracts language constructs (packages, procedures, types, control flow, etc.) and builds
//! a hierarchical tree structure that can be traversed and analyzed programmatically.
//!
//! ## Key Components
//!
//! - **NodeData**: Represents individual Ada constructs with metadata (location, type, parameters, etc.)
//! - **AST**: The main Abstract Syntax Tree structure with methods for building and querying
//! - **Expression Types**: Unary, Binary, Membership, and Condition expressions for parsing logic
//! - **ArgumentData**: Structured representation of procedure/function parameters
//!
//! ## Workflow
//!
//! 1. **Clean Code**: Remove comments and normalize strings while preserving structure
//! 2. **Extract Nodes**: Use regex patterns to identify all Ada constructs
//! 3. **Build Tree**: Establish parent-child relationships based on code structure
//! 4. **Post-Process**: Populate derived data like case alternatives and loop conditions
//! 5. **Analyze**: Traverse the tree to enforce standards or generate metrics
//!
//! ## Example
//!
//! ```rust
//! use ADA_Standards::{AST, ASTError};
//! use std::fs;
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let code = fs::read_to_string("blop.ada")?;
//! let cleaned = AST::clean_code(&code);
//! let nodes = AST::extract_all_nodes(&cleaned)?;
//! 
//! let mut ast = AST::new(nodes);
//! ast.build(&cleaned)?;
//! ast.populate_cases(&cleaned)?;
//! 
//! // Find and analyze a specific procedure
//! if let Some(proc_id) = ast.find_node_by_name_and_type("Main", "ProcedureNode") {
//!     let proc = ast.arena().get(proc_id).unwrap().get();
//!     println!("Found procedure at line {}", proc.start_line.unwrap());
//! }
//! # Ok(())
//! # }
//! ```

use indextree::{Arena, NodeId};
use regex::Regex;
use fancy_regex::Regex as Reg;
use lazy_static::lazy_static;

/// ANSI color codes for terminal output formatting.
///
/// These constants are used throughout the library for colorizing diagnostic
/// output when printing node information or tree structures.
#[allow(dead_code)]
mod text_format {
    pub const ENDC: &str = "\033[0m";
    pub const HEADER: &str = "\033[95m";
    pub const BLUE: &str = "\033[94m";
    pub const GREEN: &str = "\033[92m";
    pub const YELLOW: &str = "\033[93m";
    pub const RED: &str = "\033[91m";
    pub const CYAN: &str = "\033[96m";
    pub const MAGENTA: &str = "\033[95m";
    pub const WHITE: &str = "\033[97m";
    pub const BLACK: &str = "\033[90m";
    pub const BRIGHT_BLUE: &str = "\033[94;1m";
    pub const BRIGHT_GREEN: &str = "\033[92;1m";
    pub const BRIGHT_YELLOW: &str = "\033[93;1m";
    pub const BRIGHT_RED: &str = "\033[91;1m";
    pub const BRIGHT_CYAN: &str = "\033[96;1m";
    pub const BRIGHT_MAGENTA: &str = "\033[95;1m";
    pub const BRIGHT_WHITE: &str = "\033[97;1m";
    pub const BRIGHT_BLACK: &str = "\033[90;1m";
    pub const BOLD: &str = "\033[1m";
    pub const UNDERLINE: &str = "\033[4m";
}



/// Represents errors that can occur during AST construction or analysis.
///
/// This enum provides specific error types for different failure scenarios
/// encountered while parsing Ada code or building the tree structure.
///
/// # Variants
/// - `RegexError`: Failure to compile a regex pattern
/// - `NodeNotInArena(String)`: Attempt to access a node ID that doesn't exist in the arena
/// - `InvalidNodeData(String)`: Node data is missing required fields
/// - `StartNodeNotFound`: No matching start node found for an `end` statement
/// - `NodeIdMissing(String)`: A required node ID is missing during processing
/// - `NoMatchFound`: No regex match found in the provided text
/// - `TreeBuildError`: General error during AST tree building
/// - `MatchItemMissing`: A required regex match item was missing
/// - `InvalidCapture`: A required capture group was missing from a regex match
#[derive(Debug)]
pub enum ASTError {
    /// A required regex match item was missing
    MatchItemMissing,
    /// A required capture group was missing from a regex match
    InvalidCapture,
    /// General error during AST tree building
    TreeBuildError,
    /// No regex match found in the provided text
    NoMatchFound,
    /// A required node ID is missing during processing
    NodeIdMissing(String),
    /// No matching start node found for an `end` statement during tree building
    StartNodeNotFound,
    /// Attempt to access a node ID that doesn't exist in the arena
    NodeNotInArena(String),
    /// Failure to compile a regex pattern
    RegexError,
}



impl std::error::Error for ASTError {}

impl std::fmt::Display for ASTError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTError::MatchItemMissing => write!(f, "Required match item was missing"),
            ASTError::InvalidCapture => write!(f, "Required capture group was missing"),
            ASTError::TreeBuildError => write!(f, "Failed to build AST"),
            ASTError::NoMatchFound => write!(f, "No match found in the provided text"),
            ASTError::NodeIdMissing(node_id) => write!(f, "NodeId {} is missing", node_id),
            ASTError::StartNodeNotFound => write!(f, "Start node not found in arena during end line association"),
            ASTError::NodeNotInArena(node_id) => write!(f, "NodeId {} not found in arena", node_id),
            ASTError::RegexError => write!(f, "Regex error occurred"),
        }
    }
}



/// Represents Ada unary operators in conditional expressions.
///
/// Currently only supports the `not` operator, but structured as an enum
/// to allow for future expansion.
#[derive(Debug, Clone, PartialEq)]
pub enum Unaries {
    /// The logical NOT operator
    NOT,
}

/// Represents Ada membership test operators.
///
/// These operators test whether a value belongs to a range or type.
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, PartialEq)]
pub enum Memberships {
    /// The "not in" membership test (true if value NOT in range)
    NOT_IN,
    /// The "in" membership test (true if value in range)
    IN,
}

/// Represents Ada binary operators in conditional expressions.
///
/// Includes logical operators, comparison operators, and short-circuit variants.
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, PartialEq)]
pub enum Binaries {
    /// Logical AND
    AND,
    /// Logical OR
    OR,
    /// Short-circuit AND (right side not evaluated if left is false)
    AND_THEN,
    /// Short-circuit OR (right side not evaluated if left is true)
    OR_ELSE,
    /// Exclusive OR
    XOR,
    /// Less than comparison
    INFERIOR,
    /// Greater than comparison
    SUPERIOR,
    /// Less than or equal comparison
    INFERIOR_OR_EQUAL,
    /// Greater than or equal comparison
    SUPERIOR_OR_EQUAL,
    /// Equality comparison
    EQUAL,
    /// Inequality comparison
    UNEQUAL,
}

/// Represents different types of expressions in Ada conditional logic.
///
/// This enum is used to build expression trees from parsed conditions,
/// supporting nested operations with proper precedence handling.
#[derive(Debug, Clone)]
pub enum Expression {
    /// A unary operation (e.g., `not Found`)
    Unary(UnaryExpression),
    /// A binary operation (e.g., `X > 10`, `A and B`)
    Binary(BinaryExpression),
    /// A membership test (e.g., `X in 1..10`)
    Membership(MembershipExpression),
    /// A complete condition with both flat list and tree representation
    Condition(ConditionExpr),
    /// A literal value or identifier that cannot be further decomposed
    Literal(String),
}

/// Represents a unary operation in an expression tree.
///
/// # Fields
/// - `op`: The unary operator being applied
/// - `operand`: The expression being operated on
/// - `condstring`: The original string representation of this expression
#[derive(Debug, Clone)]
pub struct UnaryExpression {
    /// The unary operator (e.g., NOT)
    pub op: Unaries,
    /// The operand being operated on (boxed for recursive structure)
    pub operand: Box<Expression>,
    /// Original string representation of this expression
    pub condstring: String,
}

/// Represents a binary operation in an expression tree.
///
/// Binary operations include logical operators (and, or) and comparison
/// operators (<, >, =, etc.). The tree structure allows for proper
/// precedence handling during parsing.
///
/// # Fields
/// - `op`: The binary operator being applied
/// - `left`: The left operand expression
/// - `right`: The right operand expression
/// - `condstring`: The original string representation of this expression
#[derive(Debug, Clone)]
pub struct BinaryExpression {
    /// The binary operator (e.g., AND, SUPERIOR)
    pub op: Binaries,
    /// The left operand (boxed for recursive structure)
    pub left: Box<Expression>,
    /// The right operand (boxed for recursive structure)
    pub right: Box<Expression>,
    /// Original string representation of this expression
    pub condstring: String,
}

/// Represents a membership test expression.
///
/// Membership tests check if a value belongs to a range or type,
/// using the `in` or `not in` operators.
///
/// # Fields
/// - `op`: The membership operator (IN or NOT_IN)
/// - `left`: The value being tested
/// - `right`: The range or type being tested against
/// - `condstring`: The original string representation
#[derive(Debug, Clone)]
pub struct MembershipExpression {
    /// The membership operator (IN or NOT_IN)
    pub op: Memberships,
    /// The value being tested (boxed for recursive structure)
    pub left: Box<Expression>,
    /// The range or type being tested against (boxed for recursive structure)
    pub right: Box<Expression>,
    /// Original string representation of this expression
    pub condstring: String,
}

/// Represents a parsed conditional expression.
///
/// Contains both a flat list of all sub-expressions (useful for iteration)
/// and a tree representation (useful for evaluation and precedence analysis).
///
/// # Fields
/// - `list`: Flat list of all expressions found during parsing
/// - `albero`: Root of the expression tree (Italian for "tree")
#[derive(Debug, Clone)]
pub struct ConditionExpr {
    /// Flat list of all expressions (useful for linear traversal)
    pub list: Vec<Expression>,
    /// Root of the expression tree (useful for hierarchical analysis)
    pub albero: Option<Box<Expression>>,
}

/// Represents a single procedure, function, or entry parameter.
///
/// This struct captures all relevant information about a formal parameter,
/// including its name, mode (in/out/in out), type, and optional default value.
///
/// # Examples
/// ```
/// use ADA_Standards::ArgumentData;
/// 
/// let arg = ArgumentData {
///     name: "Count".to_string(),
///     mode: "in out".to_string(),
///     data_type: "Integer".to_string(),
///     default_value: Some("0".to_string()),
/// };
/// ```
#[derive(Debug, Clone)]
pub struct ArgumentData {
    /// The parameter name (e.g., "Count", "Buffer")
    pub name: String,
    /// The parameter mode: "in", "out", or "in out"
    pub mode: String,
    /// The declared type (e.g., "Integer", "String", "My_Type")
    pub data_type: String,
    /// Optional default value as a string (e.g., "0", "True", "null")
    pub default_value: Option<String>,
}

/// Represents the return type of a function.
///
/// This struct captures the return type information from a function declaration.
/// The type is optional to handle edge cases in parsing.
#[derive(Debug, Clone)]
pub struct ReturnKeywordData {
    /// The return type (e.g., "Integer", "Boolean", "My_Type")
    pub data_type: Option<String>,
}

/// Internal structure representing a found `end` statement.
///
/// Used during tree building to match `end` statements with their
/// corresponding opening constructs (package, procedure, loop, etc.).
///
/// # Fields
/// - `word`: What follows "end" (e.g., "loop", "if", or a name like "My_Proc")
/// - `index`: Character position where "end" starts
/// - `line`: Line number where "end" appears
/// - `end_index`: Character position where the statement ends (after semicolon)
#[derive(Debug, Clone)]
pub struct EndStatement {
    /// What follows "end" (keyword or name)
    pub word: String,
    /// Character index of the "end" keyword
    pub index: usize,
    /// Line number of the "end" statement
    pub line: usize,
    /// Character index after the semicolon
    pub end_index: usize,
}

/// Keywords that appear in unnamed `end` statements.
///
/// These constructs end with "end <keyword>;" rather than "end <name>;".
/// For example: "end loop;", "end if;", "end case;", "end record;"
const UNNAMED_END_KEYWORDS: [&str; 4] = ["loop", "if", "case", "record"];

/// Internal enum for tracking parse events during tree building.
///
/// Represents either the start of a node or an `end` statement,
/// allowing the builder to maintain a stack-based hierarchy.
enum ParseEvent {
    /// A node starting at the given index
    StartNodeId { node_id: NodeId, index: usize },
    /// An end statement with its details
    End { word: String, index: usize, line: usize, end_index: usize },
}

/// The core data structure representing an Ada construct in the AST.
///
/// `NodeData` is designed to accommodate many different types of Ada constructs
/// (packages, procedures, types, loops, etc.) through a flexible field structure.
/// Not all fields are relevant for all node types.
///
/// # Common Fields (used by most nodes)
/// - `name`, `node_type`: Identity and classification
/// - `start_line`, `end_line`: Source location
/// - `start_index`, `end_index`: Character positions
/// - `is_body`: Whether it's a body vs. specification
///
/// # Specialized Fields
/// Different node types use different subsets of fields:
/// - **Procedures/Functions**: `arguments`, `return_type`, `pkg_name`
/// - **Types**: `type_kind`, `base_type`, `tuple_values`
/// - **Control Flow**: `conditions`, `iterator`, `range_start`, `switch_expression`
/// - **Case Statements**: `switch_expression`, `cases`
///
/// # Examples
/// ```
/// use ADA_Standards::NodeData;
/// 
/// let proc_node = NodeData::new(
///     "Calculate".to_string(),
///     "ProcedureNode".to_string(),
///     Some(10),
///     Some(150),
///     true, // is_body
/// );
/// ```
#[derive(Debug,Clone)] 
pub struct NodeData {
    // --- Common Fields (Used by almost all nodes) ---
    
    /// The name of the node (e.g., "MyPackage", "SimpleLoop", "IfStatement").
    pub name: String,
    /// The type of node (e.g., "PackageNode", "LoopNode", "IfStatement").
    pub node_type: String,
    /// The line number where the node's declaration begins.
    pub start_line: Option<usize>,
    /// The line number where the node's block ends (e.g., `end MyPackage;`).
    pub end_line: Option<usize>,
    /// The character index (from the start of the file) where the node begins.
    pub start_index: Option<usize>,
    /// The character index (from the start of the file) where the node ends.
    pub end_index: Option<usize>,
    /// The column number where the node's declaration begins.
    pub column: Option<usize>,
    /// The character index for the start of the node's body (e.g., after `is` or `then`).
    pub body_start: Option<usize>,
    /// Reference to parent node (Note: indextree handles actual parentage)
    pub parent : Option<Box<NodeData>>, 
                                       

    // --- Package/Procedure/Function/Task/Entry Fields ---
    
    /// `true` if the node is a `body` (e.g., `package body`), `false` for a spec.
    pub is_body: Option<bool>,
    /// For nested procedures/packages, this field *may* be populated. 
    pub pkg_name: Option<String>,
    /// The category of declaration (e.g., "generic", "separate" for packages,
    /// "type", "subtype", "for" for type declarations).
    pub category_type: Option<String>,
    /// A list of parsed parameters for a procedure, function, or entry.
    pub arguments: Option<Vec<ArgumentData>>,
    /// The return type for a `FunctionNode`.
    pub return_type: Option<ReturnKeywordData>,

    // --- TypeDeclaration Fields ---
    
    /// The kind of type (e.g., "record", "derived", "tuple", "at_clause").
    pub type_kind: Option<String>,
    /// The parent type for a derived type or representation clause
    /// (e.g., "Integer" for `type My_Int is new Integer;`).
    /// Also used for the address in an `at_clause`.
    pub base_type: Option<String>,
    /// A list of values for an enumeration (tuple) type (e.g., `(Red, Green, Blue)`).
    pub tuple_values: Option<Vec<String>>,

    // --- Control Flow / Condition Fields ---
    
    /// The parsed condition for `If`, `Elsif`, `While`, `Entry`, or `exit when`.
    pub conditions: Option<ConditionExpr>,

    // --- ForLoop Fields ---
    
    /// The loop iterator variable (e.g., `I` in `for I in ...`).
    pub iterator: Option<String>,
    /// The start of a loop range (e.g., `1` in `1 .. 10`).
    pub range_start: Option<String>,
    /// The end of a loop range (e.g., `10` in `1 .. 10`).
    pub range_end: Option<String>,
    /// The loop direction (e.g., "to" (default) or "reverse").
    pub direction: Option<String>,
    /// A discrete range variable (e.g., `My_Array'Range`).
    pub range_var: Option<String>,
    /// The type of a ranged loop (e.g., `Integer` in `for I in Integer range 1 .. 10`).
    pub iterator_type: Option<String>,

    // --- CaseStatement Fields ---
    
    /// The expression being switched on (e.g., `Day` in `case Day is ...`).
    pub switch_expression: Option<String>,
    /// The list of `when` clauses populated by `populate_cases`.
    pub cases: Option<Vec<String>>,
}


impl NodeData {
    /// Creates a new `NodeData` instance with default values.
    ///
    /// Initializes a node with the given name, type, start position, and body status,
    /// setting all other specific fields to `None`. This is the primary constructor
    /// used by all extraction functions.
    ///
    /// # Parameters
    /// - `name`: The identifier of the construct (e.g., "MyPackage")
    /// - `node_type`: The type classification (e.g., "PackageNode")
    /// - `start_line`: Line number where the construct begins
    /// - `start_index`: Character index where the construct begins
    /// - `is_body`: Whether this is a body (true) or specification (false)
    ///
    /// # Returns
    /// A `NodeData` instance with initialized common fields and all specialized fields set to `None`.
    ///
    /// # Examples
    /// ```
    /// use ADA_Standards::NodeData;
    /// 
    /// let node = NodeData::new(
    ///     "My_Procedure".to_string(),
    ///     "ProcedureNode".to_string(),
    ///     Some(10),
    ///     Some(150),
    ///     false, // is a specification
    /// );
    /// assert_eq!(node.name, "My_Procedure");
    /// assert_eq!(node.is_body, Some(false));
    /// ```
    pub fn new(name: String, node_type: String, start_line: Option<usize>, start_index: Option<usize>,is_body: bool) -> Self {
        NodeData {
            
            name,
            node_type,
            start_line,
            end_line: None, // Initially end_line is None
            start_index,
            end_index: None, // Initially end_index is None
            column: None,
            is_body: Some(is_body),
            pkg_name: None,
            category_type: None,
            arguments: None,
            return_type: None,
            type_kind: None,
            base_type: None,
            tuple_values: None,
            conditions: None,
            iterator: None,
            range_start: None,
            range_end: None,
            direction: None,
            range_var: None,
            iterator_type: None,
            switch_expression: None,
            cases: None,
            parent: None,
            body_start: None,
        }
    }

    /// Prints detailed, color-coded information about the node to stdout.
    ///
    /// This method displays all non-None fields of the node using ANSI color codes
    /// for better readability. It's primarily intended for debugging and development,
    /// showing the complete state of a node including nested structures like
    /// arguments and conditions.
    ///
    /// # Output Format
    /// - Common fields (name, type, lines) are printed in blue
    /// - Arguments are printed in yellow with green headers
    /// - Conditions are printed in green with yellow details
    /// - Each nested structure is indented for clarity
    ///
    /// # Notes
    /// - Uses ANSI escape codes, so output may not render properly on all terminals
    /// - For production use, consider logging instead of printing
    /// - Large condition trees may produce verbose output
    ///
    /// # Examples
    /// ```no_run
    /// use ADA_Standards::NodeData;
    /// 
    /// let node = NodeData::new(
    ///     "Test".to_string(),
    ///     "ProcedureNode".to_string(),
    ///     Some(1),
    ///     Some(0),
    ///     true
    /// );
    /// node.print_info(); // Prints colored output to stdout
    /// ```
    pub fn print_info(&self) {
        println!("{}  Category: {} {}{}, ", text_format::BLUE, text_format::ENDC, self.node_type, ", ");
        println!("{}  Name: {} {}{}", text_format::BLUE, text_format::ENDC, self.name, ", ");
        if let Some(start_line) = self.start_line {
            println!("{}  Start Line: {} {}{}", text_format::BLUE, text_format::ENDC, start_line, ", ");
        }
        if let Some(end_line) = self.end_line {
            println!("{}  End Line: {} {}{}", text_format::BLUE, text_format::ENDC, end_line, ", ");
        }
        if let Some(pkg_name) = &self.pkg_name {
            println!("{}  Package Name: {} {}{}", text_format::BLUE, text_format::ENDC, pkg_name, ", ");
        }
        if let Some(category_type) = &self.category_type {
            println!("{}  Category Type: {} {}{}", text_format::BLUE, text_format::ENDC, category_type, ", ");
        }
        if let Some(is_body) = self.is_body {
            println!("{}  Is Body: {} {}{}", text_format::BLUE, text_format::ENDC, if is_body { "Body" } else { "Spec" }, ", ");
        }
         if let Some(arguments) = &self.arguments {
            println!("{}  Arguments: {}", text_format::GREEN, text_format::ENDC);
            for arg in arguments {
                println!("{}    Name: {} {}{}", text_format::YELLOW, text_format::ENDC, arg.name, ", ");
                println!("{}    Mode: {} {}{}", text_format::YELLOW, text_format::ENDC, arg.mode, ", ");
                println!("{}    Data Type: {} {}{}", text_format::YELLOW, text_format::ENDC, arg.data_type, ", ");
                if let Some(default_value) = &arg.default_value {
                    println!("{}    Default Value: {} {}{}", text_format::YELLOW, text_format::ENDC, default_value, ", ");
                }
                println!("    ------");
            }
        }
        if let Some(return_type) = &self.return_type {
            if let Some(data_type) = &return_type.data_type {
                 println!("{}  Return Type: {} {}{}", text_format::RED, text_format::ENDC, data_type, ", ");
            }
        }
        if let Some(type_kind) = &self.type_kind {
            println!("{}  Type Kind: {} {}{}", text_format::BLUE, text_format::ENDC, type_kind, ", ");
        }
        if let Some(base_type) = &self.base_type {
            println!("{}  Base Type: {} {}{}", text_format::RED, text_format::ENDC, base_type, ", ");
        }
        if let Some(tuple_values) = &self.tuple_values {
            println!("{}  Tuple Values: {} {:?}", text_format::RED, text_format::ENDC, tuple_values);
        }
        if let Some(conditions) = &self.conditions {
             println!("{}  Conditions: {}", text_format::GREEN, text_format::ENDC);
             let cond_list = &conditions.list ;
             for cond_expr in cond_list {
                     match cond_expr {
                         Expression::Unary(unary_expr) => {
                             println!("{}    Unary Cond: {:?} {:?}{:?}{:?}", text_format::YELLOW, text_format::ENDC, unary_expr.op, unary_expr.operand, ", ");
                         },
                         Expression::Binary(binary_expr) => {
                             println!("{}    Binary Cond: {:?} {:?}{:?}{:?}{:?}", text_format::YELLOW, text_format::ENDC, binary_expr.left, binary_expr.op, binary_expr.right, ", ");
                         },
                         Expression::Membership(membership_expr) => {
                             println!("{}   Membership Cond: {:?} {:?}{:?}{:?}{:?}", text_format::YELLOW, text_format::ENDC, membership_expr.left, membership_expr.op, membership_expr.right, ", ");
                         },
                         Expression::Literal(literal_expr) => {
                             println!("{}    Literal Cond: {:?} {:?}{:?}", text_format::YELLOW, text_format::ENDC, literal_expr, ", ");
                         }
                         Expression::Condition(_condition_expr) => {
                             println!("{}    Nested Condition Expr:  {:?} {:?}", text_format::YELLOW, text_format::ENDC, _condition_expr);
                         }
                     }
                     println!("    ------");
                 }
             
        }
        if let Some(iterator) = &self.iterator {
            println!("{}  Iterator: {} {}{}", text_format::CYAN, text_format::ENDC, iterator, ", ");
        }
        if let Some(iterator_type) = &self.iterator_type {
            println!("{}  Iterator Type: {} {}{}", text_format::YELLOW, text_format::ENDC, iterator_type, ", ");
        }
        if let Some(range_var) = &self.range_var {
            println!("{}  Range Var: {} {}{}", text_format::WHITE, text_format::ENDC, range_var, ", ");
        }
        if let Some(range_start) = &self.range_start {
            println!("{}  Range Start: {} {}{}", text_format::WHITE, text_format::ENDC, range_start, ", ");
        }
        if let Some(range_end) = &self.range_end {
            println!("{}  Range End: {} {}{}", text_format::GREEN, text_format::ENDC, range_end, ", ");
        }
        if let Some(direction) = &self.direction {
            println!("{}  Direction: {} {}{}", text_format::RED, text_format::ENDC, direction, ", ");
        }
        if let Some(switch_expression) = &self.switch_expression {
            println!("{}  Switch Expression: {} {}{}", text_format::BRIGHT_BLUE, text_format::ENDC, switch_expression, ", ");
        }
        if let Some(cases) = &self.cases {
             println!("{}  Cases: {}", text_format::RED, text_format::ENDC);
             for case in cases {
                 println!("{}    Case: {} {}{}", text_format::RED, text_format::ENDC, case, ", ");
                 println!("    ------");
             }
        }

        // ... add more print_info logic based on Python's print_info methods in nodes.py
    }
}

/// Represents the Abstract Syntax Tree (AST) for Ada source code.
///
/// The `AST` struct is the main interface for parsing and analyzing Ada code.
/// It manages a collection of `NodeData` instances in an `indextree::Arena`,
/// providing methods to extract nodes, build hierarchical relationships,
/// and query the resulting tree.
///
/// # Fields
/// - `arena`: The indextree arena that owns all node data
/// - `root_id`: ID of the synthetic root node (parent of all top-level constructs)
/// - `nodes_data`: Pre-build list of extracted nodes
/// - `node_ids`: Mapping from nodes_data indices to arena NodeIds
///
/// # Workflow
/// 1. Create with `new()` from a vector of extracted nodes
/// 2. Call `build()` to establish parent-child relationships
/// 3. Call post-processing methods like `populate_cases()`
/// 4. Query and traverse the tree using `find_node_by_name_and_type()` or arena methods
///
/// # Examples
/// ```
/// use ADA_Standards::{AST, NodeData};
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// let code = "package Test is\nend Test;";
/// let cleaned = AST::clean_code(code);
/// let nodes = AST::extract_all_nodes(&cleaned)?;
/// 
/// let mut ast = AST::new(nodes);
/// ast.build(&cleaned)?;
/// 
/// // Traverse the tree
/// for node_id in ast.root_id().descendants(ast.arena()) {
///     let node = ast.arena().get(node_id).unwrap().get();
///     println!("{}", node.name);
/// }
/// # Ok(())
/// # }
/// ``` 
pub struct AST {
    /// The `indextree` arena that owns all `NodeData` in the tree.
    pub arena: Arena<NodeData>,
    /// The `NodeId` of the synthetic root node, which is the parent of all
    /// top-level nodes found in the source code.
    pub root_id: NodeId,
    /// The raw, sorted list of nodes extracted from the text, pre-build.
    /// This is primarily used internally by the `build` function.
    pub nodes_data: Vec<NodeData>,
    /// A temporary vector mapping `nodes_data` indices to their `NodeId` in the arena.
    /// This is populated during the `build` process.
    pub node_ids: Vec<Option<NodeId>>,
}

impl AST {
    /// Creates a new `AST` instance.
    ///
    /// Initializes an empty arena, creates a synthetic root node,
    /// and stores the provided `nodes_data` for later processing by `build`.
    /// The `node_ids` vector is initialized to be parallel to `nodes_data`.
    pub fn new(nodes_data: Vec<NodeData>) -> Self {
        let mut arena =  Arena::new();
        let root_id = arena.new_node(NodeData::new("root".to_string(), "RootNode".to_string(), None, None, false)); // Create root node
        // Initialize node_ids to match nodes_data length so callers can call build() directly.
        let node_ids = vec![None; nodes_data.len()];
        AST {
            arena,
            root_id,
            nodes_data,
            node_ids,
        }
    }

    /// Public accessor for the root NodeId so external code and doc-tests can use the root.
    pub fn root_id(&self) -> NodeId {
        self.root_id
    }

    /// Public accessor for the arena reference so external code and doc-tests can traverse nodes.
    pub fn arena(&self) -> &Arena<NodeData> {
        &self.arena
    }



    /// Helper for `build` that associates `end` statements with their start nodes.
    ///
    /// This function operates directly on the nodes in the `arena` *after* they
    /// have been sorted and added.
    fn associate_end_lines_in_arena(&mut self, code_text: &str, node_ids: &[NodeId]) -> Result<(), ASTError> {
        // 1. Extract all end statements
        let end_statements = AST::extract_end_statements(code_text)?;

        // 2. Get NodeIds and start indices for nodes needing an end line.
        let start_node_ids: Vec<(NodeId, usize)> = node_ids.iter()
            .filter(|&&node_id| self.arena.get(node_id).unwrap().get().end_line.is_none()) // Filter nodes without end_line
            .map(|&node_id| (node_id, self.arena.get(node_id).unwrap().get().start_index.unwrap_or(0))) // Map to (NodeId, start_index)
            .collect();

        // 3. Create combined event list (NodeId starts + End statements)
        let mut events: Vec<ParseEvent> = Vec::new();
        for &(node_id, start_index) in &start_node_ids {
            events.push(ParseEvent::StartNodeId { node_id, index: start_index });
        }
        for end_statement in &end_statements {
            events.push(ParseEvent::End {
                word: end_statement.word.clone(),
                index: end_statement.index,
                line: end_statement.line,
                end_index: end_statement.end_index,
            });
        }

        // 4. Sort events by character index
        events.sort_by_key(|e| match e {
            ParseEvent::StartNodeId { index, .. } => *index,
            ParseEvent::End { index, .. } => *index,
        });

        // 5. Process events using a stack of NodeIds
        let mut stack: Vec<NodeId> = vec![];

        for event in events {
            match event {
                ParseEvent::StartNodeId { node_id, .. } => {
                    stack.push(node_id);
                }
                ParseEvent::End { word, line, end_index, .. } => {
                    // Define the matching logic based on the 'end' word
                    let match_logic = |node_id: &NodeId| -> bool {
                        if let Some(node) = self.arena.get(*node_id) {
                            let node_data = node.get();
                            let keyword_type = AST::get_end_keyword(&node_data.node_type);

                            if word.is_empty() {
                                keyword_type == Some("") || keyword_type == Some("name")
                            } else if UNNAMED_END_KEYWORDS.contains(&word.as_str()) { // <-- This uses the constant
                                keyword_type == Some(word.as_str())
                            } else {
                                node_data.name == word && keyword_type == Some("name")
                            }
                        } else { false }
                    };

                    // --- THIS IS THE CORRECT LOGIC ---
                    // Find the most recent node on the stack that matches
                    if let Some(pos) = stack.iter().rposition(match_logic) {
                        let node_id_to_update = stack.remove(pos);
                        // Update the node directly in the arena
                        if let Some(node) = self.arena.get_mut(node_id_to_update) {
                            let node_data = node.get_mut();
                            node_data.end_line = Some(line);
                            node_data.end_index = Some(end_index);
                        } else {
                            return Err(ASTError::NodeNotInArena(format!("NodeId {:?} not found during update", node_id_to_update)));
                        }
                    } else {
                        // This 'end' statement doesn't match anything on the stack.
                        // This is NOT an error, just an unmatched 'end'.
                        eprintln!("Warning: Unmatched 'end {}' at line {}", word, line);
                    }
                    // --- END CORRECT LOGIC ---
                }
            }
        }

        if !stack.is_empty() {
            eprintln!("Warning: Stack not empty after associating end lines. Mismatched blocks?");
        }

        Ok(())
    }



    /// Builds the complete AST hierarchy from extracted nodes.
    ///
    /// This is the core logic that transforms a flat list of nodes into a proper
    /// tree structure. It performs several critical steps:
    ///
    /// # Steps
    /// 1. **Sort nodes** by start index (ensures chronological processing)
    /// 2. **Populate arena** with all nodes
    /// 3. **Associate end lines** by matching `end` statements to opening blocks
    /// 4. **Build tree structure** using a stack-based algorithm
    ///
    /// # Algorithm
    /// The tree building uses a stack to track the current nesting context:
    /// - Start with root on the stack
    /// - For each node (in order):
    ///   - Pop from stack any parents that have ended before this node starts
    ///   - Attach current node to the top of the stack (its parent)
    ///   - Push current node onto stack if it's a container (has no end_line initially)
    ///
    /// # Parameters
    /// - `code_text`: The cleaned source code (used for end line association)
    ///
    /// # Returns
    /// - `Ok(())` if the tree was built successfully
    /// - `Err(ASTError)` if there was a problem during construction
    ///
    /// # Examples
    /// ```
    /// use ADA_Standards::AST;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let code = "package P is\n  procedure Q;\nend P;";
    /// let cleaned = AST::clean_code(code);
    /// let nodes = AST::extract_all_nodes(&cleaned)?;
    /// 
    /// let mut ast = AST::new(nodes);
    /// ast.build(&cleaned)?; // Builds the tree
    /// 
    /// // Now we can traverse it
    /// assert!(ast.root_id().children(ast.arena()).count() > 0);
    /// # Ok(())
    /// # }
    /// ```
    pub fn build(&mut self, code_text: &str) -> Result<(), ASTError> {
        // 1. Sort nodes_data by start index FIRST.
        self.nodes_data.sort_by_key(|n| n.start_index.unwrap_or(0));

        // 2. Reset arena and create root node
        self.arena = Arena::new();
        self.root_id = self.arena.new_node(NodeData::new("root".to_string(), "RootNode".to_string(), None, None, false));

        // 3. Populate the arena and create node_ids LOCALLY
        let node_ids: Vec<NodeId> = self.nodes_data.iter()
            .map(|node_data| self.arena.new_node(node_data.clone()))
            .collect();

        // 4. Associate end lines *now*, operating directly on the arena
        self.associate_end_lines_in_arena(code_text, &node_ids)?;

        // --- Tree Building Logic ---
        let mut stack: Vec<NodeId> = vec![self.root_id]; // Stack of NodeIds

        // Iterate using the node_ids we just created
        for (index, &current_node_id) in node_ids.iter().enumerate() {
            
            let current_start_line = {
                let node_data_ref = self.arena.get(current_node_id).unwrap().get();
                node_data_ref.start_line.unwrap_or(0)
            };

            // Pop parents from the stack until we find the correct one.
            while let Some(&parent_id) = stack.last() {
                if parent_id == self.root_id {
                    break; // Never pop the root
                }

                let parent_data = self.arena.get(parent_id).unwrap().get();

                if let Some(parent_end_line) = parent_data.end_line {
                    if parent_end_line < current_start_line {
                        stack.pop();
                    } else {
                        break;
                    }
                } else {
                    // Parent's end_line is None (unclosed block)
                    break;
                }
            }

            // Append to the parent currently on top of the stack
            if let Some(&parent_node_id) = stack.last() {
                parent_node_id.append(current_node_id, &mut self.arena);
            } else {
                self.root_id.append(current_node_id, &mut self.arena);
            }

            // --- THIS IS THE FIX ---
            // Get the *original* node data (pre-association)
            let node_data_original = &self.nodes_data[index];
            
            // We only push a node onto the stack if it's a container.
            // A container is any node that *originally* had no end line
            // (because its end is a separate "end ..." statement).
            if node_data_original.end_line.is_none() {
                // This is the logic from the buggy version, but because
                // `extract_procedures_functions` is now fixed,
                // `procedure Free` will have its `end_line` set and
                // will NOT be pushed onto the stack.
                stack.push(current_node_id);
            }
            // --- END FIX ---
        }
        Ok(())
    }

    /// Prints a colorized, human-readable representation of the AST to stdout.
    ///
    /// This method traverses the tree and prints each node with indentation
    /// to show nesting levels. It uses ANSI color codes for better readability:
    /// - Red for node types
    /// - Blue for node names
    ///
    /// # Output Format
    /// ```text
    /// RootNode - root
    ///   PackageNode - MyPackage (start_line: 1, end_line: 10)
    ///     ProcedureNode - DoWork (start_line: 2, end_line: 5)
    ///       IfStatement - IfStatement (start_line: 3, end_line: 4)
    /// ```
    ///
    /// # Examples
    /// ```no_run
    /// use ADA_Standards::AST;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let code = "package P is\nend P;";
    /// let cleaned = AST::clean_code(code);
    /// let nodes = AST::extract_all_nodes(&cleaned)?;
    /// let mut ast = AST::new(nodes);
    /// ast.build(&cleaned)?;
    /// 
    /// ast.print_tree(); // Prints colorized tree to stdout
    /// # Ok(())
    /// # }
    /// ```      
    pub fn print_tree(&self) {
            println!("{}", self.output_tree());
    }

    /// Returns a string representation of the AST tree structure.
    ///
    /// Similar to `print_tree()`, but returns the string instead of printing.
    /// Useful for logging, testing, or further processing.
    ///
    /// # Returns
    /// A multi-line string with the complete tree structure, including
    /// node types, names, and line numbers (where available).
    ///
    /// # Examples
    /// ```
    /// use ADA_Standards::AST;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let code = "package P is\nend P;";
    /// let cleaned = AST::clean_code(code);
    /// let nodes = AST::extract_all_nodes(&cleaned)?;
    /// let mut ast = AST::new(nodes);
    /// ast.build(&cleaned)?;
    /// 
    /// let tree_string = ast.output_tree();
    /// assert!(tree_string.contains("PackageNode"));
    /// # Ok(())
    /// # }
    /// ```
    pub fn output_tree(&self) -> String {
            let mut output = String::new();
            for node_id in self.root_id.descendants(&self.arena) {
                let indent_level = node_id.ancestors(&self.arena).count() - 1; // Root is at level 0
                let indent = "  ".repeat(indent_level);
                let node = self.arena.get(node_id).unwrap();
                output += &format!(
                    "{}{}{}{} - {}{}{}",
                    indent,
                    text_format::RED,
                    node.get().node_type,
                    text_format::ENDC,
                    text_format::BLUE,
                    node.get().name,
                    text_format::ENDC
                );
                if let (Some(start_line), Some(end_line)) = (node.get().start_line, node.get().end_line) {
                    if start_line != usize::MAX && end_line != usize::MAX { // Check against usize::MAX if you use it as default
                        output += &format!(" (start_line: {}, end_line: {})", start_line, end_line);
                    }
                }
                output += "\n";
            }
            output
    }

    /// Prints detailed info for all nodes in the original `nodes_data` list.
    ///
    /// **Note**: This iterates over `self.nodes_data`, not the final tree in `self.arena`.
    /// It's primarily useful for debugging the extraction phase before building.
    /// After `build()` is called, the arena should be considered the source of truth.
    ///
    /// # Returns
    /// - `Ok(())` if all nodes were printed successfully
    /// - `Err(ASTError::NodeNotInArena)` if a node ID is invalid
    ///
    /// # Examples
    /// ```no_run
    /// use ADA_Standards::AST;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let code = "package P is\nend P;";
    /// let cleaned = AST::clean_code(code);
    /// let nodes = AST::extract_all_nodes(&cleaned)?;
    /// let ast = AST::new(nodes);
    /// 
    /// ast.print_nodes_info()?; // Prints detailed info for each extracted node
    /// # Ok(())
    /// # }
    /// ```
    pub fn print_nodes_info(&self) -> Result<(), ASTError> {
        for index in 0..self.nodes_data.len() {
            if let Some(node_id) = self.node_ids[index] {
                let node = self.arena.get(node_id).ok_or_else(||ASTError::NodeNotInArena(format!("Node with ID {:?} not found in arena", node_id)))?; 
                node.get().print_info();
                
            }
        }
        Ok(())
    }

    /// Returns the expected closing keyword for a given node type.
    ///
    /// This helper method is used during end line association to determine
    /// what kind of `end` statement should close a particular construct.
    ///
    /// # Parameters
    /// - `node_type`: The type of node (e.g., "PackageNode", "IfStatement")
    ///
    /// # Returns
    /// - `Some("name")`: Expects `end <name>;` (e.g., procedures, packages)
    /// - `Some("loop")`: Expects `end loop;`
    /// - `Some("if")`: Expects `end if;`
    /// - `Some("case")`: Expects `end case;`
    /// - `Some("record")`: Expects `end record;`
    /// - `Some("")`: Expects `end;` (e.g., declare blocks)
    /// - `None`: Node doesn't have a corresponding end (e.g., variable declarations)
    ///
    /// # Examples
    /// ```
    /// use ADA_Standards::AST;
    /// 
    /// assert_eq!(AST::get_end_keyword("PackageNode"), Some("name"));
    /// assert_eq!(AST::get_end_keyword("IfStatement"), Some("if"));
    /// assert_eq!(AST::get_end_keyword("SimpleLoop"), Some("loop"));
    /// assert_eq!(AST::get_end_keyword("DeclareNode"), Some(""));
    /// assert_eq!(AST::get_end_keyword("ElsifStatement"), None);
    /// ```
    pub fn get_end_keyword(node_type: &str) -> Option<&'static str> {
        match node_type {
            // Blocks that end with "end <name>;"
            "PackageNode" => Some("name"),
            "ProcedureNode" => Some("name"),
            "FunctionNode" => Some("name"),
            
            // --- THIS IS THE FIX ---
            "TaskNode" => Some("name"),
            "EntryNode" => Some("name"),
            // --- END FIX ---

            // Blocks that end with "end <keyword>;"
            "SimpleLoop" => Some("loop"),
            "WhileLoop" => Some("loop"),
            "ForLoop" => Some("loop"),
            "IfStatement" => Some("if"),
            "CaseStatement" => Some("case"),
            
            // Match "end record;"
            "TypeDeclaration" => Some("record"), 

            // Blocks that end with "end;"
            "DeclareNode" => Some(""),
            
            // These blocks don't have their own "end"
            "ElsifStatement" => None,
            "ElseStatement" => None,
            
            _ => None,
        }
    }

    /// Extracts all `end ...;` statements from the source code.
    ///
    /// This function identifies all `end` keywords followed by an optional
    /// identifier or keyword, and a semicolon. The results are used by
    /// `associate_end_lines_in_arena()` to match blocks with their closures.
    ///
    /// # Pattern Matching
    /// - `end;` → empty word
    /// - `end loop;` → word = "loop"
    /// - `end My_Procedure;` → word = "My_Procedure"
    ///
    /// # Parameters
    /// - `code_text`: The cleaned source code
    ///
    /// # Returns
    /// - `Ok(Vec<EndStatement>)`: List of all found end statements
    /// - `Err(ASTError::RegexError)`: If the regex pattern is invalid
    ///
    /// # Examples
    /// ```
    /// use ADA_Standards::AST;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let code = "procedure Test is\nbegin\n  null;\nend Test;";
    /// let cleaned = AST::clean_code(code);
    /// let ends = AST::extract_end_statements(&cleaned)?;
    /// 
    /// assert_eq!(ends.len(), 1);
    /// assert_eq!(ends[0].word, "Test");
    /// assert_eq!(ends[0].line, 4);
    /// # Ok(())
    /// # }
    /// ```
    pub fn extract_end_statements(code_text: &str) -> Result<Vec<EndStatement>, ASTError> {
        // FIX: Added \s* before semicolon
        let re = Regex::new(r#"(?im)^\s*end(\s+([\w\.]+))?\s*;"#) 
            .map_err(|_| ASTError::RegexError)?;
        
        let mut ends = vec![];
        
        for cap in re.captures_iter(code_text) {
            let entire_match = cap.get(0).ok_or(ASTError::InvalidCapture)?;
            let word = cap.get(2).map_or("", |m| m.as_str()).to_string();
            let index = entire_match.start();
            let line = code_text[..index].matches('\n').count() + 1;
            
            // FIX: Capture the end index of the match
            let end_index = entire_match.end(); 
            
            // FIX: Store the end_index
            ends.push(EndStatement { word, index, line, end_index }); 
        }
        
        Ok(ends)
    }

    /// Extracts Ada package declarations from source code, capturing metadata like name and whether it’s a body or specification.
    ///
    /// # Examples
    /// ```
    /// use ADA_Standards::{NodeData, ASTError, AST};
    /// # fn main() -> Result<(), ASTError> {
    /// let code = "package My_Package is\n   -- Package spec\nend My_Package;";
    /// let c_code = AST::clean_code(code);
    /// let nodes = AST::extract_packages(&c_code)?;
    /// assert_eq!(nodes.len(), 1);
    /// assert_eq!(nodes[0].name, "My_Package");
    /// assert_eq!(nodes[0].node_type, "PackageNode");
    /// assert_eq!(nodes[0].is_body, Some(false)); // Specification
    /// Ok(())
    /// # }
    /// ```

    pub fn extract_packages(code_text: &str) -> Result<Vec<NodeData>, ASTError> {
        let package_pattern = Reg::new(
            r#"(?im)^(?!\s*--)\s*(?P<type>\b(?:generic|separate)\b)?\s*(?P<category>\bpackage\b)(?:\s+(?P<body>\bbody\b))?\s+(?P<name>\S+)"#
        ).map_err(|_| ASTError::RegexError)?;
        

        let mut nodes: Vec<NodeData> = Vec::new();
        let matches: Vec<_> = package_pattern.captures_iter(code_text)
            .collect::<Result<Vec<_>, _>>()
            .map_err(|_| ASTError::RegexError)?;
        println!("{:?}",matches);
        
        // Sort matches like Python: by base name, presence of dots, and start index
        let mut sorted_matches: Vec<_> = matches.into_iter().collect();
        sorted_matches.sort_by(|a, b| {
            let a_name = a.name("name").unwrap().as_str();
            let b_name = b.name("name").unwrap().as_str();
            let a_base = a_name.split('.').next().unwrap();
            let b_base = b_name.split('.').next().unwrap();
            a_base.cmp(b_base)
                .then(a_name.contains('.').cmp(&b_name.contains('.')))
                .then(a.get(0).unwrap().start().cmp(&b.get(0).unwrap().start()))
        });

        for mat in sorted_matches {
            let full_match = mat.get(0).ok_or(ASTError::MatchItemMissing)?;
            let category_keyword = mat.name("category").ok_or(ASTError::InvalidCapture)?;
            let start_line = code_text[..category_keyword.start()].matches('\n').count() + 1;
            let start_index = category_keyword.start();
            let name = mat.name("name").unwrap().as_str().to_string();
            let is_body = mat.name("body").is_some();

            let search_text = &code_text[full_match.end()..];
            let end_search = Regex::new(r"\s*(is|;)")
                .unwrap()
                .find(search_text);

            let mut node = NodeData::new(name.clone(), "PackageNode".to_string(), Some(start_line), Some(start_index), is_body);
            if let Some(end_match) = end_search {
                if end_match.as_str().trim() == ";" {
                    node.end_line = Some(start_line);
                    node.end_index = Some(full_match.end() + end_match.end());
                }
            }

            // Handle nested packages
            let depth_dot_level = name.chars().filter(|&c| c == '.').count();
            if depth_dot_level > 0 {
                let parts: Vec<&str> = name.split('.').collect();
                let mut current_parent = None;

                for depth in 0..depth_dot_level {
                    let parent_name = parts[depth].to_string();
                    let parent_node = nodes.iter_mut().find(|n| n.name == parent_name);

                    if let Some(parent) = parent_node {
                        if start_index < parent.start_index.unwrap_or(usize::MAX) && parent.end_line.is_some() {
                            parent.start_index = Some(start_index);
                            parent.start_line = Some(start_line);
                        }
                        current_parent = Some(parent.clone());
                    } else {
                        let mut parent = NodeData::new(
                            parent_name.clone(),
                            "PackageNode".to_string(),
                            Some(start_line),
                            Some(start_index),
                            true, // Implicitly a body if created here
                        );
                        parent.end_line = Some(usize::MAX); // Placeholder, like Python's -1
                        if let Some(prev_parent) = current_parent.take() {
                            parent.parent = Some(Box::new(prev_parent));
                        }
                        nodes.push(parent.clone());
                        current_parent = Some(parent);
                    }
                }

                node.name = parts[depth_dot_level].to_string();
                node.parent = current_parent.map(Box::new);
            }

            nodes.push(node);
        }

        // Sort by start_line like Python
        nodes.sort_by(|a, b| a.start_line.unwrap_or(usize::MAX).cmp(&b.start_line.unwrap_or(usize::MAX)));
        Ok(nodes)
    }

    /// Extracts Ada procedure declarations from source code, capturing metadata like name and whether it’s a body or specification.
    ///
    /// # Examples
    /// ```
    /// use ADA_Standards::{NodeData, ASTError, AST};
    /// # fn main() -> Result<(), ASTError> {
    /// let code = "procedure Free is new Ada.Unchecked_Deallocation (Object => Memory_Type,Name => Memory_Access);";
    /// let c_code = AST::clean_code(code);
    /// let nodes = AST::extract_procedures_functions(&c_code)?;
    /// assert_eq!(nodes.len(), 1);
    /// assert_eq!(nodes[0].name, "Free");
    /// assert_eq!(nodes[0].node_type, "ProcedureNode");
    /// println!("{:?}",nodes[0].is_body);
    /// assert_eq!(nodes[0].is_body, Some(false)); // Specification
    /// Ok(())
    /// # }
    /// ```
    pub fn extract_procedures_functions(code_text: &str) -> Result<Vec<NodeData>, ASTError> {
        // Regex is fine, it captures the signature.
        let func_proc_pattern = Reg::new(
            r"(?im)^(?!\s*--)(?:^[^\S\n]*|(?<=\n))(?P<category>\bprocedure|function\b)\s+(?P<name>[^\s\(\;]*)(?:\s*\((?P<params>[\s\S]*?(?=\)))\))?(?:\s*return\s*(?P<return_statement>[\w\.\_\-]+))?"
        ).map_err(|_| ASTError::RegexError)?;

        let end_pattern = Regex::new(r"(?m)\s*(is|;)").map_err(|_| ASTError::RegexError)?;
        let mut nodes: Vec<NodeData> = Vec::new();

        for cap_res in func_proc_pattern.captures_iter(code_text) {
            let mat = cap_res.map_err(|_| ASTError::RegexError)?;
            let full_match = mat.get(0).ok_or(ASTError::MatchItemMissing)?;
            let category_keyword = mat.name("category").ok_or(ASTError::InvalidCapture)?;

            let start_line = code_text[..category_keyword.start()].matches('\n').count() + 1;
            let start_index = category_keyword.start();
            let category = category_keyword.as_str().to_lowercase();
            let name = mat.name("name").unwrap().as_str().to_string();

            // --- NEW is_body LOGIC ---
            let mut is_body = false;
            let search_text = &code_text[full_match.end()..];
            let end_match_opt = end_pattern.find(search_text);

            if let Some(end_match) = end_match_opt {
                if end_match.as_str().trim() == "is" {
                    // It's a body *unless* it's "is new"
                    let text_after_is = &search_text[end_match.end()..];
                    if !text_after_is.trim_start().starts_with("new") {
                        is_body = true;
                    }
                }
                // if it's ";", is_body stays false.
            }
            // --- END NEW LOGIC ---

            let mut node = NodeData::new(
                name.clone(),
                if category == "function" { "FunctionNode" } else { "ProcedureNode" }.to_string(),
                Some(start_line),
                Some(start_index),
                is_body,
            );

            node.arguments = Some(AST::parse_parameters(mat.name("params").map(|m| m.as_str())));
            
            // --- NEW END_LINE LOGIC ---
            if !is_body {
                // It's a spec (e.g., "proc X;" or "proc X is new Y;").
                // We MUST find its closing semicolon to set the end_line.
                // This prevents it from being pushed onto the build stack.
                let spec_search_text = &code_text[full_match.end()..];
                // Find the *first* semicolon after the signature
                if let Some(semicolon_match) = Regex::new(r";").unwrap().find(spec_search_text) {
                    let end_pos = full_match.end() + semicolon_match.end();
                    node.end_line = Some(code_text[..end_pos].matches('\n').count() + 1);
                    node.end_index = Some(end_pos);
                }
            }
            // --- END NEW LOGIC ---
            
            // ... (rest of function: nested names, etc. is fine) ...
            let depth_dot_level = name.chars().filter(|&c| c == '.').count();
            if depth_dot_level > 0 {
                let parts: Vec<&str> = name.split('.').collect();
                let mut current_parent = None;

                for depth in 0..depth_dot_level {
                    let parent_name = parts[depth].to_string();
                    let parent_node = nodes.iter_mut().find(|n| n.name == parent_name);

                    if let Some(parent) = parent_node {
                        if start_index < parent.start_index.unwrap_or(usize::MAX) && parent.end_line.is_some() {
                            parent.start_index = Some(start_index);
                            parent.start_line = Some(start_line);
                        }
                        current_parent = Some(parent.clone());
                    } else {
                        let mut parent = NodeData::new(
                            parent_name.clone(),
                            "PackageNode".to_string(),
                            Some(start_line),
                            Some(start_index),
                            true,
                        );
                        parent.end_line = Some(usize::MAX); // Placeholder
                        if let Some(prev_parent) = current_parent.take() {
                            parent.parent = Some(Box::new(prev_parent));
                        }
                        nodes.push(parent.clone());
                        current_parent = Some(parent);
                    }
                }

                node.name = parts[depth_dot_level].to_string();
                node.parent = current_parent.map(Box::new);
            }

            nodes.push(node);
        }
        Ok(nodes)
    }

    /// Extracts `type`, `subtype`, and `for...use` declarations from Ada source code.
    ///
    /// # Examples
    /// ```
    /// use ADA_Standards::{NodeData, ASTError, AST};
    /// # fn main() -> Result<(), ASTError> {
    /// let code = r#"
    /// procedure estocazz is
    ///    type CustomType is record
    ///       DataString : String(1 .. 3) := "oui";
    ///       DataFloat : Float;
    ///    end record;
    ///
    ///    SMR : Some_Register_Type;
    ///    for SMR use at 16#F000#; -- This is the 'at' clause
    /// begin
    ///    null;
    /// end estocazz;
    /// "#;
    /// let c_code = AST::clean_code(code);
    /// let nodes = AST::extract_type_declarations(&c_code)?;
    ///
    /// // Should find 2 nodes: CustomType and the 'for' clause
    /// assert_eq!(nodes.len(), 2);
    ///
    /// let type_node = nodes.iter()
    ///     .find(|n| n.name == "CustomType")
    ///     .expect("Did not find 'CustomType' node");
    /// assert_eq!(type_node.start_line, Some(3));
    ///
    /// let for_node = nodes.iter()
    ///     .find(|n| n.name == "SMR")
    ///     .expect("Did not find 'for ... use at' node");
    ///
    /// assert_eq!(for_node.category_type, Some("for".to_string()));
    /// assert_eq!(for_node.type_kind, Some("at_clause".to_string()));
    /// assert_eq!(for_node.start_line, Some(9));
    /// assert_eq!(for_node.base_type, Some("16#F000#".to_string()));
    ///
    /// Ok(())
    /// # }
    /// ```
    pub fn extract_type_declarations(code_text: &str) -> Result<Vec<NodeData>, ASTError> {
        // PATTERN 1: Matches 'type' and 'subtype' declarations.
        let type_subtype_pattern = Reg::new(
            r#"(?ims)^(?!\s*--)(?:^[^\S\n]*|(?<=\n))(?P<category>\btype\b|\bsubtype\b)\s+(?P<name>.*?)(?:\s+is(?:\s+new\s+(?P<base_type>[\w\._\-]+)|(?:\s*(?P<tuple_type>\([^)]+\))|\s*(?P<type_kind>[\w\._\-]+(?:\s*range\s*.*?)?))))"#
        ).map_err(|_| ASTError::RegexError)?;

        // PATTERN 2: Matches 'for ... use record' representation clauses.
        let repr_clause_pattern = Reg::new(
            r#"(?im)^(?!\s*--)(?:^[^\S\n]*|(?<=\n))(?P<category>\bfor\b)\s+(?P<name>\w+)\s+use\s+(?P<type_kind>\brecord\b)"#
        ).map_err(|_| ASTError::RegexError)?;

        // --- NEW PATTERN ---
        // PATTERN 3: Matches 'for ... use at ...' representation clauses.
        let repr_at_pattern = Reg::new(
            r#"(?ims)^(?!\s*--)(?:^[^\S\n]*|(?<=\n))(?P<category>\bfor\b)\s+(?P<name>\S+)\s+use\s+at\s+(?P<address>.*?);"#
        ).map_err(|_| ASTError::RegexError)?;
        // --- END NEW PATTERN ---

        let mut nodes: Vec<NodeData> = Vec::new();

        // --- Pass 1: Find 'type' and 'subtype' declarations ---
        for cap_res in type_subtype_pattern.captures_iter(code_text) {
            let caps = cap_res.map_err(|_| ASTError::RegexError)?;
            let full_match = caps.get(0).ok_or(ASTError::MatchItemMissing)?;
            let category_match = caps.name("category").ok_or(ASTError::InvalidCapture)?;

            let start_index = full_match.start();
            let start_line = code_text[..start_index].matches('\n').count() + 1;
            let column = start_index - code_text[..start_index].rfind('\n').map_or(0, |i| i + 1);

            let category = category_match.as_str().to_lowercase();
            let name = caps.name("name").ok_or(ASTError::InvalidCapture)?.as_str().trim().to_string();

            let type_kind = caps.name("type_kind").map(|m| m.as_str().trim())
                .or_else(|| caps.name("tuple_type").map(|_| "tuple"))
                .or_else(|| caps.name("base_type").map(|_| "derived"))
                .unwrap_or("")
                .to_lowercase();

            let tuple_values = if let Some(tuple_match) = caps.name("tuple_type") {
                let values: Vec<String> = tuple_match.as_str()
                    .trim_matches(|c| c == '(' || c == ')') // Remove surrounding parentheses
                    .split(',')
                    .map(|s| s.trim().to_string())
                    .filter(|s| !s.is_empty())
                    .collect();
                Some(values)
            } else {
                None
            };

            let (end_line, end_index) = if type_kind == "record" {
                (None, None) // Records are multi-line, `associate_end_lines` will find the end
            } else {
                // Most other types are single-line declarations ending in ';'
                let end_pos = code_text[full_match.end()..].find(';').map(|p| full_match.end() + p + 1);
                if let Some(pos) = end_pos {
                    (Some(code_text[..pos].matches('\n').count() + 1), Some(pos))
                } else {
                    (None, None)
                }
            };

            let mut node = NodeData::new(
                name,
                "TypeDeclaration".to_string(),
                Some(start_line),
                Some(start_index),
                false // Type declarations are specifications
            );

            node.column = Some(column);
            node.end_line = end_line;
            node.end_index = end_index;
            node.category_type = Some(category); // "type" or "subtype"
            node.type_kind = Some(type_kind);
            node.base_type = caps.name("base_type").map(|m| m.as_str().to_lowercase());
            node.tuple_values = tuple_values;

            nodes.push(node);
        }

        // --- Pass 2: Find 'for ... use record' clauses ---
        for cap_res in repr_clause_pattern.captures_iter(code_text) {
            let caps = cap_res.map_err(|_| ASTError::RegexError)?;
            let category_keyword = caps.name("category").ok_or(ASTError::InvalidCapture)?;

            let start_index = category_keyword.start();
            let start_line = code_text[..start_index].matches('\n').count() + 1;
            let column = start_index - code_text[..start_index].rfind('\n').map_or(0, |i| i + 1);
            
            let mut node = NodeData::new(
                caps.name("name").ok_or(ASTError::InvalidCapture)?.as_str().to_string(),
                "TypeDeclaration".to_string(),
                Some(start_line),
                Some(start_index),
                false,
            );

            node.column = Some(column);
            node.category_type = Some("for".to_string()); // Category is "for"
            node.type_kind = Some("record".to_string()); // The kind is always record for this pattern
            
            nodes.push(node);
        }

        // --- NEW SECTION ---
        // --- Pass 3: Find 'for ... use at' clauses ---
        for cap_res in repr_at_pattern.captures_iter(code_text) {
            let caps = cap_res.map_err(|_| ASTError::RegexError)?;
            let full_match = caps.get(0).ok_or(ASTError::MatchItemMissing)?;
            let category_keyword = caps.name("category").ok_or(ASTError::InvalidCapture)?;

            let start_index = category_keyword.start();
            let start_line = code_text[..start_index].matches('\n').count() + 1;
            let column = start_index - code_text[..start_index].rfind('\n').map_or(0, |i| i + 1);
            
            let name = caps.name("name").ok_or(ASTError::InvalidCapture)?.as_str().to_string();
            let address = caps.name("address").ok_or(ASTError::InvalidCapture)?.as_str().trim().to_string();

            let mut node = NodeData::new(
                name,
                "TypeDeclaration".to_string(),
                Some(start_line),
                Some(start_index),
                false,
            );

            node.column = Some(column);
            node.category_type = Some("for".to_string());
            node.type_kind = Some("at_clause".to_string()); // New kind
            node.base_type = Some(address); // Store the address in base_type
            
            // This is a single-line declaration ending in ';'
            node.end_line = Some(code_text[..full_match.end()].matches('\n').count() + 1);
            node.end_index = Some(full_match.end());
            
            nodes.push(node);
        }
        // --- END NEW SECTION ---

        Ok(nodes)
    }

    /// Extracts Ada `declare` blocks from source code.
    ///
    /// This function identifies `declare` keywords that are not part of a comment
    /// and are not inside a string literal.
    ///
    /// # Examples
    /// ```
    /// use ADA_Standards::{NodeData, ASTError, AST};
    /// # fn main() -> Result<(), ASTError> {
    /// let code = r#"
    /// procedure Main is
    ///     My_Str : String := "this should not be a match: declare";
    ///   begin
    ///     declare -- This is the one it should find
    ///       X : Integer;
    ///     begin
    ///       null;
    ///     end;
    ///   end Main;
    /// "#;
    /// let c_code = AST::clean_code(code);
    /// let nodes = AST::extract_declare_blocks(&c_code)?;
    /// assert_eq!(nodes.len(), 1); // Finds 1, not 2
    /// assert_eq!(nodes[0].name, "DeclareBlock");
    /// assert_eq!(nodes[0].node_type, "DeclareNode");
    /// assert_eq!(nodes[0].start_line, Some(5)); // Matches the correct line
    /// Ok(())
    /// # }
    /// ```
    pub fn extract_declare_blocks(code_text: &str) -> Result<Vec<NodeData>, ASTError> {
        // FIX: Removed the complex lookahead.
        let declare_pattern = Reg::new(
            r#"(?im)^(?!\s*--)\s*(?P<declare>\bdeclare\b)"#
        ).map_err(|_| ASTError::RegexError)?;

        let mut nodes = Vec::new();
        // ... (rest of the function is identical) ...
        for cap_res in declare_pattern.captures_iter(code_text) {
            let caps = cap_res.map_err(|_| ASTError::RegexError)?;
            let mat = caps.name("declare").ok_or(ASTError::InvalidCapture)?;
            let start_line = code_text[..mat.start()].matches('\n').count() + 1;
            let start_index = mat.start();
            let node = NodeData::new(
                "DeclareBlock".to_string(),
                "DeclareNode".to_string(),
                Some(start_line),
                Some(start_index),
                true,
            );
            nodes.push(node);
        }
        Ok(nodes)
    }

    /// Extracts all control flow nodes (loops) from the source code.
    ///
    /// This is a convenience wrapper that calls:
    /// - `extract_simple_loops()`
    /// - `extract_while_loops()`
    /// - `extract_for_loops()`
    ///
    /// # Parameters
    /// - `code_text`: The cleaned source code
    ///
    /// # Returns
    /// A vector of all loop nodes found in the code.
    pub fn extract_control_flow_nodes(code_text: &str) -> Result<Vec<NodeData>,ASTError> {
        let mut new_nodes_data: Vec<NodeData> = Vec::new();
        new_nodes_data.extend(AST::extract_simple_loops(code_text)?);
        new_nodes_data.extend(AST::extract_while_loops(code_text)?);
        new_nodes_data.extend(AST::extract_for_loops(code_text)?);
        // REMOVE this line: nodes.extend(new_nodes_data.clone());
        Ok(new_nodes_data) // Just return the new list
    }

    /// Extracts `loop ... end loop` simple loops from Ada source code.
    ///
    /// This function identifies the `loop` keyword (when at the start of a line)
    /// and records its start position and the start of its body.
    ///
    /// # Examples
    /// ```
    /// use ADA_Standards::{NodeData, ASTError, AST};
    /// # fn main() -> Result<(), ASTError> {
    /// let code = r#"
    ///   procedure Main is
    ///   begin
    ///     loop -- a simple loop
    ///       x := x + 1;
    ///     end loop;
    ///   end Main;
    /// "#;
    /// let c_code = AST::clean_code(code);
    /// let nodes = AST::extract_simple_loops(&c_code)?;
    /// assert_eq!(nodes.len(), 1);
    /// assert_eq!(nodes[0].name, "SimpleLoop");
    /// assert_eq!(nodes[0].start_line, Some(4));
    ///
    /// // Find the index of "x := x + 1;"
    /// let body_start_index = c_code.find("x := x + 1;").unwrap();
    /// // The node's body_start should point right before that
    /// assert!(nodes[0].body_start.is_some());
    /// assert!(nodes[0].body_start.unwrap() < body_start_index);
    /// Ok(())
    /// # }
    /// ```
    pub fn extract_simple_loops(code_text: &str) -> Result<Vec<NodeData>, ASTError> {
        // FIX 1: Added 'm' for multiline flag
        let simpleloops_pattern = Regex::new(r"(?im)^\s*(?P<Captureloop>\bloop\b)")
            .map_err(|_| ASTError::RegexError)?;
        let mut nodes = Vec::new();

        for mat in simpleloops_pattern.captures_iter(code_text) {
            let full_match = mat.get(0).ok_or(ASTError::InvalidCapture)?; // e.g., "  loop"
            let loop_keyword = mat.name("Captureloop").ok_or(ASTError::InvalidCapture)?; // e.g., "loop"

            // FIX 2: Use .matches('\n') for correct line counting
            let start_line = code_text[..loop_keyword.start()].matches('\n').count() + 1;
            
            // FIX 3: Use the start of the "loop" keyword as the start_index
            let start_index = loop_keyword.start();
            
            let mut node = NodeData::new(
        "SimpleLoop".to_string(),
    "SimpleLoop".to_string(),
    Some(start_line),
    Some(start_index),
    false, // Sticking with your original value
    );

            // NEW: Save the index *after* the "loop" line as the body_start
            // This is necessary for the post-processing step.
            node.body_start = Some(full_match.end());

            nodes.push(node);
        }

        Ok(nodes)
    }

    /// # Examples
    /// ```
    /// use ADA_Standards::{NodeData, ASTError, AST, Expression};
    /// # fn main() -> Result<(), ASTError> {
    /// // Use a minimal code example
    /// let code = r#"
    ///   loop
    ///     exit when X > 10; -- This is the condition
    ///   end loop;
    /// "#;
    /// let c_code = AST::clean_code(code);
    ///
    /// // 1. Extract all nodes
    /// let nodes = AST::extract_all_nodes(&c_code)?;
    /// assert_eq!(nodes.len(), 1); // Should only find the SimpleLoop
    ///
    /// // 2. Create and build the AST
    /// let mut ast = AST::new(nodes);
    /// ast.build(&c_code)?; // This runs associate_end_lines_in_arena
    ///
    /// // 3. Run the post-processing function
    /// ast.populate_simple_loop_conditions(&c_code)?;
    ///
    /// // 4. Find the loop node and check its conditions
    ///
    /// // FIX: Access ast.root_id and &ast.arena directly
    /// // FIX: Remove the incorrect first attempt to find the node before build
    /// let loop_node_id = ast.root_id.children(&ast.arena).next() // Get first child of root
    ///     .ok_or(ASTError::NoMatchFound)?;
    ///
    /// // FIX: Access ast.arena directly
    /// let loop_node_data = ast.arena.get(loop_node_id)
    ///     .ok_or(ASTError::NoMatchFound)?
    ///     .get();
    ///
    /// // Check that it is the correct node
    /// assert_eq!(loop_node_data.node_type, "SimpleLoop");
    ///
    /// // Check that the conditions were populated
    /// let conds = loop_node_data.conditions.as_ref()
    ///     .ok_or(ASTError::InvalidCapture)?; // Fails if conditions is None
    ///
    /// // Use assert!(matches!...) for a clean check
    /// assert!(matches!(conds.albero.as_deref(), Some(Expression::Binary(_))));
    ///
    /// // You can still check the condstring if you want
    /// if let Some(Expression::Binary(bin_expr)) = conds.albero.as_ref().map(|b| b.as_ref()) {
    ///     assert_eq!(bin_expr.condstring.trim(), "X > 10");
    /// }
    /// Ok(())
    /// # }
    /// ```
    pub fn populate_simple_loop_conditions(&mut self, code_text: &str) -> Result<(), ASTError> {
        let exit_pattern = Regex::new(r"(?im)^\s*exit\s+when\s*(?P<exitcond1>[^;]*)")
            .map_err(|_| ASTError::RegexError)?;
            
        let mut updates: Vec<(NodeId, ConditionExpr)> = Vec::new();

        for node_id in self.root_id.descendants(&self.arena) {
            let node = self.arena.get(node_id).unwrap().get();

            if node.node_type == "SimpleLoop" {
                if let (Some(body_start), Some(end_index)) = (node.body_start, node.end_index) {
                    if body_start >= end_index {
                        continue;
                    }

                    let body_text = &code_text[body_start..end_index];
                        
                    // FIX IS HERE:
                    // We rename `last_match` to `cond_match` in the `if let`
                    // and remove the line `let cond_match = ...`
                    if let Some(cond_match) = exit_pattern.captures_iter(body_text).last() {
                        // The line below was removed:
                        // let cond_match = last_match.map_err(|_| ASTError::RegexError)?;
                        
                        let cond_string = cond_match.name("exitcond1")
                                        .map_or("", |m| m.as_str())
                                        .trim()
                                        .to_string();
                            
                        if !cond_string.is_empty() {
                            let conditions = AST::parse_condition_expression(&cond_string);
                            updates.push((node_id, conditions));
                        }
                    }
                }
            }
        }
            
            // Apply all updates to the arena
        for (node_id, conditions) in updates {
            if let Some(node) = self.arena.get_mut(node_id) {
                node.get_mut().conditions = Some(conditions);
            }
        }
            
        Ok(())
    }

    /// Extracts `while` loops from Ada source code.
    ///
    /// Parses the provided source code to identify `while` loops, capturing their loop condition,
    /// start position, and other metadata. Creates `NodeData` instances for each `while` loop,
    /// setting the `node_type` to "WhileLoop" and populating fields like `conditions`,
    /// `start_line`, `start_index`, and `body_start`.
    ///
    /// # Examples
    /// ```
    /// use ADA_Standards::{NodeData, ASTError, AST, Expression};
    /// # fn main() -> Result<(), ASTError> {
    /// let code = r#"procedure Main is
    ///     X : Integer := 0;
    ///   begin
    ///     while X < 10 loop -- a while loop
    ///       X := X + 1;
    ///     end loop;
    ///   end Main;
    /// "#;
    /// let c_code = AST::clean_code(code);
    /// let nodes = AST::extract_while_loops(&c_code)?;
    ///
    /// // 1. Check that the loop was found
    /// assert_eq!(nodes.len(), 1);
    /// let loop_node = &nodes[0];
    /// assert_eq!(loop_node.name, "WhileLoop");
    /// assert_eq!(loop_node.start_line, Some(4));
    ///
    /// // 2. Check the condition
    /// let conds = loop_node.conditions.as_ref().ok_or(ASTError::NoMatchFound)?;
    /// assert!(matches!(conds.albero.as_deref(), Some(Expression::Binary(_))));
    ///
    /// // 3. Check the condition string
    /// if let Some(Expression::Binary(bin_expr)) = conds.albero.as_ref().map(|b| b.as_ref()) {
    ///     assert_eq!(bin_expr.condstring.trim(), "X < 10");
    /// } else {
    ///     panic!("Condition was not parsed as a binary expression");
    /// }
    /// Ok(())
    /// # }
    /// ```
    pub fn extract_while_loops(code_text: &str) -> Result<Vec<NodeData>, ASTError> {
        // FIX: Added 's' (dotall) flag and changed `(\n|.)*?` to `.*?` for efficiency
        let whileloops_pattern = Regex::new(
            r"(?ims)(?P<Capturewhile>\bwhile\b)\s*(?P<exitcond2>.*?)\s*\bloop\b[^\n;]*"
        ).map_err(|_| ASTError::RegexError)?;
        
        let mut nodes = Vec::new();

        for mat in whileloops_pattern.captures_iter(code_text) {
            let full_match = mat.get(0).ok_or(ASTError::InvalidCapture)?;
            let while_keyword = mat.name("Capturewhile").ok_or(ASTError::InvalidCapture)?;
            
            // FIX: Use .matches('\n') and the start of the "while" keyword
            let start_line = code_text[..while_keyword.start()].matches('\n').count() + 1;
            
            // FIX: Use the start of the "while" keyword, not the whole match
            let start_index = while_keyword.start();
            
            let condition_str = mat.name("exitcond2").unwrap().as_str().to_string();
            let conditions = AST::parse_condition_expression(&condition_str);
            
            let mut node = NodeData::new(
                "WhileLoop".to_string(),
                "WhileLoop".to_string(),
                Some(start_line),
                Some(start_index),
                false,
            );
            
            node.conditions = Some(conditions);
            
            // Set body_start to the end of the full match (after "loop")
            node.body_start = Some(full_match.end()); 
            
            nodes.push(node);
        }

        Ok(nodes)
    }



    /// Extracts `for` loops from Ada source code.
    ///
    /// Parses `for` loops, capturing the iterator, direction, and the different
    /// forms of loop ranges (e.g., discrete type range, simple range, or array/range variable).
    ///
    /// # Examples
    ///
    /// ```
    /// use ADA_Standards::{NodeData, ASTError, AST};
    /// # fn main() -> Result<(), ASTError> {
    /// // Test Case 1: Simple range (e.g., 1 .. 10)
    /// let code1 = r#"procedure Main is
    ///   begin
    ///     for I in 1 .. 10 loop
    ///       null;
    ///     end loop;
    ///   end Main;
    /// "#;
    /// let c_code1 = AST::clean_code(code1);
    /// let nodes1 = AST::extract_for_loops(&c_code1)?;
    /// assert_eq!(nodes1.len(), 1);
    /// assert_eq!(nodes1[0].iterator, Some("I".to_string()));
    /// assert_eq!(nodes1[0].range_start, Some("1".to_string()));
    /// assert_eq!(nodes1[0].range_end, Some("10".to_string()));
    /// assert_eq!(nodes1[0].iterator_type, None);
    /// assert_eq!(nodes1[0].range_var, None);
    ///
    /// // Test Case 2: Range with type (e.g., Integer range 1 .. 10)
    /// let code2 = r#"for J in reverse Integer range 1 .. 10 loop
    ///       null;
    ///     end loop;
    /// "#;
    /// let c_code2 = AST::clean_code(code2);
    /// let nodes2 = AST::extract_for_loops(&c_code2)?;
    /// assert_eq!(nodes2.len(), 1);
    /// assert_eq!(nodes2[0].iterator, Some("J".to_string()));
    /// assert_eq!(nodes2[0].direction, Some("reverse".to_string()));
    /// assert_eq!(nodes2[0].iterator_type, Some("Integer".to_string()));
    /// assert_eq!(nodes2[0].range_start, Some("1".to_string()));
    /// assert_eq!(nodes2[0].range_end, Some("10".to_string()));
    ///
    /// // Test Case 3: Range variable (e.g., My_Array'Range)
    /// let code3 = r#"
    ///     for K in My_Array'Range loop
    ///       null;
    ///     end loop;
    /// "#;
    /// let c_code3 = AST::clean_code(code3);
    /// let nodes3 = AST::extract_for_loops(&c_code3)?;
    /// assert_eq!(nodes3.len(), 1);
    /// assert_eq!(nodes3[0].iterator, Some("K".to_string()));
    /// assert_eq!(nodes3[0].range_var, Some("My_Array'Range".to_string()));
    /// assert_eq!(nodes3[0].range_start, None);
    /// assert_eq!(nodes3[0].range_end, None);
    ///
    /// Ok(())
    /// # }
    /// ```
    pub fn extract_for_loops(code_text: &str) -> Result<Vec<NodeData>, ASTError> {
        // FIX 1: Use Reg::new and 's' (dotall) flag.
        let forloops_pattern = Reg::new(
            r#"(?i)(?P<Capturefor>\bfor\b)\s*(?P<index>.*?)\s*\bin\b\s*(?:(?P<loop_direction>.*?))?\s*(?P<primavar>[^\s]*)\s*(?:(?=\brange\b)\brange\b\s*(?P<frst>(?:.|\n)*?)\s+\.\.\s*(?P<scnd>(?:.|\n)*?)\s+\bloop\b|(?:(?=\.\.)\.\.\s*(?P<range_end>(?:.|\n)*?)\s*\bloop\b|\s*\bloop\b))"#
        ).map_err(|_| ASTError::RegexError)?;
        
        let mut nodes = Vec::new();

        for cap_res in forloops_pattern.captures_iter(code_text) {
            let mat = cap_res.map_err(|_| ASTError::RegexError)?;
            let full_match = mat.get(0).ok_or(ASTError::MatchItemMissing)?;
            let for_keyword = mat.name("Capturefor").ok_or(ASTError::InvalidCapture)?;

            // FIX 2: Correct line and index counting
            let start_line = code_text[..for_keyword.start()].matches('\n').count() + 1;
            let start_index = for_keyword.start();

            // FIX 3: Replicate Python's complex conditional logic
            let iterator = mat.name("index").unwrap().as_str().to_string();
            let direction = mat.name("loop_direction").map_or("to".to_string(), |m| m.as_str().to_string());

            // Get all optional capture groups
            let frst = mat.name("frst");
            let scnd = mat.name("scnd");
            let range_end_group = mat.name("range_end");
            let primavar = mat.name("primavar");

            // Python: range_start = match.group("primavar") if (match.group("range_end") and not match.group("frst")) else (match.group("frst") if (match.group("frst") and not match.group("range_end")) else None)
            let range_start = if range_end_group.is_some() && frst.is_none() {
                primavar.map(|m| m.as_str().trim().to_string())
            } else if frst.is_some() { // `range_end_group` will be None here due to regex `|`
                frst.map(|m| m.as_str().trim().to_string())
            } else {
                None
            };

            // Python: range_end = match.group("range_end") if (match.group("range_end") and not match.group("scnd")) else (match.group("scnd") if (match.group("scnd") and not match.group("range_end")) else None)
            let range_end = if range_end_group.is_some() && scnd.is_none() {
                range_end_group.map(|m| m.as_str().trim().to_string())
            } else if scnd.is_some() { // `range_end_group` will be None here
                scnd.map(|m| m.as_str().trim().to_string())
            } else {
                None
            };
            
            // Python: rang = match.group("primavar") if not (match.group("range_end") or match.group("frst")) else None
            let range_var = if range_end_group.is_none() && frst.is_none() {
                primavar.map(|m| m.as_str().trim().to_string())
            } else {
                None
            };

            // Python: iterator_type = match.group("primavar") if match.group("frst") else None
            let iterator_type = if frst.is_some() {
                primavar.map(|m| m.as_str().trim().to_string())
            } else {
                None
            };

            let mut node = NodeData::new(
                "ForLoop".to_string(), 
                "ForLoop".to_string(), 
                Some(start_line), 
                Some(start_index), 
                false
            );
            
            node.iterator = Some(iterator);
            node.range_start = range_start;
            node.range_end = range_end;
            node.direction = Some(direction);
            node.iterator_type = iterator_type;
            node.range_var = range_var;
            
            // Set body_start to the end of the full match (after "loop")
            node.body_start = Some(full_match.end());
            
            nodes.push(node);
        }

        Ok(nodes)
    }


    /// Extracts all statement nodes (if/elsif/else, case) from the source code.
    ///
    /// This is a convenience wrapper that calls:
    /// - `extract_if_statements()`
    /// - `extract_case_statements()`
    /// - `extract_elsif_statements()`
    /// - `extract_else_statements()`
    ///
    /// # Parameters
    /// - `code_text`: The cleaned source code
    ///
    /// # Returns
    /// A vector of all statement nodes found in the code.
    pub fn extract_statement_nodes(code_text: &str) -> Result<Vec<NodeData>,ASTError> {
        let mut new_nodes_data: Vec<NodeData> = Vec::new();
        new_nodes_data.extend(AST::extract_if_statements(code_text)?);
        new_nodes_data.extend(AST::extract_case_statements(code_text)?);

        new_nodes_data.extend(AST::extract_elsif_statements(code_text)?);
        new_nodes_data.extend(AST::extract_else_statements(code_text)?);    

        Ok(new_nodes_data) // Just return the new list
    }

    /// Extracts `if` statements from Ada source code.
    ///
    /// Parses the provided source code to identify `if` statements, capturing their conditions,
    /// start position, and other metadata.
    ///
    /// # Examples
    /// ```
    /// use ADA_Standards::{NodeData, ASTError, AST, Expression};
    /// # fn main() -> Result<(), ASTError> {
    /// let code = r#"
    /// procedure Main is
    ///   begin
    ///     if X > 10 and then Y < 5 then -- an if statement
    ///       null;
    ///     end if;
    ///   end Main;
    /// "#;
    /// let c_code = AST::clean_code(code);
    /// let nodes = AST::extract_if_statements(&c_code)?;
    ///
    /// // 1. Check that the 'if' was found
    /// assert_eq!(nodes.len(), 1);
    /// let if_node = &nodes[0];
    /// assert_eq!(if_node.name, "IfStatement");
    /// assert_eq!(if_node.start_line, Some(4));
    ///
    /// // 2. Check the condition
    /// let conds = if_node.conditions.as_ref().ok_or(ASTError::NoMatchFound)?;
    /// assert!(matches!(conds.albero.as_deref(), Some(Expression::Binary(_))));
    ///
    /// // 3. Check the condition string
    /// if let Some(Expression::Binary(bin_expr)) = conds.albero.as_ref().map(|b| b.as_ref()) {
    ///     assert_eq!(bin_expr.condstring.trim(), "X > 10 and then Y < 5");
    /// } else {
    ///     panic!("Condition was not parsed as a binary expression");
    /// }
    ///
    /// // 4. Check body_start
    /// let body_start_index = c_code.find("null;").unwrap();
    /// assert!(if_node.body_start.is_some());
    /// assert!(if_node.body_start.unwrap() < body_start_index);
    /// Ok(())
    /// # }
    /// ```
    pub fn extract_if_statements(code_text: &str) -> Result<Vec<NodeData>, ASTError> {
        // FIX: Removed the "skip-fail" prefix.
        let if_pattern = Reg::new(
            r"(?ims)^\s*(?P<ifstat>\bif\b)(?P<Condition>.*?)(?<!\band\b\s)then"
        ).map_err(|_| ASTError::RegexError)?;
        
        let mut nodes = Vec::new();

        for mat in if_pattern.captures_iter(code_text) {
            let captures = mat.map_err(|_| ASTError::RegexError)?; 
            // FIX: No longer need to check if we matched the keyword
            let full_match = captures.get(0).ok_or(ASTError::MatchItemMissing)?;
            let if_keyword = captures.name("ifstat").ok_or(ASTError::InvalidCapture)?;
            // ... (rest of function is identical) ...
            let start_line = code_text[..if_keyword.start()].matches('\n').count() + 1;
            let start_index = if_keyword.start();
            let condition_str = captures.name("Condition").unwrap().as_str().to_string();
            let conditions = AST::parse_condition_expression(&condition_str);
            let mut node = NodeData::new(
                "IfStatement".to_string(),
                "IfStatement".to_string(),
                Some(start_line),
                Some(start_index),
                false,
            );
            node.conditions = Some(conditions);
            node.body_start = Some(full_match.end()); 
            nodes.push(node);
        }
        Ok(nodes)
    }

    /// Extracts case statements from Ada source code.
    ///
    /// Identifies `case` statements using a regex pattern, capturing their switch expression and
    /// body start position. This regex uses the "skip-fail" technique to explicitly
    /// ignore strings and comments, ensuring it only matches valid `case` keywords.
    ///
    /// # Examples
    /// ```
    /// use ADA_Standards::{NodeData, ASTError, AST};
    /// # fn main() -> Result<(), ASTError> {
    /// // Use a standard string literal to ensure newlines are precise.
    /// let code = "\n  procedure Main is\n    Str : String := \"this case is in a string\";\n  begin\n    case My_Var is -- this is the real one\n      when 1 => null;\n      when others => null;\n    end case;\n  end Main;\n";
    ///
    /// let c_code = AST::clean_code(code);
    /// let nodes = AST::extract_case_statements(&c_code)?;
    ///
    /// assert_eq!(nodes.len(), 1); // This is the key assertion
    /// let case_node = &nodes[0];
    ///
    /// assert_eq!(case_node.name, "CaseStatement");
    /// assert_eq!(case_node.node_type, "CaseStatement");
    ///
    /// // With clean_code fixed, line 5 is now correct
    /// assert_eq!(case_node.start_line, Some(5));
    /// assert_eq!(case_node.switch_expression, Some("My_Var".to_string()));
    /// Ok(())
    /// # }
    /// ```
    pub fn extract_case_statements(code_text: &str) -> Result<Vec<NodeData>, ASTError> {
        // FIX: Removed the "skip-fail" prefix
        let case_pattern = Reg::new(
            r#"(?ims)(?<!end\s)(?P<Casestmnt>\bcase\b)\s*(?P<var>.*?)\s*\bis\b"#
        ).map_err(|_| ASTError::RegexError)?;
        
        let mut nodes = Vec::new();
        for mat in case_pattern.captures_iter(code_text) {
            let captures = mat.map_err(|_| ASTError::RegexError)?;

            // FIX: No longer need to check if we matched the keyword
            let case_keyword = captures.name("Casestmnt").ok_or(ASTError::InvalidCapture)?;
            let full_match = captures.get(0).ok_or(ASTError::MatchItemMissing)?;
            // ... (rest of function is identical) ...
            let start_line = code_text[..case_keyword.start()].matches('\n').count() + 1;
            let start_index = case_keyword.start();
            let body_start = full_match.end();
            let switch_expression = captures.name("var").unwrap().as_str().trim().to_string();
            let mut node = NodeData::new(
                "CaseStatement".to_string(),
                "CaseStatement".to_string(),
                Some(start_line),
                Some(start_index),
                false,
            );
            node.switch_expression = Some(switch_expression);
            node.body_start = Some(body_start);
            nodes.push(node);
        }
        Ok(nodes)
    }

    /// Populates the `cases` field for all `CaseStatement` nodes in the AST.
    ///
    /// This method traverses the AST, identifies nodes with `node_type` "CaseStatement", and extracts
    /// the list of case alternatives (e.g., "when X =>") from their body text, defined by `body_start`
    /// and `end_index`.
    ///
    /// # Examples
    /// ```
    /// use ADA_Standards::{NodeData, ASTError, AST};
    /// # fn main() -> Result<(), ASTError> {
    /// let code = r#"
    ///     case My_Var is
    ///       when 1 => null;
    ///       when 2 | 3 => null;
    ///       when others => null;
    ///     end case;
    /// "#;
    /// let c_code = AST::clean_code(code);
    ///
    /// // 1. Extract and build AST
    /// let nodes = AST::extract_all_nodes(&c_code)?;
    /// let mut ast = AST::new(nodes);
    /// ast.build(&c_code)?;
    ///
    /// // 2. Run populate_cases
    /// ast.populate_cases(&c_code)?;
    ///
    /// // 3. Check the node
    /// let case_node_id = ast.root_id().children(&ast.arena).next().unwrap();
    /// let case_node = ast.arena().get(case_node_id).unwrap().get();
    ///
    /// assert_eq!(case_node.node_type, "CaseStatement");
    /// let cases = case_node.cases.as_ref().unwrap();
    /// assert_eq!(cases.len(), 3);
    /// assert_eq!(cases[0], "1");
    /// assert_eq!(cases[1], "2 | 3");
    /// assert_eq!(cases[2], "others");
    /// Ok(())
    /// # }
    /// ```
    pub fn populate_cases(&mut self, code_text: &str) -> Result<(), ASTError> {
        // Compile the regex *once* outside the loop
        let re_when = Reg::new(
            r#"(?ims)^(?P<spaces> *)\bwhen\b\s*(?P<caso>.*?)\s*=>\s*(?:(?=\bcase\b)(?P<doublecase>\bcase\b)[\s\S]*?\bis\b|)"#
        ).map_err(|_| ASTError::RegexError)?;

        // First, collect all the nodes that need updating
        let updates: Vec<_> = self.arena.iter()
            .filter_map(|nodo| {
                let node_id = self.arena.get_node_id(&nodo)?;
                let node = self.arena.get(node_id)?;
                if node.get().node_type == "CaseStatement" {
                    if let (Some(body_start), Some(end_index)) = (node.get().body_start, node.get().end_index) {
                        if body_start >= end_index { return None; }
                        let body_text = &code_text[body_start..end_index];
                        
                        // Call the new, corrected helper function
                        let cases = AST::extract_cases_from_body(body_text, &re_when);
                        Some((node_id, cases))
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect();

        // Then, apply all updates
        for (node_id, cases) in updates {
            let mut node_data = self.arena[node_id].get().clone();
            node_data.cases = Some(cases);
            *self.arena[node_id].get_mut() = node_data;
        }
        Ok(())
    }


    /// Extracts case alternatives from the body text of a case statement.
    ///
    /// Parses the body text to identify "when" clauses (e.g., "when X =>") and returns their conditions
    /// as a vector of strings. This helper function for `populate_cases` replicates
    /// the complex Python logic for ignoring nested `case` statements.
    ///
    /// # Parameters
    /// - `body_text`: The body text of the case statement (from `body_start` to `end_index`).
    /// - `re_when`: The compiled fancy-regex for finding "when" clauses.
    ///
    /// # Returns
    /// A vector of case conditions (e.g., ["1", "2", "others"]).
    pub fn extract_cases_from_body(body_text: &str, re_when: &Reg) -> Vec<String> {
        let mut cases = Vec::new();
        let mut nm_spaces = 0; // 0 means not tracking indentation yet

        for cap_res in re_when.captures_iter(body_text) {
            if let Ok(caso) = cap_res {
                let spaces = caso.name("spaces").unwrap().as_str();
                
                // Python logic: if we find a "doublecase" (nested case),
                // start tracking indentation.
                if caso.name("doublecase").is_some() {
                    nm_spaces = spaces.len();
                }

                // Python logic: If we are tracking indentation...
                if nm_spaces > 0 {
                    // ...only add cases that match the *exact* indentation.
                    if nm_spaces == spaces.len() {
                        let choice = caso.name("caso").unwrap().as_str().trim().to_string();
                        cases.push(choice);
                    }
                } else {
                    // ...otherwise, add all cases we find.
                    let choice = caso.name("caso").unwrap().as_str().trim().to_string();
                    cases.push(choice);
                }
            }
        }
        cases
    }

    /// Parses a raw parameter string into structured `ArgumentData`.
    ///
    /// This function handles the complex Ada parameter syntax including:
    /// - Multiple parameters with same type: `A, B, C : Integer`
    /// - Different modes: `in`, `out`, `in out`
    /// - Default values: `Count : Integer := 0`
    /// - Semicolon-separated groups
    ///
    /// # Parameters
    /// - `params_opt`: The parameter string from inside parentheses, or None
    ///
    /// # Returns
    /// A vector of `ArgumentData`, one per parameter name.
    /// Returns empty vector if `params_opt` is None or empty.
    ///
    /// # Parsing Rules
    /// 1. Split by semicolon to get groups
    /// 2. For each group, split at colon to separate names from type spec
    /// 3. Parse type spec to extract mode, type, and default value
    /// 4. Create one `ArgumentData` per comma-separated name
    /// # Examples
    /// ```
    /// use ADA_Standards::{AST, ArgumentData};
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let code = "Dato, Porta: out Integer; P : in My_Type; Flag : Boolean := True";
    ///
    /// // We pass the string as an Option to simulate a regex match
    /// let args = AST::parse_parameters(Some(code));
    ///
    /// assert_eq!(args.len(), 4);
    ///
    /// assert_eq!(args[0].name, "Dato");
    /// assert_eq!(args[0].mode, "out");
    /// assert_eq!(args[0].data_type, "Integer");
    /// assert_eq!(args[0].default_value, None);
    ///
    /// assert_eq!(args[1].name, "Porta");
    /// assert_eq!(args[1].mode, "out");
    ///
    /// assert_eq!(args[2].name, "P");
    /// assert_eq!(args[2].mode, "in");
    /// assert_eq!(args[2].data_type, "My_Type");
    ///
    /// assert_eq!(args[3].name, "Flag");
    /// assert_eq!(args[3].mode, "in"); // Default mode
    /// assert_eq!(args[3].data_type, "Boolean");
    /// assert_eq!(args[3].default_value, Some("True".to_string()));
    /// Ok(())
    /// # }
    /// ```
    pub fn parse_parameters(params_opt: Option<&str>) -> Vec<ArgumentData> {
        let params_str = match params_opt {
            Some(s) => s.trim(),
            None => return Vec::new(),
        };

        let mut all_args = Vec::new();
        if params_str.is_empty() {
            return all_args;
        }

        // 1. Split parameter groups by semicolon
        for group in params_str.split(';') {
            if group.trim().is_empty() {
                continue;
            }

            // 2. Split name(s) from type/mode spec
            let parts: Vec<&str> = group.splitn(2, ':').map(str::trim).collect();
            if parts.len() != 2 {
                // Malformed group, e.g., no colon. Skip it.
                // You could log a warning here.
                continue;
            }

            let names_part = parts[0];
            let type_part = parts[1];

            // 3. Parse the type_part: split default value " := "
            let (spec_part, default_value) = if let Some((spec, default)) = type_part.split_once(" := ") {
                (spec.trim(), Some(default.trim().to_string()))
            } else {
                (type_part.trim(), None)
            };

            // 4. Parse mode and data_type from the spec_part
            let (mode, data_type) = if let Some(stripped) = spec_part.strip_prefix("in out") {
                // Check 'in out' first
                ("in out", stripped.trim())
            } else if let Some(stripped) = spec_part.strip_prefix("out") {
                ("out", stripped.trim())
            } else if let Some(stripped) = spec_part.strip_prefix("in") {
                ("in", stripped.trim())
            } else {
                ("in", spec_part) // Default mode is "in"
            };

            let data_type_str = data_type.to_string();

            // 5. Create ArgumentData for each comma-separated name
            for name in names_part.split(',') {
                if name.trim().is_empty() {
                    continue;
                }
                all_args.push(ArgumentData {
                    name: name.trim().to_string(),
                    mode: mode.to_string(),
                    data_type: data_type_str.clone(),
                    default_value: default_value.clone(),
                });
            }
        }

        all_args
    }


    /// Extracts Ada `task` and `task body` declarations from source code.
    ///
    /// # Examples
    /// ```
    /// use ADA_Standards::{NodeData, ASTError, AST};
    /// # fn main() -> Result<(), ASTError> {
    /// let code = r#"
    ///   task MyTask is
    ///     entry MyEntry;
    ///   end MyTask;
    ///   
    ///   task body MyTask is
    ///   begin
    ///     null;
    ///   end MyTask;
    /// "#;
    /// let c_code = AST::clean_code(code);
    /// let nodes = AST::extract_tasks(&c_code)?;
    /// assert_eq!(nodes.len(), 2);
    ///
    /// assert_eq!(nodes[0].name, "MyTask");
    /// assert_eq!(nodes[0].node_type, "TaskNode");
    /// assert_eq!(nodes[0].is_body, Some(false)); // Spec
    /// assert_eq!(nodes[0].start_line, Some(2));
    ///
    /// assert_eq!(nodes[1].name, "MyTask");
    /// assert_eq!(nodes[1].node_type, "TaskNode");
    /// assert_eq!(nodes[1].is_body, Some(true)); // Body
    /// assert_eq!(nodes[1].start_line, Some(6));
    /// Ok(())
    /// # }
    /// ```
    pub fn extract_tasks(code_text: &str) -> Result<Vec<NodeData>, ASTError> {
        // This regex is adapted from extract_packages
        let task_pattern = Reg::new(
            r#"(?im)^(?!\s*--)\s*(?P<category>\btask\b)(?:\s+(?P<body>\bbody\b))?\s+(?P<name>\S+)"#
        ).map_err(|_| ASTError::RegexError)?;

        let mut nodes: Vec<NodeData> = Vec::new();

        for cap_res in task_pattern.captures_iter(code_text) {
            let mat = cap_res.map_err(|_| ASTError::RegexError)?;
            let full_match = mat.get(0).ok_or(ASTError::MatchItemMissing)?;
            let category_keyword = mat.name("category").ok_or(ASTError::InvalidCapture)?;
            
            let start_line = code_text[..category_keyword.start()].matches('\n').count() + 1;
            let start_index = category_keyword.start();
            let name = mat.name("name").unwrap().as_str().to_string();
            let is_body = mat.name("body").is_some();

            // Check for an immediate semicolon (for task type declarations, e.g., "task type T;")
            let search_text = &code_text[full_match.end()..];
            let end_search = Regex::new(r"^\s*;").unwrap().find(search_text);

            let mut node = NodeData::new(
                name, 
                "TaskNode".to_string(), 
                Some(start_line), 
                Some(start_index), 
                is_body
            );

            if let Some(end_match) = end_search {
                // This is a task type declaration (spec), which ends here.
                node.end_line = Some(start_line);
                node.end_index = Some(full_match.end() + end_match.end());
            }
            
            // For 'task is' or 'task body is', associate_end_lines_in_arena will find the 'end Name;'
            
            nodes.push(node);
        }
        
        // Tasks don't have the same complex nesting/sorting as packages, so we just return
        Ok(nodes)
    }

    /// Extracts Ada `entry` declarations (specs) and bodies from source code.
    ///
    /// # Examples
    /// ```
    /// use ADA_Standards::{NodeData, ASTError, AST, Expression};
    /// # fn main() -> Result<(), ASTError> {
    /// let code = r#"
    ///   task body MyTask is
    ///     entry MyEntry(P : Integer); -- Declaration
    ///
    ///     entry MyEntry(P : Integer) when P > 0 is -- Body
    ///     begin
    ///       null;
    ///     end MyEntry;
    ///   end MyTask;
    /// "#;
    /// let c_code = AST::clean_code(code);
    /// let nodes = AST::extract_entries(&c_code)?;
    /// assert_eq!(nodes.len(), 2);
    ///
    /// assert_eq!(nodes[0].name, "MyEntry");
    /// assert_eq!(nodes[0].node_type, "EntryNode");
    /// assert_eq!(nodes[0].is_body, Some(false)); // Spec
    /// assert_eq!(nodes[0].start_line, Some(3));
    ///
    /// assert_eq!(nodes[1].name, "MyEntry");
    /// assert_eq!(nodes[1].node_type, "EntryNode");
    /// assert_eq!(nodes[1].is_body, Some(true)); // Body
    /// assert_eq!(nodes[1].start_line, Some(5));
    ///
    /// // Check if condition was parsed
    /// let conds = nodes[1].conditions.as_ref().unwrap();
    /// assert!(matches!(conds.albero.as_deref(), Some(Expression::Binary(_))));
    /// Ok(())
    /// # }
    /// ```
    pub fn extract_entries(code_text: &str) -> Result<Vec<NodeData>, ASTError> {
        // This regex is adapted from extract_procedures_functions
        let entry_pattern = Reg::new(
            r"(?ims)^(?!\s*--)(?:^[^\S\n]*|(?<=\n))(?P<category>\bentry\b)\s+(?P<name>[^\s\(\;]*)(?:\s*\((?P<params>[\s\S]*?(?=\)))\))?(?:\s+when\s+(?P<condition>.*?))?(?=\s*(?:is|;))"
        ).map_err(|_| ASTError::RegexError)?;

        // This pattern finds the 'is' or ';' that determines if it's a body or spec
        let end_pattern = Regex::new(r"(?m)\s*(is|;)").map_err(|_| ASTError::RegexError)?;
        let mut nodes: Vec<NodeData> = Vec::new();

        for cap_res in entry_pattern.captures_iter(code_text) {
            let mat = cap_res.map_err(|_| ASTError::RegexError)?;
            let full_match = mat.get(0).ok_or(ASTError::MatchItemMissing)?;
            let category_keyword = mat.name("category").ok_or(ASTError::InvalidCapture)?;

            let start_line = code_text[..category_keyword.start()].matches('\n').count() + 1;
            let start_index = category_keyword.start();
            let name = mat.name("name").unwrap().as_str().to_string();

            let is_body = {
                let search_text = &code_text[full_match.end()..];
                if let Some(end_match) = end_pattern.find(search_text) {
                    // An "entry" is a body if it's followed by "is".
                    // It's a spec if it's followed by ";".
                    end_match.as_str().trim() == "is"
                } else {
                    false // No "is" or ";" found
                }
            };

            let mut node = NodeData::new(
                name,
                "EntryNode".to_string(),
                Some(start_line),
                Some(start_index),
                is_body,
            );

            node.arguments = Some(AST::parse_parameters(mat.name("params").map(|m| m.as_str())));

            // If a 'when' condition was found, parse it.
            if let Some(condition_match) = mat.name("condition") {
                let condition_str = condition_match.as_str().to_string();
                if !condition_str.is_empty() {
                    node.conditions = Some(AST::parse_condition_expression(&condition_str));
                }
            }
            
            // If it's a spec, find the trailing semicolon to set its end_index
            if !is_body {
                let search_text = &code_text[full_match.end()..];
                if let Some(end_match) = end_pattern.find(search_text) {
                    if end_match.as_str().trim() == ";" {
                        node.end_line = Some(start_line); // Specs are single-line
                        node.end_index = Some(full_match.end() + end_match.end());
                    }
                }
            }
            
            // If it's a body, associate_end_lines_in_arena will find the 'end Name;'
            
            nodes.push(node);
        }

        Ok(nodes)
    }

    /// Extracts variable declarations from Ada source code.
    ///
    /// This function finds variable declarations, including those with
    /// multiple identifiers, type information, and optional default values.
    /// Default values are parsed as `ConditionExpr`.
    ///
    /// # Examples
    /// ```
    /// use ADA_Standards::{NodeData, ASTError, AST, Expression};
    /// # fn main() -> Result<(), ASTError> {
    /// let code = r#"
    ///   procedure Main is
    ///     A, B : Integer;
    ///     My_Str : String := "Hello";
    ///     Is_Valid : Boolean := A > 10;
    ///   begin
    ///     null;
    ///   end Main;
    /// "#;
    /// let c_code = AST::clean_code(code);
    /// let nodes = AST::extract_variable_declarations(&c_code)?;
    ///
    /// assert_eq!(nodes.len(), 4);
    ///
    /// assert_eq!(nodes[0].name, "A");
    /// assert_eq!(nodes[0].node_type, "VariableDeclaration");
    /// assert_eq!(nodes[0].base_type, Some("Integer".to_string()));
    /// assert!(nodes[0].conditions.is_none());
    ///
    /// assert_eq!(nodes[1].name, "B");
    /// assert_eq!(nodes[1].base_type, Some("Integer".to_string()));
    ///
    /// assert_eq!(nodes[2].name, "My_Str");
    /// assert_eq!(nodes[2].base_type, Some("String".to_string()));
    /// let conds_str = nodes[2].conditions.as_ref().unwrap();
    /// assert!(matches!(conds_str.albero.as_deref(), Some(Expression::Literal(_))));
    ///
    /// assert_eq!(nodes[3].name, "Is_Valid");
    /// assert_eq!(nodes[3].base_type, Some("Boolean".to_string()));
    /// let conds_bool = nodes[3].conditions.as_ref().unwrap();
    /// assert!(matches!(conds_bool.albero.as_deref(), Some(Expression::Binary(_))));
    ///
    /// Ok(())
    /// # }
    /// ```
    pub fn extract_variable_declarations(code_text: &str) -> Result<Vec<NodeData>, ASTError> {
        // FIX: Removed the skip-fail logic (\"[^\"]*\"|--.*$|)
        // This regex now only matches the variable declaration itself.
        let var_pattern = Reg::new(
            r#"(?m)^[^\S\n]*(?P<names_list>[A-Za-z_][A-Za-z0-9_]*(?:[^\S\n]*,[^\S\n]*[A-Za-z_][A-Za-z0-9_]*)*)[^\S\n]*:[^\S\n]*(?P<data_type>[A-Za-z_][^\n:;]*?)(?:[^\S\n]*:=[^\S\n]*(?P<default_value>[^\n;]+?))?[^\S\n]*;"#
        ).map_err(|_| ASTError::RegexError)?;

        let mut nodes = Vec::new();

        for cap_res in var_pattern.captures_iter(code_text) {
            let captures = cap_res.map_err(|_| ASTError::RegexError)?;
            if let Some(names_list_match) = captures.name("names_list") {
                let _full_match = captures.get(0).ok_or(ASTError::MatchItemMissing)?;
            
                let data_type_str = captures.name("data_type")
                                        .map_or("", |m| m.as_str())
                                        .trim()
                                        .to_string();

                let conditions = captures.name("default_value")
                                    .map(|m| AST::parse_condition_expression(m.as_str()));
            
                let start_line = code_text[..names_list_match.start()].matches('\n').count() + 1;
                let end_line = code_text[.._full_match.end()].matches('\n').count() + 1;
                let end_index = _full_match.end();
            
                for name_str in names_list_match.as_str().split(',') {
                    let name = name_str.trim().to_string();
                    if name.is_empty() { continue; }

                    let start_index = names_list_match.start();

                    let mut node = NodeData::new(
                        name,
                        "VariableDeclaration".to_string(),
                        Some(start_line),
                        Some(start_index),
                        false,
                    );

                    node.end_line = Some(end_line);
                    node.end_index = Some(end_index);
                    node.base_type = Some(data_type_str.clone());
                    node.conditions = conditions.clone();

                    nodes.push(node);
                }
            }
        }
        Ok(nodes)
    }

    /// Parses a condition string into a structured `ConditionExpr`.
    ///
    /// This is the main entry point for the expression parser. It converts
    /// Ada conditional expressions into a tree structure that respects
    /// operator precedence and parenthesization.
    ///
    /// # Features
    /// - **Binary operators**: `and`, `or`, `and then`, `or else`, `xor`
    /// - **Comparison**: `<`, `>`, `<=`, `>=`, `=`, `/=`
    /// - **Membership**: `in`, `not in`
    /// - **Unary**: `not`
    /// - **Proper precedence**: `or else` > `and then` > comparisons
    /// - **Parentheses**: Respected for grouping
    ///
    /// # Parameters
    /// - `condition_str`: The condition expression as a string
    ///
    /// # Returns
    /// A `ConditionExpr` with:
    /// - `list`: Flat vector of all sub-expressions
    /// - `albero`: Root of the expression tree
    ///
    /// # Examples
    /// ```
    /// use ADA_Standards::{AST, Expression, Binaries};
    /// 
    /// let cond = AST::parse_condition_expression("X > 10 and Y < 20");
    /// 
    /// // Check the root is a binary AND
    /// if let Some(Expression::Binary(bin)) = cond.albero.as_ref().map(|b| b.as_ref()) {
    ///     assert_eq!(bin.op, Binaries::AND);
    /// }
    /// 
    /// // List contains all sub-expressions
    /// assert_eq!(cond.list.len(), 3); // X > 10, Y < 20, and the AND itself
    /// ```
    pub fn parse_condition_expression(condition_str: &str) -> ConditionExpr {
        let mut list = Vec::new();
        let root_expression = AST::supersplitter(condition_str.to_string(), &mut list); // Pass mut list

        ConditionExpr { list,albero: Some(Box::new(root_expression)), }
    }

    /// (Internal) Checks if a keyword is safe to check at this index.
    fn size_checker(keyword: &str, condstring: &str, index: usize) -> bool {
        keyword.len() + index < condstring.len()
    }

    /// (Internal) Checks if a keyword (e.g., "in", "and") is present at this index.
    fn keyword_checker(keyword: &str, condstring: &str, index: usize) -> bool {
        if condstring[index..].starts_with(keyword) {
            if index > 0 {
                if !(condstring.as_bytes()[index + keyword.len()] == b' ' || condstring.as_bytes()[index + keyword.len()] == b'\n') || !(condstring.as_bytes()[index - 1] == b' ' || condstring.as_bytes()[index - 1] == b'\n' || index == 0) {
                    return false;
                }
            } else {
                if !(condstring.as_bytes()[index + keyword.len()] == b' ' || condstring.as_bytes()[index + keyword.len()] == b'\n') {
                    return false;
                }
            }
            return true;
        }
        false
    }

    /// (Internal) Identifies the highest-priority operator at the current index.
    fn keyword_identifier(condstring: &str, index: usize) -> (&'static str, i32) {
        if AST::size_checker("<", condstring, index) {
            if condstring.as_bytes()[index] == b'<' && condstring.as_bytes()[index + 1] != b'=' {
                return ("<", 0);
            }
        }
        if AST::size_checker(">", condstring, index) {
            if condstring.as_bytes()[index] == b'>' && condstring.as_bytes()[index + 1] != b'=' {
                return (">", 0);
            }
        }
        if AST::size_checker("<=", condstring, index) {
            if condstring.as_bytes()[index] == b'<' && condstring.as_bytes()[index + 1] == b'=' {
                return ("<=", 0);
            }
        }
        if AST::size_checker(">=", condstring, index) {
            if condstring.as_bytes()[index] == b'>' && condstring.as_bytes()[index + 1] == b'=' {
                return (">=", 0);
            }
        }
        if AST::size_checker("/=", condstring, index) {
            if condstring.as_bytes()[index] == b'/' && condstring.as_bytes()[index + 1] == b'=' {
                return ("/=", 0);
            }
        }
        if condstring.as_bytes()[index] == b'=' && (index == 0 || (condstring.as_bytes()[index - 1] != b'/' && condstring.as_bytes()[index - 1] != b'<' && condstring.as_bytes()[index - 1] != b'>')) {
            return ("=", 0);
        }
        if AST::size_checker("in", condstring, index) {
            if AST::keyword_checker("in", condstring, index) {
                return ("in", 0);
            }
        }
        if AST::size_checker("not in", condstring, index) {
            if AST::keyword_checker("not in", condstring, index) {
                return ("not in", 0);
            }
        }
        if AST::size_checker("and then", condstring, index) {
            if AST::keyword_checker("and then", condstring, index) {
                return ("and then", 1);
            }
        }
        if AST::size_checker("and", condstring, index) {
            if AST::keyword_checker("and", condstring, index) {
                return ("and", 1);
            }
        }
        if AST::size_checker("or else", condstring, index) {
            if AST::keyword_checker("or else", condstring, index) {
                return ("or else", 1);
            }
        }
        if AST::size_checker("or", condstring, index) {
            if AST::keyword_checker("or", condstring, index) {
                return ("or", 1);
            }
        }
        if AST::size_checker("xor", condstring, index) {
            if AST::keyword_checker("xor", condstring, index) {
                return ("xor", 1);
            }
        }
        if AST::size_checker("not", condstring, index) {
            if AST::keyword_checker("not", condstring, index) {
                return ("not", 1);
            }
        }
        ("No_keyword", 2)
    }


    /// (Internal) The recursive step of the expression parser.
    ///
    /// Given an operator and its position, this function splits the expression
    /// and recursively calls `supersplitter` on the operands.
    ///
    /// # Parameters
    /// - `keyword_str`: The operator string (e.g., "and", ">", "not")
    /// - `condstring`: The full expression string
    /// - `index`: Position of the operator in the string
    /// - `lst`: Mutable reference to the flat list
    ///
    /// # Returns
    /// An `Expression` node (Unary, Binary, or Membership) with recursively parsed operands.
    pub fn recursive_function(keyword_str: &str, condstring: String, index: usize, lst: &mut Vec<Expression>) -> Expression {
        let keyword = keyword_str.to_string(); // Convert to String once
        if let Some(un_keyword) = match keyword.as_str() {
            "not" => Some(Unaries::NOT),
            _ => None,
        } {
            let split2 = condstring[index + keyword.len()..].trim().to_string();
            let operand_expr = AST::supersplitter(split2.clone(), lst); // Pass lst here
            let unary_expr = UnaryExpression {
                op: un_keyword,
                operand: Box::new(operand_expr.clone()),
                condstring: condstring.clone(),
            };
            lst.push(Expression::Unary(unary_expr.clone())); // Push Expression::Unary
            return Expression::Unary(unary_expr);

        } else if let Some(bin_keyword) = match keyword.as_str() {
            "and" => Some(Binaries::AND),
            "or" => Some(Binaries::OR),
            "and then" => Some(Binaries::AND_THEN),
            "or else" => Some(Binaries::OR_ELSE),
            "xor" => Some(Binaries::XOR),
            "<" => Some(Binaries::INFERIOR),
            ">" => Some(Binaries::SUPERIOR),
            "<=" => Some(Binaries::INFERIOR_OR_EQUAL),
            ">=" => Some(Binaries::SUPERIOR_OR_EQUAL),
            "=" => Some(Binaries::EQUAL),
            "/=" => Some(Binaries::UNEQUAL),
            _ => None,
        } {
            let split1 = condstring[..index].trim().to_string();
            let split2 = condstring[index + keyword.len()..].trim().to_string();

            let left_expr = AST::supersplitter(split1.clone(), lst); // Recursively parse left
            let right_expr = AST::supersplitter(split2.clone(), lst); // Recursively parse right
            let binary_expr = BinaryExpression {
                op: bin_keyword,
                left: Box::new(left_expr.clone()),
                right: Box::new(right_expr.clone()),
                condstring: condstring.clone(),
            };
            lst.push(Expression::Binary(binary_expr.clone())); // Push Expression::Binary
            return Expression::Binary(binary_expr);

        } else if let Some(mem_keyword) = match keyword.as_str() {
            "in" => Some(Memberships::IN),
            "not in" => Some(Memberships::NOT_IN),
            _ => None,
        } {
            let split1 = condstring[..index].trim().to_string();
            let split2 = condstring[index + keyword.len()..].trim().to_string();

            let left_expr = AST::supersplitter(split1.clone(), lst);
            let right_expr = AST::supersplitter(split2.clone(), lst);
            let membership_expr = MembershipExpression {
                op: mem_keyword,
                left: Box::new(left_expr.clone()),
                right: Box::new(right_expr.clone()),
                condstring: condstring.clone(),
            };
            lst.push(Expression::Membership(membership_expr.clone())); // Push Expression::Membership
            return Expression::Membership(membership_expr);
        }

        Expression::Literal(condstring) // Base case: return literal expression
    }


    /// (Internal) The main recursive expression parser.
    ///
    /// Implements a precedence-climbing parser that handles Ada's operator
    /// precedence and associativity rules. It works by:
    /// 1. Finding the lowest-precedence operator in the expression
    /// 2. Splitting the expression at that operator
    /// 3. Recursively parsing the left and right operands
    ///
    /// # Precedence (low to high)
    /// 1. `or else` (priority 1)
    /// 2. `and then` (priority 1)
    /// 3. `or`, `and`, `xor`, `not` (priority 1)
    /// 4. Comparisons and membership (priority 0)
    ///
    /// # Parameters
    /// - `condstring_in`: The expression string to parse
    /// - `lst`: Mutable reference to the flat list (for collecting sub-expressions)
    ///
    /// # Returns
    /// The root `Expression` of the parsed tree.
    pub fn supersplitter(condstring_in: String, lst: &mut Vec<Expression>) -> Expression {
        let mut number_of_open_parenthesis = 0;
        let mut number_of_closed_parenthesis = 0;
        let mut string_flag = 0;
        let mut saved_index = 0;
        let mut priority_flag = 0;
        let mut first_occurence = 0;
        let mut temp_keyword: &'static str = "No_keyword"; // Initialize with default

        let mut condstring = condstring_in.trim().to_string(); //trim here to avoid extra spaces

        if AST::is_expression_a_parenthesis(&condstring) {
            condstring = condstring[1..condstring.len() - 1].to_string();
        }

        for (index, char) in condstring.chars().enumerate() {
            let (updated_string_flag, updated_open_parenthesis, updated_closed_parenthesis) =
                AST::flag_setter(char, string_flag, number_of_open_parenthesis, number_of_closed_parenthesis);
            string_flag = updated_string_flag;
            number_of_open_parenthesis = updated_open_parenthesis;
            number_of_closed_parenthesis = updated_closed_parenthesis;

            if AST::flags_check(number_of_open_parenthesis, number_of_closed_parenthesis, string_flag) {
                let (keyword, prio_flag) = AST::keyword_identifier(&condstring, index);
                priority_flag = prio_flag;

                if priority_flag == 1 {
                    return AST::recursive_function(keyword, condstring, index, lst);
                } else if priority_flag == 0 && first_occurence == 0 {
                    temp_keyword = keyword;
                    first_occurence = 1;
                    saved_index = index;
                }
            }
        }

        if priority_flag == 0 || priority_flag == 2 {
            return AST::recursive_function(temp_keyword, condstring, saved_index, lst);
        }

        Expression::Literal(condstring) // Return literal if no split happened
    }


    /// (Internal) Tracks string literals and parenthesis depth for the parser.
    pub fn flag_setter( char: char, string_flag: i32, number_of_open_parenthesis: i32, number_of_closed_parenthesis: i32) -> (i32, i32, i32) {
        let mut mut_string_flag = string_flag;
        let mut mut_number_of_open_parenthesis = number_of_open_parenthesis;
        let mut mut_number_of_closed_parenthesis = number_of_closed_parenthesis;

        if char == '"' && string_flag == 0 {
            mut_string_flag = 1;
        } else if char == '"'  && string_flag == 1 {
            mut_string_flag = 0;
        }

        if char == '(' && string_flag == 0 {
            mut_number_of_open_parenthesis += 1;
        }
        if char == ')' && string_flag == 0 {
            mut_number_of_closed_parenthesis += 1;
        }
        (mut_string_flag, mut_number_of_open_parenthesis, mut_number_of_closed_parenthesis)
    }

    /// (Internal) Checks if the parser is at the top level (not inside parens or a string).
    pub fn flags_check(number_of_open_parenthesis: i32, number_of_closed_parenthesis: i32, string_flag: i32) -> bool {
        (number_of_open_parenthesis - number_of_closed_parenthesis == 0) && string_flag == 0
    }

    /// (Internal) Checks if parentheses are only on the outside of an expression.
    pub fn is_parenthesis_exterior(expression: &str) -> bool {
        let mut cnt_paren = 0;
        let mut cnt_prn = 0;
        for (idx, char) in expression.chars().enumerate() {
            if char == '(' && idx != 0 {
                cnt_paren += 1;
            }
            if char == ')' && idx != expression.len() - 1 {
                cnt_prn += 1;
                if cnt_prn > cnt_paren {
                    return false;
                }
            }
        }
        true
    }

    /// (Internal) Checks if an expression is fully wrapped in parentheses.
    pub fn is_expression_a_parenthesis(expression: &str) -> bool {
        if expression.len() < 2 {
            return false;
        }
        if expression.starts_with('(') && expression.ends_with(')') {
            if AST::is_parenthesis_exterior(expression) {
                return true;
            } else {
                return false;
            }
        }
        false
    }


    
    /// Cleans Ada source code by removing comments and replacing string contents.
    ///
    /// This is a critical preprocessing step that:
    /// 1. Converts tabs to spaces (4 spaces per tab)
    /// 2. Replaces string literal contents with spaces (preserving length and quotes)
    /// 3. Replaces comments with spaces (preserving length)
    ///
    /// By replacing content with spaces instead of removing it, we preserve:
    /// - Line numbers
    /// - Character indices
    /// - Column positions
    ///
    /// This ensures that all extracted node positions match the original source.
    ///
    /// # Why Clean?
    /// - Prevents regex patterns from matching inside strings or comments
    /// - Simplifies parsing logic
    /// - Maintains accurate position information
    ///
    /// # Parameters
    /// - `raw_code`: The original Ada source code
    ///
    /// # Returns
    /// A string with tabs normalized, string contents replaced, and comments replaced.
    ///
    /// # Examples
    /// ```
    /// use ADA_Standards::AST;
    /// 
    /// let code = r#"X : String := "end loop;"; -- comment"#;
    /// let cleaned = AST::clean_code(code);
    /// 
    /// assert!(cleaned.contains("String := \"         \""));
    /// assert!(!cleaned.contains("comment"));
    /// assert_eq!(code.len(), cleaned.len()); // Same length!
    /// ```
    pub fn clean_code(raw_code: &str) -> String {
        let space_to_tab_ratio = 4;
        // First, replace tabs with spaces
        let ada_code_content = raw_code.replace("\t", &" ".repeat(space_to_tab_ratio));

        lazy_static! {
            static ref CLEAN_CODE_REGEX: Regex = Regex::new(
                r#"(?m)(?P<string>\"[^\"]*\")|(?P<comment>--.*$)"#
            ).unwrap();
        }

        let cleaned_code = CLEAN_CODE_REGEX.replace_all(&ada_code_content, |caps: &regex::Captures| {
            
            // --- THIS IS THE FIX ---
            if let Some(string_match) = caps.name("string") {
                let full_str = string_match.as_str();
                if full_str.len() >= 2 {
                    // Keep the opening and closing quotes
                    // Replace the content (len - 2) with spaces
                    format!("\"{}\"", " ".repeat(full_str.len() - 2))
                } else {
                    // Should not happen (e.g., empty string ""), but as a fallback:
                    " ".repeat(full_str.len())
                }
            // --- END FIX ---

            } else if let Some(comment) = caps.name("comment") {
                // This part is correct
                " ".repeat(comment.as_str().len())
            } 
            else {
                caps.get(0).unwrap().as_str().to_string()
            }
        });

        // Return the modified string.
        cleaned_code.into_owned()
    }

    /// Extracts all known Ada constructs from the source code.
    ///
    /// This is the main entry point for the extraction phase. It runs all
    /// specialized extraction functions and returns a flat vector of all
    /// found nodes, ready to be passed to `AST::new()` and `build()`.
    ///
    /// # Extracted Constructs
    /// - Packages (specs and bodies)
    /// - Procedures and functions (specs, bodies, generics)
    /// - Types and subtypes
    /// - Declare blocks
    /// - Control flow (loops: simple, while, for)
    /// - Statements (if/elsif/else, case)
    /// - Variables
    /// - Tasks and entries
    ///
    /// # Parameters
    /// - `code_text`: The **cleaned** source code (use `clean_code()` first)
    ///
    /// # Returns
    /// - `Ok(Vec<NodeData>)`: All extracted nodes in no particular order
    /// - `Err(ASTError)`: If any extraction function fails
    ///
    /// # Examples
    /// ```
    /// use ADA_Standards::AST;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let code = r#"
    ///   package My_Pkg is
    ///     procedure Do_Work;
    ///   end My_Pkg;
    /// "#;
    /// let cleaned = AST::clean_code(code);
    /// let nodes = AST::extract_all_nodes(&cleaned)?;
    /// 
    /// assert!(nodes.iter().any(|n| n.node_type == "PackageNode"));
    /// assert!(nodes.iter().any(|n| n.node_type == "ProcedureNode"));
    /// # Ok(())
    /// # }
    /// ```
    pub fn extract_all_nodes(code_text: &str) -> Result<Vec<NodeData>,ASTError> {
        let mut nodes: Vec<NodeData> = Vec::new();
        nodes.extend(AST::extract_packages(code_text)?);
        
        // No more temp vecs or mutable borrows needed
        nodes.extend(AST::extract_procedures_functions(code_text)?);
        nodes.extend(AST::extract_type_declarations(code_text)?);
        nodes.extend(AST::extract_declare_blocks(code_text)?);
        
        // Just call the updated functions and extend
        nodes.extend(AST::extract_control_flow_nodes(code_text)?);
        nodes.extend(AST::extract_statement_nodes(code_text)?);

        nodes.extend(AST::extract_variable_declarations(code_text)?);

        nodes.extend(AST::extract_tasks(code_text)?); 
        nodes.extend(AST::extract_entries(code_text)?);

        Ok(nodes)
    }

    /// (Internal) Helper to recursively print an expression tree for debugging.
    ///
    /// Prints the tree structure with indentation to show nesting levels.
    /// Useful for visualizing how conditions were parsed.
    ///
    /// # Parameters
    /// - `nodo`: The expression node to print
    /// - `level`: Current indentation level
    /// - `prefix`: Prefix string (e.g., "L---" for left child, "R---" for right)
    ///
    /// # Examples
    /// ```no_run
    /// use ADA_Standards::AST;
    /// 
    /// let cond = AST::parse_condition_expression("X > 10 and Y < 20");
    /// if let Some(root) = &cond.albero {
    ///     AST::leggitree(root, 0, "Root: ");
    /// }
    /// ```
    pub fn leggitree(nodo: &Expression, level: u32, prefix: &str) {
        match nodo {
            Expression::Binary(binary_expr) => {
                println!("{} {} {}", " ".repeat((level * 4) as usize), prefix, binary_expr.condstring);
                AST::leggitree(&*binary_expr.left, level + 1, "L--- ");
                AST::leggitree(&*binary_expr.right, level + 1, "R--- ");
            }
            Expression::Membership(membership_expr) => {
                println!("{} {} {}", " ".repeat((level * 4) as usize), prefix, membership_expr.condstring);
                AST::leggitree(&*membership_expr.left, level + 1, "L--- ");
                AST::leggitree(&*membership_expr.right, level + 1, "R--- ");
            }
            Expression::Unary(unary_expr) => {
                println!("{} {} {}", " ".repeat((level * 4) as usize), prefix, unary_expr.condstring);
                AST::leggitree(&*unary_expr.operand, level + 1, "U--- ");
            }
            Expression::Literal(literal_expr) => {
                println!("{} {} Literal: {}", " ".repeat((level * 4) as usize), prefix, literal_expr);
            }
            Expression::Condition(condition_expr) => {
                println!("{} {} Condition Expression (albero):", " ".repeat((level * 4) as usize), prefix);
                if let Some(albero) = &condition_expr.albero {
                    AST::leggitree(&*albero, level + 1, "A--- "); // Prefix for albero in ConditionExpr
                } else {
                    println!("{}     No albero in ConditionExpr", " ".repeat(((level + 1) * 4) as usize));
                }
            }
        }
    }


    /// Finds the first node in the arena matching a given name and type.
    ///
    /// This is a convenience method for querying the AST. It performs a depth-first
    /// search starting from the root to find the first node that matches both criteria.
    ///
    /// # Parameters
    /// - `name`: The node name to search for (exact match)
    /// - `node_type`: The node type to search for (exact match)
    ///
    /// # Returns
    /// - `Some(NodeId)`: The ID of the first matching node
    /// - `None`: If no matching node is found
    ///
    /// # Examples
    /// ```
    /// use ADA_Standards::AST;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let code = "procedure MyProc is\nbegin\n  null;\nend MyProc;";
    /// let cleaned = AST::clean_code(code);
    /// let nodes = AST::extract_all_nodes(&cleaned)?;
    /// let mut ast = AST::new(nodes);
    /// ast.build(&cleaned)?;
    /// 
    /// if let Some(proc_id) = ast.find_node_by_name_and_type("MyProc", "ProcedureNode") {
    ///     let proc = ast.arena().get(proc_id).unwrap().get();
    ///     assert_eq!(proc.name, "MyProc");
    ///     assert_eq!(proc.is_body, Some(true));
    /// }
    /// # Ok(())
    /// # }
    /// ```
    pub fn find_node_by_name_and_type(&self, name: &str, node_type: &str) -> Option<NodeId> {
        self.root_id.descendants(&self.arena).find(|&node_id| {
            let node = self.arena.get(node_id).unwrap().get();
            node.name == name && node.node_type == node_type
        })
    }

    /// Extracts `elsif` branches from Ada source code.
    ///
    /// Parses `elsif ... then` statements, capturing their conditions and start position.
    /// These nodes are intended to be parented to a preceding `IfStatement` node
    /// during the AST build process.
    ///
    /// # Examples
    /// ```
    /// use ADA_Standards::{NodeData, ASTError, AST, Expression};
    /// # fn main() -> Result<(), ASTError> {
    /// let code = r#"
    ///   if X = 1 then
    ///     null;
    ///   elsif X = 2 then -- this is the one
    ///     null;
    ///   end if;
    /// "#;
    /// let c_code = AST::clean_code(code);
    /// let nodes = AST::extract_elsif_statements(&c_code)?;
    ///
    /// assert_eq!(nodes.len(), 1);
    /// let elsif_node = &nodes[0];
    ///
    /// assert_eq!(elsif_node.name, "ElsifStatement");
    /// assert_eq!(elsif_node.start_line, Some(4));
    ///
    /// // Check the condition
    /// let conds = elsif_node.conditions.as_ref().ok_or(ASTError::NoMatchFound)?;
    /// if let Some(Expression::Binary(bin_expr)) = conds.albero.as_ref().map(|b| b.as_ref()) {
    ///     assert_eq!(bin_expr.condstring.trim(), "X = 2");
    /// } else {
    ///     panic!("Condition was not parsed as a binary expression");
    /// }
    /// Ok(())
    /// # }
    /// ```
    pub fn extract_elsif_statements(code_text: &str) -> Result<Vec<NodeData>, ASTError> {
        // FIX: Removed the "skip-fail" prefix
        let elsif_pattern = Reg::new(
            r#"(?ims)^\s*(?P<elsifstat>\belsif\b)(?P<Condition>.*?)(?<!\band\b\s)then"#
        ).map_err(|_| ASTError::RegexError)?;
        
        let mut nodes = Vec::new();
        for mat in elsif_pattern.captures_iter(code_text) {
            let captures = mat.map_err(|_| ASTError::RegexError)?;
            
            // FIX: No longer need to check if we matched the keyword
            let elsif_keyword = captures.name("elsifstat").ok_or(ASTError::InvalidCapture)?;
            let full_match = captures.get(0).ok_or(ASTError::MatchItemMissing)?;
            // ... (rest of function is identical) ...
            let start_line = code_text[..elsif_keyword.start()].matches('\n').count() + 1;
            let start_index = elsif_keyword.start();
            let condition_str = captures.name("Condition").unwrap().as_str().to_string();
            let conditions = AST::parse_condition_expression(&condition_str);
            let mut node = NodeData::new(
                "ElsifStatement".to_string(),
                "ElsifStatement".to_string(),
                Some(start_line),
                Some(start_index),
                false,
            );
            node.conditions = Some(conditions);
            node.body_start = Some(full_match.end());
            node.end_line = Some(code_text[..full_match.end()].matches('\n').count() + 1);
            node.end_index = Some(full_match.end());
            nodes.push(node);
        }
        Ok(nodes)
    }

    /// Extracts `else` branches from Ada source code.
    ///
    /// Parses `else` statements and records their start position.
    /// These nodes are intended to be parented to a preceding `IfStatement`
    /// or `CaseStatement` during the AST build process.
    ///
    /// # Examples
    /// ```
    /// use ADA_Standards::{NodeData, ASTError, AST};
    /// # fn main() -> Result<(), ASTError> {
    /// let code = r#"
    ///   if X = 1 then
    ///     null;
    ///   else -- this is the one
    ///     null;
    ///   end if;
    /// "#;
    /// let c_code = AST::clean_code(code);
    /// let nodes = AST::extract_else_statements(&c_code)?;
    ///
    /// assert_eq!(nodes.len(), 1);
    /// let else_node = &nodes[0];
    ///
    /// assert_eq!(else_node.name, "ElseStatement");
    /// assert_eq!(else_node.start_line, Some(4));
    /// assert!(else_node.conditions.is_none());
    /// Ok(())
    /// # }
    /// ```
    pub fn extract_else_statements(code_text: &str) -> Result<Vec<NodeData>, ASTError> {
        // FIX: Removed the "skip-fail" prefix
        let else_pattern = Reg::new(
            r#"(?ims)^\s*(?P<elsestat>\belse\b)"#
        ).map_err(|_| ASTError::RegexError)?;
        
        let mut nodes = Vec::new();
        for mat in else_pattern.captures_iter(code_text) {
            let captures = mat.map_err(|_| ASTError::RegexError)?;
            
            // FIX: No longer need to check if we matched the keyword
            let else_keyword = captures.name("elsestat").ok_or(ASTError::InvalidCapture)?;
            let full_match = captures.get(0).ok_or(ASTError::MatchItemMissing)?;
            // ... (rest of function is identical) ...
            let start_line = code_text[..else_keyword.start()].matches('\n').count() + 1;
            let start_index = else_keyword.start();
            let mut node = NodeData::new(
                "ElseStatement".to_string(),
                "ElseStatement".to_string(),
                Some(start_line),
                Some(start_index),
                false,
            );
            node.body_start = Some(full_match.end()); 
            node.end_line = Some(start_line);
            node.end_index = Some(full_match.end());
            nodes.push(node);
        }
        Ok(nodes)
    }

}







