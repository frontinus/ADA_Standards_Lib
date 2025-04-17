//! # ADA_Standards
//!
//! [![crates.io](https://img.shields.io/crates/v/ADA_Standards.svg)](https://crates.io/crates/ADA_Standards)
//! [![docs.rs](https://docs.rs/ADA_Standards/badge.svg)](https://docs.rs/ADA_Standards)
//!
//! A library for parsing and analyzing Ada source code to enforce coding standards and identify potential issues.
//!
//! This library provides a way to programmatically inspect the structure and content of Ada files,
//! allowing for the creation of custom rules to verify specific coding conventions.
//!
//! ## Overview
//!
//! The `ADA_Standards` library uses regular expressions to perform a lightweight parsing of Ada code
//! and builds an Abstract Syntax Tree (AST) represented by the `indextree` crate. This AST can then
//! be traversed to analyze the code based on user-defined rules.
//!
//! ## Modules
//!
//! * `text_format`: Provides constants for ANSI escape codes to format output in the console.
//!
//! ## Enums
//!
//! The library defines several enums to represent different elements of Ada expressions:
//!
//! * `Unaries`: Represents unary operators like `not`.
//! * `Memberships`: Represents membership operators like `in` and `not in`.
//! * `Binaries`: Represents binary operators like `and`, `or`, `and then`, `or else`, etc.
//!
//! ## Structs
//!
//! Key data structures used by the library:
//!
//! * `NodeData`: Stores information about a node in the AST, including its name, type, line numbers, and specific data related to the Ada construct it represents (e.g., conditions for an `if` statement, arguments for a procedure).
//! * `UnaryExpression`, `BinaryExpression`, `MembershipExpression`, `ConditionExpr`: Represent different types of expressions in Ada code.
//! * `ArgumentData`: Stores information about procedure or function arguments.
//! * `ReturnKeywordData`: Stores information about the return type of a function.
//! * `AST`: Represents the Abstract Syntax Tree of the Ada code. It provides methods to build the tree from a list of `NodeData` and traverse it.
//!
//! ## Usage
//!
//! To use the `ADA_Standards` library, you would typically:
//!
//! 1.  Read the content of an Ada file as a string.
//! 2.  Use the library's functions (like `extract_packages`, `extract_procedures_functions`, etc.) to parse the code and create a `Vec<NodeData>`.
//! 3.  Create an `AST` instance from the `Vec<NodeData>`.
//! 4.  Define your own analysis rules as functions that take an `&AST`
//! 7.  Process the list of nodes and various informations in the AST to try to check if your coding standards are respected
//!
//! 
///!

use indextree::{Arena, Node, NodeId};
use regex::Regex;

use lazy_static::lazy_static;

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
/// # Variants
/// - `RegexError(String)`: Failure to compile a regex pattern.
/// - `NodeNotInArena(String)`: Attempt to access an invalid node ID.
/// - `InvalidNodeData(String)`: Node data is missing required fields (e.g., `body_start`).
#[derive(Debug)]
pub enum ASTError {
    MatchItemMissing,
    InvalidCapture,
    TreeBuildError,
    NoMatchFound,
    NodeIdMissing(String),
    StartNodeNotFound,
    NodeNotInArena(String),
    RegexError,
    // Add other variants as needed
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



#[derive(Debug, Clone, PartialEq)]
pub enum Unaries {
    NOT,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Memberships {
    NOT_IN,
    IN,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Binaries {
    AND,
    OR,
    AND_THEN,
    OR_ELSE,
    XOR,
    INFERIOR,
    SUPERIOR,
    INFERIOR_OR_EQUAL,
    SUPERIOR_OR_EQUAL,
    EQUAL,
    UNEQUAL,
}

// Define NodeData struct to hold node information, similar to BaseNode in Python
#[derive(Debug,Clone)] // For printing and debugging, remove later if not needed
pub enum Expression {
    Unary(UnaryExpression),
    Binary(BinaryExpression),
    Membership(MembershipExpression),
    Condition(ConditionExpr),
    Literal(String), // Added to represent basic expressions as literals
}

#[derive(Debug, Clone)]
pub struct UnaryExpression {
    pub op: Unaries, // Changed to use Unaries enum
    pub operand: Box<Expression>, // Changed to Box<Expression>
    pub condstring: String,
}

#[derive(Debug, Clone)]
pub struct BinaryExpression {
    pub op: Binaries, // Changed to use Binaries enum
    pub left: Box<Expression>, // Changed to Box<Expression>
    pub right: Box<Expression>, // Changed to Box<Expression>
    pub condstring: String,
}

#[derive(Debug, Clone)]
pub struct MembershipExpression {
    pub op: Memberships, // Changed to use Memberships enum
    pub left: Box<Expression>, // Changed to Box<Expression>
    pub right: Box<Expression>, // Changed to Box<Expression>
    pub condstring: String,
}

#[derive(Debug, Clone)]
pub struct ConditionExpr {
    pub list: Vec<Expression>, // Changed to Expression enum
    pub albero : Option<Box<Expression>>,
}

#[derive(Debug, Clone)]
pub struct ArgumentData {
    pub name: String,
    pub mode: String,          // "in", "out", "in out"
    pub data_type: String,
    pub default_value: Option<String>,
}

#[derive(Debug, Clone)]
pub struct ReturnKeywordData {
    pub data_type: Option<String>,
}

#[derive(Debug, Clone)]
pub struct EndStatement {
    word: String, // What follows "end", e.g., "loop", "if", or a name like "Proc"
    index: usize,
    line: usize,
}

const UNNAMED_END_KEYWORDS: [&str; 4] = ["loop", "if", "case", "record"];

enum ParseEvent {
    Start { node: NodeData, index: usize, original_index: usize },
    End { word: String, index: usize, line: usize },
}

/// Creates a new `NodeData` instance with specified attributes.
///
/// Initializes a node with the given name, type, start position, and body status, setting
/// other fields to `None`. This constructor is used during node extraction (e.g., for packages,
/// procedures, or case statements) to capture essential metadata before further processing.
///
/// # Parameters
/// - `name`: The identifier or name of the Ada construct (e.g., "MyPackage").
/// - `node_type`: The type of construct (e.g., "PackageNode", "CaseStatement").
/// - `start_line`: The line number where the construct begins, if known.
/// - `start_index`: The character index where the construct begins, if known.
/// - `is_body`: Whether the construct is a body (e.g., `package body`) or specification.
///
/// # Returns
/// A `NodeData` instance with initialized fields and others set to `None`.
///
/// # Examples
/// ```
///
/// use ADA_Standards::NodeData;
/// let node = NodeData::new(
///     "MyProcedure".to_string(),
///     "ProcedureNode".to_string(),
///     Some(10),
///     Some(15),
///     false,
/// );
/// assert_eq!(node.name, "MyProcedure");
/// assert_eq!(node.is_body, Some(false));
/// assert_eq!(node.start_line, Some(10));
/// ```
/// 
#[derive(Debug,Clone)] 
pub struct NodeData {
    pub name: String,
    pub node_type: String, // e.g., "BaseNode", "WhileLoop", etc.
    pub start_line: Option<usize>, // Use Option to represent potentially missing values
    pub end_line: Option<usize>,
    pub start_index: Option<usize>,
    pub end_index: Option<usize>, // Use Option to represent potentially missing values
    pub column: Option<usize>,
    // Specific Node Data:
    // PackageNode, ProcedureNode, FunctionNode
    pub is_body: Option<bool>,
    pub pkg_name: Option<String>,
    pub category_type: Option<String>, // For ProcedureNode/FunctionNode: type, for TypeDeclaration: category
    pub arguments: Option<Vec<ArgumentData>>,
    pub return_type: Option<ReturnKeywordData>,
    pub type_kind: Option<String>,   // For TypeDeclaration
    pub base_type: Option<String>,   // For TypeDeclaration (derived types)
    pub tuple_values: Option<Vec<String>>, // For TypeDeclaration (tuple types)
    // ControlFlowNode, WhileLoop, IfStatement, ForLoop, SimpleLoop, CaseStatement
    pub conditions: Option<ConditionExpr>,
    // ForLoop
    pub iterator: Option<String>,
    pub range_start: Option<String>,
    pub range_end: Option<String>,
    pub direction: Option<String>,
    pub range_var: Option<String>,
    pub iterator_type: Option<String>,
    // CaseStatement
    pub switch_expression: Option<String>,
    pub cases: Option<Vec<String>>,
    pub parent : Option<Box<NodeData>>,
    pub body_start: Option<usize>,


    // ... add other relevant fields from BaseNode and its subclasses
}

impl NodeData {
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

/// Prints detailed information about the node in a color-coded format.
///
/// Displays all relevant fields of the node, such as name, type, position, conditions, and cases,
/// using color formatting to distinguish field types (e.g., blue for metadata, green for conditions).
/// This method is primarily used for debugging or inspecting the AST during development or analysis.
/// Optional fields are only printed if they contain values, and nested structures (e.g., arguments,
/// conditions) are formatted with indentation for clarity.
///
/// # Notes
/// - The output uses ANSI color codes from the `text_format` module (e.g., `BLUE`, `GREEN`).
/// - Complex fields like `conditions` and `arguments` are printed with nested details.
/// - The `parent` field, if present, is included to show hierarchical relationships.
/// - This method is I/O-bound and may be slow for large ASTs; consider using a verbosity flag for production.
///

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
            println!("{}  Is Body: {} {}{}", text_format::BLUE, text_format::ENDC, if is_body { "Body" } else { "Spec" }, ", ");
        }
         if let Some(arguments) = &self.arguments {
            println!("{}  Arguments: {}", text_format::GREEN, text_format::ENDC);
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
/// Manages a collection of `NodeData` instances in an `indextree::Arena`, with a root node
/// anchoring the tree. Provides methods to extract nodes, build the tree, and analyze Ada code
/// for coding standard enforcement.
///
/// # Fields
/// - `arena`: The `indextree` arena holding all nodes.
/// - `root_id`: The ID of the synthetic root node.
/// - `nodes_data`: Temporary storage for extracted nodes before building.
///
/// # Examples
/// ```
/// use ADA_Standards::{AST, NodeData};
/// let nodes = vec![NodeData::new("Root".to_string(), "RootNode".to_string(), None, None, false)];
/// let ast = AST::new(nodes);
/// ```

pub struct AST {
    arena: Arena<NodeData>,
    root_id: NodeId, // Hold the root NodeId
    nodes_data: Vec<NodeData>, // Temporary storage for node data before building tree
    node_ids: Vec<Option<NodeId>>, // Store NodeIds in parallel to nodes_data for association
}

impl AST {
    pub fn new(nodes_data: Vec<NodeData>) -> Self {
        let mut arena =  Arena::new();
        let root_id = arena.new_node(NodeData::new("root".to_string(), "RootNode".to_string(), None, None,false)); // Create root node
        AST {
            arena,
            root_id,
            nodes_data,
            node_ids: Vec::new(), // Initialize empty node_ids
        }
    }

pub fn associate_end_lines(&mut self,code_text : &str) -> Result<(), ASTError> {
        let end_statements = AST::extract_end_statements(code_text)?;

        let mut start_nodes: Vec<(NodeData, usize)> = self.nodes_data.iter().enumerate()
        .filter(|&(_i, n)| n.end_line.is_none())
        .map(|(i, n)| (n.clone(), i))
        .collect();

        start_nodes.sort_by_key(|n| n.0.start_index.unwrap_or(0));

        let mut events = vec![];
        for (node, original_index) in &start_nodes {
            events.push(ParseEvent::Start {
               node: node.clone(),
                index: node.start_index.unwrap_or(0),
                original_index: *original_index,
            });
        }
        
        for end_statement in &end_statements {
            events.push(ParseEvent::End {
                word: end_statement.word.clone(),
                index: end_statement.index,
                line: end_statement.line,
            });
        }
        

        events.sort_by_key(|e| match e {
            ParseEvent::Start { index, .. } => *index,
            ParseEvent::End { index, .. } => *index,
        });

        let mut stack: Vec<(NodeData, usize)> = vec![]; // (node, original_index)

        for event in events {
            match event {
                ParseEvent::Start { node, original_index, .. } => {
                    stack.push((node, original_index));
                }
                ParseEvent::End { word, line, .. } => {
                    if word.is_empty() {
                        // For blocks ending with "end;", e.g., declare blocks
                        if let Some(pos) = stack.iter().rposition(|(n, _)| AST::get_end_keyword(&n.node_type) == Some("")) {
                            let (mut node, original_index) = stack.remove(pos);
                            node.end_line = Some(line);
                            self.nodes_data[original_index] = node;
                        } else {
                            return Err(ASTError::NoMatchFound);
                        }
                    } else if UNNAMED_END_KEYWORDS.contains(&word.as_str()) {
                        // Unnamed end, e.g., "end loop;"
                        if let Some(pos) = stack.iter().rposition(|(n, _)| AST::get_end_keyword(&n.node_type) == Some(word.as_str())) {
                            let (mut node, original_index) = stack.remove(pos);
                            node.end_line = Some(line);
                            self.nodes_data[original_index] = node;
                        } else {
                            return Err(ASTError::NoMatchFound);
                        }
                    } else {
                        // Named end, e.g., "end Proc;"
                        if let Some(pos) = stack.iter().rposition(|(n, _)| n.name == word && AST::get_end_keyword(&n.node_type) == Some("name")) {
                            let (mut node, original_index) = stack.remove(pos);
                            node.end_line = Some(line);
                            self.nodes_data[original_index] = node;
                        } else {
                            return Err(ASTError::NoMatchFound);
                        }
                    }
                }
            }
        }

        if !stack.is_empty() {
        return Err(ASTError::NoMatchFound);
    }

    Ok(())
}


pub fn build(&mut self,code_text: &str) -> Result<(), ASTError> {
        self.associate_end_lines(code_text)?; // Associate end lines first

        self.arena = Arena::new(); // Re-create arena - root node will be re-added
        self.root_id = self.arena.new_node(NodeData::new("root".to_string(), "RootNode".to_string(), None, None, false)); // Re-create root

        let mut stack: Vec<(NodeId, Option<usize>)> = vec![(self.root_id, Some(usize::MAX))]; // Stack of (NodeId, end_line)

        for (index, node_data) in self.nodes_data.iter().enumerate() {
             if self.node_ids[index].is_none() {
                 // Skip placeholder nodes, or nodes without associated NodeIds if needed.
                 continue;
             }
            let current_node_id = self.node_ids[index].ok_or_else(||ASTError::NodeIdMissing(format!("Missing NodeId for node at index {}", index)))?; // Get NodeId for current node


        
            while let Some(&(_parent_node_id, parent_end_line)) = stack.last() {
                if parent_end_line < node_data.start_line {
                    stack.pop(); // Pop from stack if parent's end_line is before current node's start_line
                } else {
                    break; // Correct parent found
                }
            }

            if let Some(&(parent_node_id, _)) = stack.last() {
                parent_node_id.append(current_node_id, &mut self.arena); // Append to parent
            }

            stack.push((current_node_id, node_data.end_line)); // Push current node and its end_line
        }
        Ok(())
}


pub fn print_tree(&self) {
        println!("{}", self.output_tree());
}

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


pub fn print_nodes_info(&self) -> Result<(), ASTError> {
    for index in 0..self.nodes_data.len() {
        if let Some(node_id) = self.node_ids[index] {
            let node = self.arena.get(node_id).ok_or_else(||ASTError::NodeNotInArena(format!("Node with ID {:?} not found in arena", node_id)))?; 
            node.get().print_info();
             
        }
    }
    Ok(())
}

pub fn get_end_keyword(node_type: &str) -> Option<&'static str> {
    match node_type {
        "PackageNode" => Some("name"),
        "ProcedureNode" => Some("name"),
        "FunctionNode" => Some("name"),
        "SimpleLoop" => Some("loop"),
        "WhileLoop" => Some("loop"),
        "ForLoop" => Some("loop"),
        "IfStatement" => Some("if"),
        "CaseStatement" => Some("case"),
        "DeclareNode" => Some(""),
        _ => None,
    }
}

pub fn extract_end_statements(code_text: &str) -> Result<Vec<EndStatement>,ASTError> {
    let re = Regex::new(r"(?im)^\s*end(\s+(\w+))?;").map_err(|_| ASTError::RegexError)?;
    let mut ends = vec![];
    for cap in re.captures_iter(code_text) {
        let entire_match = cap.get(0).ok_or(ASTError::InvalidCapture)?;
        let word = cap.get(2).map_or("", |m| m.as_str()).to_string();
        let index = entire_match.start(); // Position of the match
        let line = code_text[..index].lines().count() + 1;
        ends.push(EndStatement { word, index, line });
    }
    if ends.is_empty() {
        return Err(ASTError::NoMatchFound); // Return error if no matches
    }
    Ok(ends)
}

pub fn extract_packages(code_text: &str) -> Result<Vec<NodeData>, ASTError> {
    let package_pattern = Regex::new(
        r"(?i)^(?!\s*--)(?:^[^\S\n]*|(?<=\n))(?P<type>\b(?:generic|separate)\b)?\s*(?P<category>\bpackage\b)(?:\s+(?P<body>\bbody\b))?\s+(?P<name>(?:(?!\bis(?! new\b)|;).)*)"
    ).map_err(|_| ASTError::RegexError)?;

    let mut nodes: Vec<NodeData> = Vec::new();
    let matches: Vec<_> = package_pattern.captures_iter(code_text).collect();
    
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
        let start_line = code_text[..mat.get(0).unwrap().start()].lines().count() + 1;
        let start_index = mat.get(0).unwrap().start();
        let name = mat.name("name").unwrap().as_str().to_string();
        let is_body = mat.name("body").is_some();

        let search_text = &code_text[mat.get(0).unwrap().end()..];
        let end_search = Regex::new(r"\s*(is|;)")
            .unwrap()
            .find(search_text);

        let mut node = NodeData::new(name.clone(), "PackageNode".to_string(), Some(start_line), Some(start_index), is_body);
        if let Some(end_match) = end_search {
            if end_match.as_str().trim() == ";" {
                node.end_line = Some(start_line);
                node.end_index = Some(mat.get(0).unwrap().end() + end_match.end());
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

pub fn extract_procedures_functions(code_text: &str) -> Result<Vec<NodeData>, ASTError> {
    let func_proc_pattern = Regex::new(
        r"(?i)^(?!\s*--)(?:^[^\S\n]*|(?<=\n))(?P<category>\bprocedure|function\b)\s+(?P<name>[^\s\(\;]*)(?:\s*\((?P<params>[\s\S]*?(?=\)))\))?(?:\s*return\s*(?P<return_statement>[\w\.\_\-]+))?"
    ).map_err(|_| ASTError::RegexError)?;

    let end_pattern = Regex::new(r"(?m)\s*(is|;)").map_err(|_| ASTError::RegexError)?;
    let mut nodes: Vec<NodeData> = Vec::new();

    for mat in func_proc_pattern.captures_iter(code_text) {
        let start_line = code_text[..mat.get(0).unwrap().start()].lines().count() + 1;
        let start_index = mat.get(0).unwrap().start();
        let category = mat.name("category").unwrap().as_str().to_lowercase();
        let name = mat.name("name").unwrap().as_str().to_string();
        let is_body = {
            let search_text = &code_text[mat.get(0).unwrap().end()..];
            if let Some(end_match) = end_pattern.find(search_text) {
                end_match.as_str().trim() == "is"
            } else {
                false
            }
        };

        let mut node = NodeData::new(
            name.clone(),
            if category == "function" { "FunctionNode" } else { "ProcedureNode" }.to_string(),
            Some(start_line),
            Some(start_index),
            is_body,
        );

        if !is_body {
            let search_text = &code_text[mat.get(0).unwrap().end()..];
            if let Some(end_match) = end_pattern.find(search_text) {
                if end_match.as_str().trim() == ";" {
                    node.end_line = Some(start_line);
                    node.end_index = Some(mat.get(0).unwrap().end() + end_match.end());
                }
            }
        }

        // Handle nested names (e.g., Package.Procedure)
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

pub fn extract_type_declarations(code_text: &str) -> Result<Vec<NodeData>, ASTError> {
    let type_pattern = Regex::new(
        r"(?i)^(?!\s*--)(?:^[^\S\n]*|(?<=\n))(?P<category>\btype\b|\bsubtype\b)\s+(?P<name>[\w\.\_]+)\s+is\s+[^;]+;"
    ).map_err(|_| ASTError::RegexError)?;

    let mut nodes = Vec::new();
    for mat in type_pattern.captures_iter(code_text) {
        let start_line = code_text[..mat.get(0).unwrap().start()].lines().count() + 1;
        let start_index = mat.get(0).unwrap().start();
        let name = mat.name("name").unwrap().as_str().to_string();

        let mut node = NodeData::new(
            name,
            "TypeDeclaration".to_string(),
            Some(start_line),
            Some(start_index),
            false, // Types are typically specifications
        );
        node.end_line = Some(start_line); // Single-line construct
        node.end_index = Some(mat.get(0).unwrap().end());
        nodes.push(node);
    }
    Ok(nodes)
}

pub fn extract_declare_blocks(code_text: &str) -> Result<Vec<NodeData>, ASTError> {
    let declare_pattern = Regex::new(r"(?i)^(?!\s*--)\s*\bdeclare\b").map_err(|_| ASTError::RegexError)?;
    let mut nodes = Vec::new();

    for mat in declare_pattern.find_iter(code_text) {
        let start_line = code_text[..mat.start()].lines().count() + 1;
        let start_index = mat.start();
        let node = NodeData::new(
            "DeclareBlock".to_string(),
            "DeclareNode".to_string(),
            Some(start_line),
            Some(start_index),
            true, // Declare blocks have bodies
        );
        nodes.push(node); // end_line remains None
    }
    Ok(nodes)
}

pub fn extract_control_flow_nodes(code_text: &str, nodes: &mut Vec<NodeData>) -> Result<Vec<NodeData>,ASTError> {
        let mut new_nodes_data: Vec<NodeData> = Vec::new();
        new_nodes_data.extend(AST::extract_simple_loops(code_text)?);
        new_nodes_data.extend(AST::extract_while_loops(code_text)?);
        new_nodes_data.extend(AST::extract_for_loops(code_text)?);
        nodes.extend(new_nodes_data.clone());
        Ok(new_nodes_data)
}

pub fn extract_simple_loops(code_text: &str) -> Result<Vec<NodeData>, ASTError> {
    let simpleloops_pattern = Regex::new(r"(?i)^\s*(?P<Captureloop>\bloop\b)").map_err(|_| ASTError::RegexError)?;
    let mut nodes = Vec::new();

    for mat in simpleloops_pattern.captures_iter(code_text) {
        let start_line = code_text[..mat.get(0).unwrap().start()].lines().count() + 1;
        let start_index = mat.get(0).unwrap().start();
        let node = NodeData::new(
            "SimpleLoop".to_string(),
            "SimpleLoop".to_string(),
            Some(start_line),
            Some(start_index),
            false,
        );
        nodes.push(node);
    }

    Ok(nodes)
}

/// Extracts `while` loops from Ada source code.
///
/// Parses the provided source code to identify `while` loops, capturing their loop condition,
/// start position, and other metadata. Creates `NodeData` instances for each `while` loop,
/// setting the `node_type` to "WhileLoop" and populating fields like `conditions`,
/// `start_line`, `start_index`, and `body_start`. Fields like `end_line` and `end_index`
/// are set to `None`, to be resolved later by `associate_end_lines`. This function is part
/// of the node extraction phase, enabling analysis of `while` loops for Ada coding standards,
/// such as ensuring termination conditions or limiting loop complexity.
///
/// # Parameters
/// - `code_text`: The Ada source code to parse, as a string slice.
///
/// # Returns
/// - `Ok(Vec<NodeData>)`: A vector of `NodeData` instances, each representing a `while` loop.
/// - `Err(ASTError)`: If the regular expression for parsing fails to compile or another error occurs.
///
/// # Errors
/// - `ASTError::RegexError`: If the regex pattern for `while` loops cannot be compiled.
///
///
/// # Notes
/// - Captures the condition after `while` up to `loop`, storing it in `conditions.list`.
/// - Sets `body_start` to the index after `loop`, marking the start of the loop body.
/// - Ignores comments and nested constructs during initial extraction.
/// - Use with `build` and `associate_end_lines` to complete node metadata.

pub fn extract_while_loops(code_text: &str) -> Result<Vec<NodeData>, ASTError> {
    let whileloops_pattern = Regex::new(r"(?im)(?P<Capturewhile>\bwhile\b)\s*(?P<exitcond2>(\n|.)*?)\s*\bloop\b[^\n;]*").map_err(|_| ASTError::RegexError)?;
    let mut nodes = Vec::new();

    for mat in whileloops_pattern.captures_iter(code_text) {
        let start_line = code_text[..mat.get(0).unwrap().start()].lines().count() + 1;
        let start_index = mat.get(0).unwrap().start();
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
        nodes.push(node);
    }

    Ok(nodes)
}


pub fn extract_for_loops(code_text: &str) -> Result<Vec<NodeData>, ASTError> {
    let forloops_pattern = Regex::new(r"(?i)(?P<Capturefor>\bfor\b)\s*(?P<index>.*?)\s*\bin\b\s*(?:(?P<loop_direction>.*?))?\s*(?P<primavar>[^\s]*)\s*(?:(?=\brange\b)\brange\b\s*(?P<frst>(?:.|\n)*?)\s+\.\.\s*(?P<scnd>(?:.|\n)*?)\s+\bloop\b|(?:(?=\.\.)\.\.\s*(?P<range_end>.*?)\s*\bloop\b|\s*\bloop\b))").map_err(|_| ASTError::RegexError)?;
    let mut nodes = Vec::new();

    for mat in forloops_pattern.captures_iter(code_text) {
        let start_line = code_text[..mat.get(0).unwrap().start()].lines().count() + 1;
        let start_index = mat.get(0).unwrap().start();
        let iterator = mat.name("index").unwrap().as_str().to_string();
        let range_start = mat.name("frst").map(|m| m.as_str().to_string());
        let range_end = mat.name("range_end").or_else(|| mat.name("scnd")).map(|m| m.as_str().to_string());
        let direction = mat.name("loop_direction").map_or("to".to_string(), |m| m.as_str().to_string());
        let iterator_type = mat.name("primavar").map(|m| m.as_str().to_string());
        let range_var = if range_start.is_none() && range_end.is_none() { Some(mat.name("primavar").unwrap().as_str().to_string()) } else { None };

        let mut node = NodeData::new("ForLoop".to_string(), "ForLoop".to_string(), Some(start_line), Some(start_index), false);
        node.iterator = Some(iterator);
        node.range_start = range_start;
        node.range_end = range_end;
        node.direction = Some(direction);
        node.iterator_type = iterator_type;
        node.range_var = range_var;
        nodes.push(node);
    }

    Ok(nodes)
}


pub fn extract_statement_nodes(code_text: &str, nodes: &mut Vec<NodeData>) -> Result<Vec<NodeData>,ASTError> {
        let mut new_nodes_data: Vec<NodeData> = Vec::new();
        new_nodes_data.extend(AST::extract_if_statements(code_text)?);
        new_nodes_data.extend(AST::extract_case_statements(code_text)?);
        nodes.extend(new_nodes_data.clone());
        Ok(new_nodes_data)
}

/// Extracts `if` statements from Ada source code.
///
/// Parses the provided source code to identify `if` statements, capturing their conditions,
/// start position, and other metadata. Creates `NodeData` instances for each `if` statement,
/// setting the `node_type` to "IfStatement" and populating fields like `conditions`,
/// `start_line`, `start_index`, and `body_start`. Fields like `end_line` and `end_index`
/// are set to `None`, to be resolved later by `associate_end_lines`. This function is part
/// of the node extraction phase, enabling analysis of `if` statements for Ada coding standards,
/// such as condition complexity or proper nesting.
///
/// # Parameters
/// - `code_text`: The Ada source code to parse, as a string slice.
///
/// # Returns
/// - `Ok(Vec<NodeData>)`: A vector of `NodeData` instances, each representing an `if` statement.
/// - `Err(ASTError)`: If the regular expression for parsing fails to compile or another error occurs.
///
/// # Errors
/// - `ASTError::RegexError`: If the regex pattern for `if` statements cannot be compiled.
///
///
/// # Notes
/// - Captures the condition after `if` up to `then`, storing it in `conditions.list`.
/// - Sets `body_start` to the index after `then`, marking the start of the `if` body.
/// - Ignores comments and nested constructs during initial extraction.
/// - Use with `build` and `associate_end_lines` to complete node metadata.

pub fn extract_if_statements(code_text: &str) -> Result<Vec<NodeData>, ASTError> {
    let if_pattern = Regex::new(r"(?i)^\s*(?P<ifstat>\bif\b)(?P<Condition>(?:.|\n)*?)(?<!\band\b\s)then").map_err(|_| ASTError::RegexError)?;
    let mut nodes = Vec::new();

    for mat in if_pattern.captures_iter(code_text) {
        let start_line = code_text[..mat.get(0).unwrap().start()].lines().count() + 1;
        let start_index = mat.get(0).unwrap().start();
        let condition_str = mat.name("Condition").unwrap().as_str().to_string();
        let conditions = AST::parse_condition_expression(&condition_str);
        let mut node = NodeData::new(
            "IfStatement".to_string(),
            "IfStatement".to_string(),
            Some(start_line),
            Some(start_index),
            false,
        );
        node.conditions = Some(conditions);
        nodes.push(node);
    }

    Ok(nodes)
}

/// Extracts case statements from Ada source code.
///
/// Identifies `case` statements using a regex pattern, capturing their switch expression and
/// body start position. Creates `NodeData` instances for each case statement, setting `cases`
/// to `None` for later population by `populate_cases`.
///
/// # Parameters
/// - `code_text`: The Ada source code to parse.
///
/// # Returns
/// - `Ok(Vec<NodeData>)`: A vector of `NodeData` instances for case statements.
/// - `Err(ASTError)`: If the regex pattern fails to compile.
///


pub fn extract_case_statements(code_text: &str) -> Result<Vec<NodeData>, ASTError> {
    let case_pattern = Regex::new(r#"(?i)(?<!end\s)(?:\"\s*|\'\s*)?(?P<Casestmnt>\bcase\b)\s*(?P<var>(?:.|\n)*?)\s*\bis\b(?:\s*\"|\s*\')?"#).map_err(|_| ASTError::RegexError)?;
    let mut nodes = Vec::new();

    for mat in case_pattern.captures_iter(code_text) {
        let start_line = code_text[..mat.get(0).unwrap().start()].lines().count() + 1;
        let start_index = mat.get(0).unwrap().start();
        let body_start = mat.get(0).unwrap().end(); // After "is"
        let switch_expression = mat.name("var").unwrap().as_str().to_string();
        let mut node = NodeData::new(
            "CaseStatement".to_string(),
            "CaseStatement".to_string(),
            Some(start_line),
            Some(start_index),
            false,
        );
        node.switch_expression = Some(switch_expression);
        node.body_start = Some(body_start); // Store the start of the body
        nodes.push(node);
    }

    Ok(nodes)
}

/// Populates the `cases` field for all `CaseStatement` nodes in the AST.
///
/// This method traverses the AST, identifies nodes with `node_type` "CaseStatement", and extracts
/// the list of case alternatives (e.g., "when X =>") from their body text, defined by `body_start`
/// and `end_index`. It is called after `build` to ensure all nodes have valid start and end positions.
/// The extracted cases are stored in the `cases` field, enabling analysis like checking for complete
/// coverage or duplicate cases in Ada code.
///
/// # Parameters
/// - `code_text`: The full Ada source code from which to extract case bodies.
///
/// # Returns
/// - `Ok(())` if cases are populated successfully.
/// - `Err(ASTError)` if a node lacks required positions (`body_start` or `end_index`) or if parsing fails.
///
/// # Errors
/// - `ASTError::NodeNotInArena` if a node ID is invalid.
/// - `ASTError::InvalidNodeData` if `body_start` or `end_index` is missing.
///
///
/// # Notes
/// - Must be called after `build` to ensure `end_index` is set.
/// - Handles nested case statements by checking indentation, if implemented.
/// - Performance depends on the number of case statements and body text size.

pub fn populate_cases(&mut self, code_text: &str) -> Result<(), ASTError> {
    // First, collect all the nodes that need updating
    let updates: Vec<_> = self.arena.iter()
        .filter_map(|nodo| {
            let node_id = self.arena.get_node_id(&nodo)?;
            let node = self.arena.get(node_id)?;
            if node.get().node_type == "CaseStatement" {
                let body_start = node.get().body_start.unwrap();
                let end_index = node.get().end_index.unwrap();
                let body_text = &code_text[body_start..end_index];
                let cases = AST::extract_cases_from_body(body_text);
                Some((node_id, cases))
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
/// as a vector of strings. This is a helper function for `populate_cases`.
///
/// # Parameters
/// - `body_text`: The body text of the case statement (from `body_start` to `end_index`).
///
/// # Returns
/// A vector of case conditions (e.g., ["1", "2", "others"]).

pub fn extract_cases_from_body(body_text: &str) -> Vec<String> {
    let re_when = Regex::new(r"(?is)when\s+(.*?)\s*=>").unwrap();
    let mut cases = Vec::new();
    for cap in re_when.captures_iter(body_text) {
        let choice = cap.get(1).unwrap().as_str().trim().to_string();
        cases.push(choice);
    }
    cases
}

pub fn parse_condition_expression(condition_str: &str) -> ConditionExpr {
    let mut list = Vec::new();
    let root_expression = AST::supersplitter(condition_str.to_string(), &mut list); // Pass mut list

    ConditionExpr { list,albero: Some(Box::new(root_expression)), }
}

// Utility function to check size, inlined for now
fn size_checker(keyword: &str, condstring: &str, index: usize) -> bool {
    keyword.len() + index < condstring.len()
}

// Utility function to check keyword, inlined for now
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


// Flag setter function - inlined and corrected to return updated values
pub fn flag_setter( char: char, string_flag: i32, number_of_open_parenthesis: i32, number_of_closed_parenthesis: i32) -> (i32, i32, i32) {
    let mut mut_string_flag = string_flag;
    let mut mut_number_of_open_parenthesis = number_of_open_parenthesis;
    let mut mut_number_of_closed_parenthesis = number_of_closed_parenthesis;

    if (char == '"' || char == '\'') && string_flag == 0 {
        mut_string_flag = 1;
    } else if (char == '"' || char == '\'') && string_flag == 1 {
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

// Flags check function - inlined for now
pub fn flags_check(number_of_open_parenthesis: i32, number_of_closed_parenthesis: i32, string_flag: i32) -> bool {
    (number_of_open_parenthesis - number_of_closed_parenthesis == 0) && string_flag == 0
}

// Is parenthesis exterior function
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

// Is expression a parenthesis function
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


pub fn clean_code(raw_code: &str) -> String {
    let space_to_tab_ratio = 4;
    let ada_code_content = raw_code.replace("\t", &" ".repeat(space_to_tab_ratio));
    let mut cleaned_lines = Vec::new();

    lazy_static! {
        static ref CLEAN_CODE_REGEX: Regex = Regex::new(r#""[^"]*"|--.*$"#).unwrap();
    }

    for line in ada_code_content.lines() {
        let cleaned_line = CLEAN_CODE_REGEX.replace_all(line, |caps: &regex::Captures| {
            if caps.get(0).map_or(false, |m| m.as_str().starts_with('"')) {
                caps.get(0).unwrap().as_str().to_string()
            } else {
                " ".repeat(caps.get(0).unwrap().as_str().len())
            }
        });
        cleaned_lines.push(cleaned_line.into_owned());
    }

    cleaned_lines.join("\n")
}


pub fn extract_all_nodes(code_text: &str) -> Result<Vec<NodeData>,ASTError> {
    let mut nodes: Vec<NodeData> = Vec::new();
    nodes.extend(AST::extract_packages(code_text)?);
    let mut temp_nodes_1: Vec<NodeData> = Vec::new();
    temp_nodes_1.extend(AST::extract_procedures_functions(code_text)?);
    let mut temp_nodes_2: Vec<NodeData> = Vec::new();
    temp_nodes_2.extend(AST::extract_type_declarations(code_text)?);
    let mut temp_nodes_3: Vec<NodeData> = Vec::new();
    temp_nodes_3.extend(AST::extract_declare_blocks(code_text)?);
    let mut temp_nodes_4: Vec<NodeData> = Vec::new();
    temp_nodes_4.extend(AST::extract_control_flow_nodes(code_text, &mut nodes)?);
    let mut temp_nodes_5: Vec<NodeData> = Vec::new();
    temp_nodes_5.extend(AST::extract_statement_nodes(code_text, &mut nodes)?);

    nodes.extend(temp_nodes_1);
    nodes.extend(temp_nodes_2);
    nodes.extend(temp_nodes_3);
    nodes.extend(temp_nodes_4);
    nodes.extend(temp_nodes_5);
    Ok(nodes)

}

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



}







