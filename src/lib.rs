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
use fancy_regex::Regex as Reg;
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
    end_index: usize,
}

const UNNAMED_END_KEYWORDS: [&str; 4] = ["loop", "if", "case", "record"];

// Make sure this is the definition you are using
enum ParseEvent {
    StartNodeId { node_id: NodeId, index: usize }, // Use this variant
    End { word: String, index: usize, line: usize, end_index: usize },
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
     pub arena: Arena<NodeData>,
     pub root_id: NodeId, // Hold the root NodeId
     pub nodes_data: Vec<NodeData>, // Temporary storage for node data before building
     pub node_ids: Vec<Option<NodeId>>, // Store NodeIds in parallel to nodes_data for association
 }

impl AST {
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



// Add this function inside `impl AST { ... }`
fn associate_end_lines_in_arena(&mut self, code_text: &str) -> Result<(), ASTError> {
    // 1. Extract all end statements
    let end_statements = AST::extract_end_statements(code_text)?;

    // 2. Get NodeIds and start indices for nodes needing an end line.
    //    These are already in sorted order because self.node_ids is parallel to sorted self.nodes_data.
    let start_node_ids: Vec<(NodeId, usize)> = self.node_ids.iter()
        .filter_map(|&opt_id| opt_id) // Get NodeId from Option
        .filter(|&node_id| self.arena.get(node_id).unwrap().get().end_line.is_none()) // Filter nodes without end_line
        .map(|node_id| (node_id, self.arena.get(node_id).unwrap().get().start_index.unwrap_or(0))) // Map to (NodeId, start_index)
        .collect();

    // 3. Create combined event list (NodeId starts + End statements)
    let mut events: Vec<ParseEvent> = Vec::new();
    for &(node_id, start_index) in &start_node_ids {
        // Use the StartNodeId variant
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
        // _ => usize::MAX, // Remove if ParseEvent only has these two variants now
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
                    // Get data directly from the arena for the check
                    if let Some(node) = self.arena.get(*node_id) {
                         let node_data = node.get();
                         if word.is_empty() { // "end;"
                             AST::get_end_keyword(&node_data.node_type) == Some("")
                         } else if UNNAMED_END_KEYWORDS.contains(&word.as_str()) { // "end loop;"
                             AST::get_end_keyword(&node_data.node_type) == Some(word.as_str())
                         } else { // "end Name;"
                             node_data.name == word && AST::get_end_keyword(&node_data.node_type) == Some("name")
                         }
                    } else {
                        false // NodeId not valid in arena? Should not happen.
                    }
                };

                // Find the most recent node on the stack that matches
                if let Some(pos) = stack.iter().rposition(match_logic) {
                    let node_id_to_update = stack.remove(pos);
                    // Update the node directly in the arena
                    if let Some(node) = self.arena.get_mut(node_id_to_update) {
                         let node_data = node.get_mut();
                         node_data.end_line = Some(line);
                         node_data.end_index = Some(end_index);
                    } else {
                        // This would indicate a serious internal error
                        return Err(ASTError::NodeNotInArena(format!("NodeId {:?} not found during update", node_id_to_update)));
                    }
                } else {
                    // Log warning or handle unmatched 'end' - do not error immediately
                    eprintln!("Warning: Unmatched 'end {}' at line {}", word, line);
                }
            }
            // Remove if ParseEvent only has StartNodeId and End
            // _ => {}
        }
    }

    // Stack check - consider making this optional or a warning
    if !stack.is_empty() {
        eprintln!("Warning: Stack not empty after associating end lines. Mismatched blocks?");
        // return Err(ASTError::NoMatchFound); // Decide if this is a hard error
    }

    Ok(())
}


pub fn build(&mut self, code_text: &str) -> Result<(), ASTError> {
    // 1. Sort nodes_data by start index FIRST.
    self.nodes_data.sort_by_key(|n| n.start_index.unwrap_or(0));

    // 2. Reset arena and create root node
    self.arena = Arena::new();
    self.root_id = self.arena.new_node(NodeData::new("root".to_string(), "RootNode".to_string(), None, None, false));

    // 3. Populate the arena and create node_ids IN PARALLEL with the *sorted* nodes_data
    self.node_ids = self.nodes_data.iter()
        .map(|node_data| Some(self.arena.new_node(node_data.clone())))
        .collect();

    // 4. ✅ CORRECT: Associate end lines *now*, operating directly on the arena
    self.associate_end_lines_in_arena(code_text)?; // <--- This is the correct helper call

    // --- Tree Building Logic ---
    let mut stack: Vec<(NodeId, usize)> = vec![(self.root_id, usize::MAX)]; // Stack: (NodeId, effective_end_line)

    // Iterate using the node_ids we just created
    for node_id_option in &self.node_ids { // Iterate over node_ids directly
         if node_id_option.is_none() {
             continue;
         }
         let current_node_id = node_id_option.unwrap();

         // Get the necessary data *before* potentially modifying the arena.
         let (current_start_line, current_effective_end) = {
             // Borrow immutably just for this block
             let node_data_ref = self.arena.get(current_node_id).unwrap().get();
             (
                 node_data_ref.start_line.unwrap_or(0),
                 node_data_ref.end_line.unwrap_or(usize::MAX) // Get end_line AFTER associate_end_lines ran
             )
         }; // Immutable borrow ends here


         // Pop parents whose scope ends before the current node starts
         while let Some(&(_parent_id, parent_effective_end)) = stack.last() {
             if parent_effective_end < current_start_line {
                 stack.pop();
             } else {
                 break; // Found the correct parent
             }
         }

         // Append to the parent currently on top of the stack
         // This mutable borrow is now safe.
         if let Some(&(parent_node_id, _)) = stack.last() {
            parent_node_id.append(current_node_id, &mut self.arena);
         } else {
             self.root_id.append(current_node_id, &mut self.arena);
         }

         // Push the current node onto the stack using the copied end line value.
         stack.push((current_node_id, current_effective_end));
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
/// 
pub fn extract_procedures_functions(code_text: &str) -> Result<Vec<NodeData>, ASTError> {
    let func_proc_pattern = Reg::new(
        r"(?im)^(?!\s*--)(?:^[^\S\n]*|(?<=\n))(?P<category>\bprocedure|function\b)\s+(?P<name>[^\s\(\;]*)(?:\s*\((?P<params>[\s\S]*?(?=\)))\))?(?:\s*return\s*(?P<return_statement>[\w\.\_\-]+))?"
    ).map_err(|_| ASTError::RegexError)?;

    let end_pattern = Regex::new(r"(?m)\s*(is|;)").map_err(|_| ASTError::RegexError)?;
    let mut nodes: Vec<NodeData> = Vec::new();

    for cap_res in func_proc_pattern.captures_iter(code_text) {
        let mat = cap_res.map_err(|_| ASTError::RegexError)?;
        let start_line = code_text[..mat.get(0).unwrap().start()].lines().count() + 1;
        let start_index = mat.get(0).unwrap().start();
        let category = mat.name("category").unwrap().as_str().to_lowercase();
        let name = mat.name("name").unwrap().as_str().to_string();
        let is_body = {
            let search_text = &code_text[mat.get(0).unwrap().end()..];
            if let Some(end_match) = end_pattern.find(search_text) {
                if end_match.as_str().trim() == "is" {
                    let text_after_is = &search_text[end_match.end()..];
                    !text_after_is.trim_start().starts_with("new")
                } else {
            // The token was ";", which is always a specification.
            false
            }
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

/// Extracts `type`, `subtype`, and `for...use` declarations from Ada source code.
///
/// # Examples
/// ```
/// use ADA_Standards::{NodeData, ASTError, AST};
/// # fn main() -> Result<(), ASTError> {
/// let code = r#"procedure estocazz is
/// type CustomType is record
///     DataString : String(1 .. 3) := "oui";
///     DataFloat : Float;
/// end record;"#;
/// let c_code = AST::clean_code(code);
/// let nodes = AST::extract_type_declarations(&c_code)?;
/// assert_eq!(nodes.len(), 1);
/// assert_eq!(nodes[0].name, "CustomType");
/// assert_eq!(nodes[0].node_type, "TypeDeclaration");
/// assert_eq!(nodes[0].is_body, Some(false)); // Specification
/// Ok(())
/// # }
/// ```
pub fn extract_type_declarations(code_text: &str) -> Result<Vec<NodeData>, ASTError> {
    // PATTERN 1: Matches 'type' and 'subtype' declarations.
    // This regex is a direct translation and improvement of the first Python pattern.
    // It correctly handles derived types (`is new`), enumerations (`is (...)`), and base types with ranges.
    let type_subtype_pattern = Reg::new(
        r#"(?ims)^(?!\s*--)(?:^[^\S\n]*|(?<=\n))(?P<category>\btype\b|\bsubtype\b)\s+(?P<name>.*?)(?:\s+is(?:\s+new\s+(?P<base_type>[\w\._\-]+)|(?:\s*(?P<tuple_type>\([^)]+\))|\s*(?P<type_kind>[\w\._\-]+(?:\s*range\s*.*?)?))))"#
    ).map_err(|_| ASTError::RegexError)?;

    // PATTERN 2: Matches 'for ... use record' representation clauses.
    let repr_clause_pattern = Reg::new(
        r#"(?im)^(?!\s*--)(?:^[^\S\n]*|(?<=\n))(?P<category>\bfor\b)\s+(?P<name>\w+)\s+use\s+(?P<type_kind>\brecord\b)"#
    ).map_err(|_| ASTError::RegexError)?;

    let mut nodes: Vec<NodeData> = Vec::new();

    // --- Pass 1: Find 'type' and 'subtype' declarations ---
    for cap_res in type_subtype_pattern.captures_iter(code_text) {
        let caps = cap_res.map_err(|_| ASTError::RegexError)?;
        let full_match = caps.get(0).ok_or(ASTError::MatchItemMissing)?;
        let category_match = caps.name("category").ok_or(ASTError::InvalidCapture)?;

        let start_index = full_match.start();
        let start_line = code_text[..start_index].lines().count() + 1;
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

        // Determine if the declaration is a single-line spec
        let (end_line, end_index) = if type_kind == "record" {
            (None, None) // Records are multi-line, `associate_end_lines` will find the end
        } else {
            // Most other types are single-line declarations ending in ';'
            let end_pos = code_text[full_match.end()..].find(';').map(|p| full_match.end() + p + 1);
            if let Some(pos) = end_pos {
                (Some(code_text[..pos].lines().count()), Some(pos))
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
        let full_match = caps.get(0).ok_or(ASTError::MatchItemMissing)?;

        let start_index = full_match.start();
        let start_line = code_text[..start_index].lines().count() + 1;
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
        
        // end_line and end_index are None; will be found by `associate_end_lines`
        
        nodes.push(node);
    }

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
/// let code = r#"procedure Main is
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
    // This is the direct Rust translation of the Python regex.
    // It uses fancy_regex (Reg) to support the positive lookahead `(?=...)`.
    // It does NOT anchor to the start of a line (no '^').
    let declare_pattern = Reg::new(
        r#"(?i)(?!\s*--)(?P<declare>\bdeclare\b)(?=([^\"]*\"[^\"]*\")*[^\"]*$)"#
    ).map_err(|_| ASTError::RegexError)?;

    let mut nodes = Vec::new();

    // We must use captures_iter to get the named group "declare"
    for cap_res in declare_pattern.captures_iter(code_text) {
        let caps = cap_res.map_err(|_| ASTError::RegexError)?;
        // Get the match for the *named group* "declare"
        let mat = caps.name("declare").ok_or(ASTError::InvalidCapture)?;

        let start_line = code_text[..mat.start()].lines().count() + 1;
        let start_index = mat.start(); // The index of the word "declare"
        let node = NodeData::new(
            "DeclareBlock".to_string(),
            "DeclareNode".to_string(),
            Some(start_line),
            Some(start_index),
            true, // Declare blocks have bodies
        );
        nodes.push(node);
    }
    Ok(nodes)
}

// REMOVE the `nodes: &mut Vec<NodeData>` parameter
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
        r"(?is)(?P<Capturefor>\bfor\b)\s*(?P<index>.*?)\s*\bin\b\s*(?:(?P<loop_direction>.*?))?\s*(?P<primavar>[^\s]*)\s*(?:(?=\brange\b)\brange\b\s*(?P<frst>.*?)\s+\.\.\s*(?P<scnd>.*?)\s+\bloop\b|(?:(?=\.\.)\.\.\s*(?P<range_end>.*?)\s*\bloop\b|\s*\bloop\b))"
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


// REMOVE the `nodes: &mut Vec<NodeData>` parameter
pub fn extract_statement_nodes(code_text: &str) -> Result<Vec<NodeData>,ASTError> {
    let mut new_nodes_data: Vec<NodeData> = Vec::new();
    new_nodes_data.extend(AST::extract_if_statements(code_text)?);
    new_nodes_data.extend(AST::extract_case_statements(code_text)?);
    // REMOVE this line: nodes.extend(new_nodes_data.clone());
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
    // FIX 1: Use Reg::new (from fancy-regex)
    // FIX 2: Add 'm' (multiline) and 's' (dotall) flags
    let if_pattern = Reg::new(
        r"(?ims)^\s*(?P<ifstat>\bif\b)(?P<Condition>.*?)(?<!\band\b\s)then"
    ).map_err(|_| ASTError::RegexError)?;
    
    let mut nodes = Vec::new();

    for mat in if_pattern.captures_iter(code_text) {
        // Must unwrap the Result from fancy-regex's iterator
        let captures = mat.map_err(|_| ASTError::RegexError)?; 
        let full_match = captures.get(0).ok_or(ASTError::MatchItemMissing)?;
        let if_keyword = captures.name("ifstat").ok_or(ASTError::InvalidCapture)?;

        // FIX 3: Correct line and index counting
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
        
        // FIX 4: Set body_start to the end of the full match (after "then")
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
    // FIX: This regex uses the "skip-fail" pattern.
    // 1. Match strings (`\"[^\"]*\"`) and ignore them.
    // 2. Match comments (`--.*$`) and ignore them.
    // 3. OR, match the target `case` statement and capture it.
    let case_pattern = Reg::new(
        // Note: comments are already spaces, but this is a robust pattern
        r#"(?ims)\"[^\"]*\"|--.*$|(?<!end\s)(?P<Casestmnt>\bcase\b)\s*(?P<var>.*?)\s*\bis\b"#
    ).map_err(|_| ASTError::RegexError)?;
    
    let mut nodes = Vec::new();

    for mat in case_pattern.captures_iter(code_text) {
        let captures = mat.map_err(|_| ASTError::RegexError)?;

        // CRITICAL: Only proceed if we captured our target group ("Casestmnt").
        // This filters out the string and comment matches.
        if let Some(case_keyword) = captures.name("Casestmnt") {
            let full_match = captures.get(0).ok_or(ASTError::MatchItemMissing)?;

            let start_line = code_text[..case_keyword.start()].matches('\n').count() + 1;
            let start_index = case_keyword.start();
            
            let body_start = full_match.end(); // After "is"
            let switch_expression = captures.name("var").unwrap().as_str().trim().to_string();
            
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
    // First, replace tabs with spaces
    let ada_code_content = raw_code.replace("\t", &" ".repeat(space_to_tab_ratio));

    lazy_static! {
        // This regex is multiline-aware ('m' flag).
        // It matches one of two things:
        // 1. (?P<string>\"[^\"]*\"): A complete string literal.
        // 2. (?P<comment>--.*$): A comment from '--' to the end of the line.
        static ref CLEAN_CODE_REGEX: Regex = Regex::new(
            r#"(?m)(?P<string>\"[^\"]*\")|(?P<comment>--.*$)"#
        ).unwrap();
    }

    // Run replace_all on the *entire* string, not line by line.
    // This preserves all newlines, including blank lines.
    let cleaned_code = CLEAN_CODE_REGEX.replace_all(&ada_code_content, |caps: &regex::Captures| {
        // Check if we matched the "string" group
        if caps.name("string").is_some() {
            // It's a string, so *return it unchanged* (as per your test)
            caps.name("string").unwrap().as_str().to_string()
        } 
        // Check if we matched the "comment" group
        else if let Some(comment) = caps.name("comment") {
            // It's a comment, so *replace it with spaces*.
            " ".repeat(comment.as_str().len())
        } 
        else {
            // This case should not be hit
            caps.get(0).unwrap().as_str().to_string()
        }
    });

    // Return the modified string.
    cleaned_code.into_owned()
}


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







