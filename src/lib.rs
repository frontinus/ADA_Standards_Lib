use indextree::{Arena, NodeId};
use std::collections::VecDeque;
use regex::Regex;
use fancy_regex::Regex as Reg;
use lazy_static::lazy_static;


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
    // albero: ...  // Skipping 'albero' for now, needs tree structure consideration
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


#[derive(Debug,Clone)] 
pub struct NodeData {
    pub name: String,
    pub node_type: String, // e.g., "BaseNode", "WhileLoop", etc.
    pub start_line: Option<usize>, // Use Option to represent potentially missing values
    pub end_line: Option<usize>,
    pub start_index: Option<usize>,
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


    // ... add other relevant fields from BaseNode and its subclasses
}

impl NodeData {
    fn new(name: String, node_type: String, start_line: Option<usize>, start_index: Option<usize>) -> Self {
        NodeData {
            name,
            node_type,
            start_line,
            end_line: None, // Initially end_line is None
            start_index,
            column: None,
            is_body: None,
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
        }
    }

    fn print_info(&self) {
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
             if let cond_list = &conditions.list {
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


struct AST {
    arena: Arena<NodeData>,
    root_id: NodeId, // Hold the root NodeId
    nodes_data: Vec<NodeData>, // Temporary storage for node data before building tree
    node_ids: Vec<Option<NodeId>>, // Store NodeIds in parallel to nodes_data for association
}

impl AST {
    fn new(nodes_data: Vec<NodeData>) -> Self {
        let mut arena =  Arena::new();
        let root_id = arena.new_node(NodeData::new("root".to_string(), "RootNode".to_string(), None, None)); // Create root node
        AST {
            arena,
            root_id,
            nodes_data,
            node_ids: Vec::new(), // Initialize empty node_ids
        }
    }

    fn associate_end_lines(&mut self) -> Result<(), String> {
        let mut sorted_node_indices: Vec<usize> = (0..self.nodes_data.len()).collect();
        sorted_node_indices.sort_by_key(|&index| self.nodes_data[index].start_index);

        let mut associated_nodes_data: Vec<NodeData> = Vec::new();
        let mut associated_node_ids: Vec<Option<NodeId>> = Vec::new(); // Store NodeIds in parallel
        let mut stack: Vec<usize> = Vec::new(); // Stack to store indices of start nodes

        for original_index in sorted_node_indices {
            let node_data = &self.nodes_data[original_index]; // Access node_data by index (borrow, don't remove)

            if node_data.end_line.is_none() {
                // It's a start node, push its *original index* onto the stack.
                let node_id = self.arena.new_node(node_data.clone());
                self.node_ids.push(Some(node_id)); // Store the NodeId
                stack.push(self.node_ids.len() - 1); // Push index in node_ids vector
                associated_nodes_data.push(node_data.clone()); 
                associated_node_ids.push(Some(node_id)); 

            } else {
                // It's an end node
                if let Some(start_node_index_in_ids) = stack.pop() {
                    // Get the NodeId of the start node from node_ids
                    let start_node_id = self.node_ids[start_node_index_in_ids];
                    if let Some(mut_node) = self.arena.get_mut(start_node_id.expect("KBOOM_1")) {
                        mut_node.get_mut().end_line = node_data.start_line; // Use end_line as start_line for EndNode in Python logic

                        // Store the updated start node data and id
                        associated_nodes_data.push(mut_node.get().clone()); // Clone the data
                        associated_node_ids.push(start_node_id);
                    } else {
                        return Err("Start node not found in arena during end line association".to_string());
                    }

                } else {
                     associated_nodes_data.push(node_data.clone());
                     associated_node_ids.push(None); // No NodeId to associate for end node
                }
            }
        }
        self.nodes_data = associated_nodes_data;
        self.node_ids = associated_node_ids;


        Ok(())
    }


    fn build(&mut self) -> Result<(), String> {
        self.associate_end_lines()?; // Associate end lines first

        self.arena = Arena::new(); // Re-create arena - root node will be re-added
        self.root_id = self.arena.new_node(NodeData::new("root".to_string(), "RootNode".to_string(), None, None)); // Re-create root

        let mut stack: Vec<(NodeId, Option<usize>)> = vec![(self.root_id, Some(usize::MAX))]; // Stack of (NodeId, end_line)

        for (index, node_data) in self.nodes_data.iter().enumerate() {
             if self.node_ids[index].is_none() {
                 // Skip placeholder nodes, or nodes without associated NodeIds if needed.
                 continue;
             }
            let current_node_id = self.node_ids[index];


            while let Some(&(parent_node_id, parent_end_line)) = stack.last() {
                if parent_end_line < node_data.start_line {
                    stack.pop(); // Pop from stack if parent's end_line is before current node's start_line
                } else {
                    break; // Correct parent found
                }
            }

            if let Some(&(parent_node_id, _)) = stack.last() {
                parent_node_id.append(current_node_id.expect("KBOOM_2"), &mut self.arena); // Append to parent
            }

            stack.push((current_node_id.expect("KBOOM_3"), node_data.end_line)); // Push current node and its end_line
        }
        Ok(())
    }


    fn print_tree(&self) {
        println!("{}", self.output_tree());
    }

    fn output_tree(&self) -> String {
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


    fn print_nodes_info(&self) -> Result<(), String> {
        for index in 0..self.nodes_data.len() {
            if !self.node_ids[index].is_none() {
                if let Some(node) = self.arena.get(self.node_ids[index].expect("KABOOM_4")) {
                    node.get().print_info();
                } else {
                    return Err(format!("NodeId {:?} not found in arena for print_nodes_info", self.node_ids[index]));
                }
            }
        }
        Ok(())
    }

    fn extract_packages(code_text: &str) -> Vec<NodeData> {
        let mut nodes_data: Vec<NodeData> = Vec::new();
        lazy_static! {
            static ref PACKAGE_PATTERN: Reg = Reg::new(
                r"^(?i)(?!\s*--)\s*(?<type>\b(?:generic|separate)\b)?\s*\bpackage\b(?:\s+\bbody\b)?\s+(?<name>(?:(?!\bis(?! new\b)|;).)*)"
            ).unwrap();
        }

        let package_matches = PACKAGE_PATTERN.captures_iter(code_text);

        for match_item in package_matches {
            let category = match_item.as_ref().expect("GUOOO").name("category").unwrap().as_str();
            let start_index = match_item.as_ref().expect("GUOOO").name("category").unwrap().start();
            let start_line = code_text[..start_index].lines().count() + 1; // Correct line count

            let name = match_item.as_ref().expect("GUOOO").name("name").unwrap().as_str().to_string();
            let is_body = match_item.as_ref().expect("GUOOO").name("body").is_some();

            let mut node_data = NodeData::new(name.clone(), "PackageNode".to_string(), Some(start_line), Some(start_index));
            node_data.is_body = Some(is_body);
            node_data.category_type = Some(category.to_string());
            nodes_data.push(node_data);

            let search_text = &code_text[match_item.as_ref().expect("GUOOO").get(0).unwrap().end()..]; // Search after the match
            lazy_static! {
                static ref END_SEARCH_PATTERN: Regex = Regex::new(r"(?im)\s*(is|;)").unwrap();
            }
            if let Some(end_match) = END_SEARCH_PATTERN.find(search_text) {
                if end_match.as_str().contains(';') {
                    let end_index_val = match_item.as_ref().expect("GUOOO").get(0).unwrap().end() + end_match.end();
                    let end_line = code_text[..end_index_val].lines().count() + 1;
                    let mut end_node_data = NodeData::new("EndPackage".to_string(), "EndNode".to_string(), Some(end_line), Some(end_index_val)); // Use a distinct name for end nodes
                    end_node_data.end_line = Some(end_line);
                    nodes_data.push(end_node_data);
                }
            }
        }
        nodes_data
    }

    fn extract_procedures_functions(code_text: &str, nodes: &mut Vec<NodeData>) -> Vec<NodeData> {
        lazy_static! {
           static ref FUNCTION_PATTERN: Reg = Reg::new(r"(?i)^(?!\s*--)(?:^[^\S\n]*|(?<=\n))(?<category>\bprocedure|function\b)\s+(?<name>[^\s\(\;]*)(?:\s*\bis\b\s*\bnew\b\s*(?:(?<unchecked_conversion_type>(.*\.unchecked_conversion|unchecked_conversion)(?=\s*\())|(?<unchecked_deallocation_type>(.*\.unchecked_deallocation|unchecked_deallocation)(?=\s*\()))?)?(?:\s*\((?<params>[\s\S]*?(?=\)))\))?(?:\s*return\s*(?<return_statement>[\w\.\_\-]+))?(?:\s*renames\s*(?<renames_type>[\w\.\_\-]+))?").unwrap();
        }
       let function_matches = FUNCTION_PATTERN.captures_iter(code_text);
       let mut new_nodes_data: Vec<NodeData> = Vec::new();

       for match_item in function_matches {
           let category_type = match_item.as_ref().expect("GUOOO").name("category").unwrap().as_str().to_string();
           let start_index = match_item.as_ref().expect("GUOOO").name("category").unwrap().start();
           let start_line = code_text[..start_index].lines().count() + 1;
           let name = match_item.as_ref().expect("GUOOO").name("name").unwrap().as_str().to_string();
           let params_str = match_item.as_ref().expect("GUOOO").name("params").map(|m| m.as_str().to_string());
           let return_statement = match_item.as_ref().expect("GUOOO").name("return_statement").map(|m| m.as_str().to_string());

           let mut arguments: Vec<ArgumentData> = Vec::new();
           if let Some(params) = params_str {
               let param_list: Vec<&str> = params.split(';').collect();
               for param_str in param_list {
                   let param_str_cleaned = Regex::new(r"--(.*)(?=\n)").unwrap().replace_all(Regex::new(r"\s+").unwrap().replace_all(param_str.trim(), " ").as_ref(), "").to_string();
                   if !param_str_cleaned.trim().is_empty() {
                       if let Some(param_match) = Regex::new(r"([\w\s,]+?)\s*:\s*(in\s+out|in\s|out\s)?([\w\.\s_]+)(?:\s*:=\s*(.+))?").unwrap().captures(&param_str_cleaned) {
                            let names_str = param_match.get(1).map_or("", |m| m.as_str());
                            let mode = param_match.get(2).map_or("in", |m| m.as_str());
                            let data_type = param_match.get(3).map_or("", |m| m.as_str());
                            let default_value = param_match.get(4).map(|m| m.as_str().to_string());

                            let names: Vec<&str> = names_str.split(',').map(|s| s.trim()).collect();
                            for name in names {
                               arguments.push(ArgumentData{
                                   name: name.to_string(),
                                   mode: mode.to_string(),
                                   data_type: data_type.to_string(),
                                   default_value: default_value.clone(),
                               });
                            }
                       }
                   }
               }
           }

           let return_keyword = return_statement.map(|rt| ReturnKeywordData { data_type: Some(rt) });

           let mut node_data = NodeData::new(name.clone(),
                                              if category_type == "function" { "FunctionNode".to_string() } else { "ProcedureNode".to_string() },
                                              Some(start_line), Some(start_index));
           node_data.category_type = Some(category_type.clone());
           node_data.arguments = Some(arguments);
           node_data.return_type = return_keyword;
           new_nodes_data.push(node_data);

            let search_text = &code_text[match_item.as_ref().expect("GUOOO").get(0).unwrap().end()..]; // Search after the match
           lazy_static! {
               static ref END_PROC_FUNC_PATTERN: Regex = Regex::new(r"(?im)\s*(is|;)").unwrap();
           }
           if let Some(end_match) = END_PROC_FUNC_PATTERN.find(search_text) {
               if end_match.as_str().contains(';') {
                   let end_index_val = match_item.as_ref().expect("GUOOO").get(0).unwrap().end() + end_match.end();
                   let end_line = code_text[..end_index_val].lines().count() + 1;
                   let mut end_node_data = NodeData::new(format!("End{}", if category_type == "function" { "Function" } else { "Procedure" }), "EndNode".to_string(), Some(end_line), Some(end_index_val)); // Differentiate end node names
                   end_node_data.end_line = Some(end_line);
                   new_nodes_data.push(end_node_data);
               }
           }
       }
       nodes.extend(new_nodes_data.clone()); // Append new nodes to the existing list
       new_nodes_data // Return the newly created nodes
   }

   fn extract_type_declarations(code_text: &str, nodes: &mut Vec<NodeData>) -> Vec<NodeData> {
    lazy_static! {
        static ref TYPE_PATTERN: Regex = Regex::new(
            r"(?im)(?P<category>\btype\b|\bsubtype\b)\s+(?P<name>[\S\s]*?(?=\bis\b))(?:\s*is\s*(?:(?P<tuple_type>\([^)]+\))|(?P<type_kind>\w+)|new\s*(?P<base_type>[\w\.\_\-]+)))?"
        ).unwrap();
    }
    let type_matches = TYPE_PATTERN.captures_iter(code_text);
    let mut new_nodes_data: Vec<NodeData> = Vec::new();

    for match_item in type_matches {
        let category_type = match_item.name("category").unwrap().as_str().to_string();
        let start_index = match_item.name("category").unwrap().start();
        let start_line = code_text[..start_index].lines().count() + 1;
        let name = match_item.name("name").unwrap().as_str().to_string();
        let type_kind = match_item.name("type_kind").map(|m| m.as_str().to_string());
        let tuple_type_str = match_item.name("tuple_type").map(|m| m.as_str().to_string());
        let base_type = match_item.name("base_type").map(|m| m.as_str().to_string());

        let tuple_values = tuple_type_str.map(|tuple_str|
            Regex::new(r"[^,\s]+").unwrap().find_iter(&tuple_str).map(|m| m.as_str().to_string()).collect()
        );

        let mut node_data = NodeData::new(name.clone(), "TypeDeclaration".to_string(), Some(start_line), Some(start_index));
        node_data.category_type = Some(category_type.clone());
        node_data.type_kind = type_kind;
        node_data.tuple_values = tuple_values;
        node_data.base_type = base_type;
        new_nodes_data.push(node_data);

        let end_line = code_text[..match_item.get(0).unwrap().end()].lines().count() + 1;
        let end_index_val = match_item.get(0).unwrap().end();
        let mut end_node_data = NodeData::new("EndTypeDeclaration".to_string(), "EndNode".to_string(), Some(end_line), Some(end_index_val)); // Use specific end node name
        end_node_data.end_line = Some(end_line);
        new_nodes_data.push(end_node_data);
    }
    nodes.extend(new_nodes_data.clone());
    new_nodes_data
}

fn extract_declare_blocks(code_text: &str, nodes: &mut Vec<NodeData>) -> Vec<NodeData> {
    lazy_static! {
        static ref DECLARE_PATTERN: Reg = Reg::new(r#"(?i)(?!\s*--)(?<declare>\bdeclare\b)(?=([^"]*"[^"]*")*[^"]*$)"#).unwrap();
    }
    let declare_matches = DECLARE_PATTERN.captures_iter(code_text);
    let mut new_nodes_data: Vec<NodeData> = Vec::new();

    for match_item in declare_matches {
        let start_index = match_item.as_ref().expect("MERDA_1").name("declare").unwrap().start();
        let start_line = code_text[..start_index].lines().count() + 1;

        let node_data = NodeData::new("DeclareBlock".to_string(), "DeclareNode".to_string(), Some(start_line), Some(start_index));
        new_nodes_data.push(node_data);

        let end_index_val = match_item.expect("MERDA_3").get(0).unwrap().end();
        let end_line = code_text[..end_index_val].lines().count() + 1;
        
        let mut end_node_data = NodeData::new("EndDeclareBlock".to_string(), "EndNode".to_string(), Some(end_line), Some(end_index_val)); // Specific end node name
        end_node_data.end_line = Some(end_line);
        new_nodes_data.push(end_node_data);
    }
    nodes.extend(new_nodes_data.clone());
    new_nodes_data
}

fn extract_control_flow_nodes(code_text: &str, nodes: &mut Vec<NodeData>) -> Vec<NodeData> {
    let mut new_nodes_data: Vec<NodeData> = Vec::new();
    new_nodes_data.extend(AST::extract_simple_loops(code_text));
    new_nodes_data.extend(AST::extract_while_loops(code_text));
    new_nodes_data.extend(AST::extract_for_loops(code_text));
    nodes.extend(new_nodes_data.clone());
    new_nodes_data
}

fn extract_simple_loops(code_text: &str) -> Vec<NodeData> {
    lazy_static! {
        static ref SIMPLE_LOOPS_PATTERN: Regex = Regex::new(r"(?im)^\s*(?P<Captureloop>\bloop\b)").unwrap();
    }
    let simpleloops_matches = SIMPLE_LOOPS_PATTERN.captures_iter(code_text);
    let mut new_nodes_data: Vec<NodeData> = Vec::new();

    for match_item in simpleloops_matches {
        let start_index = match_item.name("Captureloop").unwrap().start();
        let start_line = code_text[..start_index].lines().count() + 1;

        let mut node_data = NodeData::new("SimpleLoop".to_string(), "SimpleLoop".to_string(), Some(start_line), Some(start_index));
        new_nodes_data.push(node_data);

        let end_line = code_text[..match_item.get(0).unwrap().end()].lines().count() + 1;
        let end_index_val = match_item.get(0).unwrap().end();
        let mut end_node_data = NodeData::new("EndSimpleLoop".to_string(), "EndNode".to_string(), Some(end_line), Some(end_index_val)); // Specific end node name
        end_node_data.end_line = Some(end_line);
        new_nodes_data.push(end_node_data);
    }
    new_nodes_data
}

fn extract_while_loops(code_text: &str) -> Vec<NodeData> {
    lazy_static! {
        static ref WHILE_LOOPS_PATTERN: Regex = Regex::new(r"(?im)(?P<Capturewhile>\bwhile\b)\s*(?P<exitcond2>(\n|.)*?)\s*\bloop\b[^\n;]*").unwrap();
    }
    let whileloops_matches = WHILE_LOOPS_PATTERN.captures_iter(code_text);
    let mut new_nodes_data: Vec<NodeData> = Vec::new();

    for match_item in whileloops_matches {
        let start_index = match_item.name("Capturewhile").unwrap().start();
        let start_line = code_text[..start_index].lines().count() + 1;
        let condstring = match_item.name("exitcond2").map(|m| m.as_str().to_string()).unwrap_or_default();

        let mut node_data = NodeData::new("WhileLoop".to_string(), "WhileLoop".to_string(), Some(start_line), Some(start_index));
        // TODO: Condition parsing and storage - similar to Python's ConditionExpr
         if !condstring.is_empty() {
            let conditions = AST::parse_condition_expression(&condstring);
            node_data.conditions = Some(conditions);
         }
        new_nodes_data.push(node_data);

        let end_line = code_text[..match_item.get(0).unwrap().end()].lines().count() + 1;
        let end_index_val = match_item.get(0).unwrap().end();
        let mut end_node_data = NodeData::new("EndWhileLoop".to_string(), "EndNode".to_string(), Some(end_line), Some(end_index_val)); // Specific end node name
        end_node_data.end_line = Some(end_line);
        new_nodes_data.push(end_node_data);
    }
    new_nodes_data
}


fn extract_for_loops(code_text: &str) -> Vec<NodeData> {
    lazy_static! {
        static ref FOR_LOOPS_PATTERN: Regex = Regex::new(r"(?im)(?P<Capturefor>\bfor\b)\s*(?P<index>.*?)\s*\bin\b\s*(?:(?P<loop_direction>.*?))?\s*(?P<primavar>[^\s]*)\s*(?:(?=\brange\b)\brange\b\s*(?P<frst>(?:.|\n)*?)\s+\.\.\s*(?P<scnd>(?:.|\n)*?)\s+\bloop\b|(?:(?=\.\.)\.\.\s*(?P<range_end>.*?)\s*\bloop\b|\s*\bloop\b))").unwrap();
    }
    let forloops_matches = FOR_LOOPS_PATTERN.captures_iter(code_text);
    let mut new_nodes_data: Vec<NodeData> = Vec::new();

    for match_item in forloops_matches {
        let start_index = match_item.name("Capturefor").unwrap().start();
        let start_line = code_text[..start_index].lines().count() + 1;

        let mut node_data = NodeData::new("ForLoop".to_string(), "ForLoop".to_string(), Some(start_line), Some(start_index));
        node_data.iterator = match_item.name("index").map(|m| m.as_str().to_string());
        node_data.range_start = match_item.name("frst").map(|m| m.as_str().to_string()).filter(|_| match_item.name("range_end").is_some());
        node_data.range_var = match_item.name("primavar").map(|m| m.as_str().to_string()).filter(|_| match_item.name("range_end").is_none() && match_item.name("frst").is_none());
        node_data.iterator_type = match_item.name("primavar").map(|m| m.as_str().to_string()).filter(|_| match_item.name("frst").is_some());
        node_data.range_end = match_item.name("range_end").map(|m| m.as_str().to_string()).filter(|_| match_item.name("scnd").is_none());
        node_data.range_end = match_item.name("scnd").map(|m| m.as_str().to_string()).filter(|_| match_item.name("range_end").is_none()).or(node_data.range_end); // if range_end was already set, keep it, otherwise use scnd if available
        node_data.direction = match_item.name("loop_direction").map(|m| m.as_str().to_string()).or(Some("to".to_string()));


        new_nodes_data.push(node_data);

        let end_line = code_text[..match_item.get(0).unwrap().end()].lines().count() + 1;
        let end_index_val = match_item.get(0).unwrap().end();
        let mut end_node_data = NodeData::new("EndForLoop".to_string(), "EndNode".to_string(), Some(end_line), Some(end_index_val)); // Specific end node name
        end_node_data.end_line = Some(end_line);
        new_nodes_data.push(end_node_data);
    }
    new_nodes_data
}


fn extract_statement_nodes(code_text: &str, nodes: &mut Vec<NodeData>) -> Vec<NodeData> {
    let mut new_nodes_data: Vec<NodeData> = Vec::new();
    new_nodes_data.extend(AST::extract_if_statements(code_text));
    new_nodes_data.extend(AST::extract_case_statements(code_text));
    nodes.extend(new_nodes_data.clone());
    new_nodes_data
}

fn extract_if_statements(code_text: &str) -> Vec<NodeData> {
    lazy_static! {
        static ref IF_STATEMENTS_PATTERN: Regex = Regex::new(r"(?im)^\s*(?P<ifstat>\bif\b)(?P<Condition>(?:.|\n)*?)(?<!\band\b\s)then").unwrap();
    }
    let ifstatements_matches = IF_STATEMENTS_PATTERN.captures_iter(code_text);
    let mut new_nodes_data: Vec<NodeData> = Vec::new();

    for match_item in ifstatements_matches {
        let start_index = match_item.name("ifstat").unwrap().start();
        let start_line = code_text[..start_index].lines().count() + 1;
        let conditionsstring = match_item.name("Condition").map(|m| m.as_str().to_string()).unwrap_or_default();


        let mut node_data = NodeData::new("IfStatement".to_string(), "IfStatement".to_string(), Some(start_line), Some(start_index));
         if !conditionsstring.is_empty() {
            let conditions = AST::parse_condition_expression(&conditionsstring);
            node_data.conditions = Some(conditions);
         }
        new_nodes_data.push(node_data);

        let end_line = code_text[..match_item.get(0).unwrap().end()].lines().count() + 1;
        let end_index_val = match_item.get(0).unwrap().end();
        let mut end_node_data = NodeData::new("EndIfStatement".to_string(), "EndNode".to_string(), Some(end_line), Some(end_index_val)); // Specific end node name
        end_node_data.end_line = Some(end_line);
        new_nodes_data.push(end_node_data);
    }
    new_nodes_data
}

fn extract_case_statements(code_text: &str) -> Vec<NodeData> {
    lazy_static! {
        static ref CASE_STATEMENTS_PATTERN: Regex = Regex::new(r#"(?i)(?<!\s*end)(?:\"\s*|\'\s*)?(?<Casestmnt>\bcase\b)\s*(?<var>(?:.|\n)*?)\s*\bis\b(?:\s*\"|\s*\')?"#).unwrap();
    }
    let casestatements_matches = CASE_STATEMENTS_PATTERN.captures_iter(code_text);
    let mut new_nodes_data: Vec<NodeData> = Vec::new();

    for match_item in casestatements_matches {
        let start_index = match_item.name("Casestmnt").unwrap().start();
        let start_line = code_text[..start_index].lines().count() + 1;
        let switch_expression = match_item.name("var").map(|m| m.as_str().to_string()).unwrap_or_default();


        let mut node_data = NodeData::new("CaseStatement".to_string(), "CaseStatement".to_string(), Some(start_line), Some(start_index));
        node_data.switch_expression = Some(switch_expression);
        // Cases are extracted in post_extract, as body is needed. For now cases = None
        new_nodes_data.push(node_data);

        let end_line = code_text[..match_item.get(0).unwrap().end()].lines().count() + 1;
        let end_index_val = match_item.get(0).unwrap().end();
        let mut end_node_data = NodeData::new("EndCaseStatement".to_string(), "EndNode".to_string(), Some(end_line), Some(end_index_val)); // Specific end node name
        end_node_data.end_line = Some(end_line);
        new_nodes_data.push(end_node_data);
    }
    new_nodes_data
}

fn parse_condition_expression(condition_str: &str) -> ConditionExpr {
    let mut list = Vec::new();
    let albero = AST::supersplitter(condition_str.to_string(), &mut list); // Pass mut list
    ConditionExpr { list }
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


fn recursive_function(keyword_str: &str, condstring: String, index: usize, lst: &mut Vec<Expression>) -> Expression {
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


fn supersplitter(condstring_in: String, lst: &mut Vec<Expression>) -> Expression {
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
            AST::flag_setter(&condstring, index, char, string_flag, number_of_open_parenthesis, number_of_closed_parenthesis);
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
fn flag_setter(condstring: &str, index: usize, char: char, string_flag: i32, number_of_open_parenthesis: i32, number_of_closed_parenthesis: i32) -> (i32, i32, i32) {
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
fn flags_check(number_of_open_parenthesis: i32, number_of_closed_parenthesis: i32, string_flag: i32) -> bool {
    (number_of_open_parenthesis - number_of_closed_parenthesis == 0) && string_flag == 0
}

// Is parenthesis exterior function
fn is_parenthesis_exterior(expression: &str) -> bool {
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
fn is_expression_a_parenthesis(expression: &str) -> bool {
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


fn clean_code(raw_code: &str) -> String {
    let space_to_tab_ratio = 4;
    let ada_code_content = raw_code.replace("\t", &" ".repeat(space_to_tab_ratio));

    lazy_static! {
        static ref CLEAN_CODE_REGEX: Regex = Regex::new(r#""[^"]*"|--.*$"#).unwrap();
    }

    let cleaned_code = CLEAN_CODE_REGEX.replace_all(&ada_code_content, |caps: &regex::Captures| {
        if caps.get(0).unwrap().as_str().starts_with('"') {
            caps.get(0).unwrap().as_str().to_string()
        } else {
            " ".repeat(caps.get(0).unwrap().as_str().len())
        }
    });

    cleaned_code.split("\n")
        .collect::<Vec<&str>>()
        .join("\n")
}


fn extract_all_nodes(code_text: &str) -> Vec<NodeData> {
    let mut nodes: Vec<NodeData> = Vec::new();
    nodes.extend(AST::extract_packages(code_text));
    let mut temp_nodes_1: Vec<NodeData> = Vec::new();
    temp_nodes_1.extend(AST::extract_procedures_functions(code_text, &mut nodes));
    let mut temp_nodes_2: Vec<NodeData> = Vec::new();
    temp_nodes_2.extend(AST::extract_type_declarations(code_text, &mut nodes));
    let mut temp_nodes_3: Vec<NodeData> = Vec::new();
    temp_nodes_3.extend(AST::extract_declare_blocks(code_text, &mut nodes));
    let mut temp_nodes_4: Vec<NodeData> = Vec::new();
    temp_nodes_4.extend(AST::extract_control_flow_nodes(code_text, &mut nodes));
    let mut temp_nodes_5: Vec<NodeData> = Vec::new();
    temp_nodes_5.extend(AST::extract_statement_nodes(code_text, &mut nodes));

    nodes.extend(temp_nodes_1);
    nodes.extend(temp_nodes_2);
    nodes.extend(temp_nodes_3);
    nodes.extend(temp_nodes_4);
    nodes.extend(temp_nodes_5);
    nodes

}



}


fn main() {
    // Example Usage (replace with your actual node creation and data)
    let mut nodes_data = vec![
        NodeData::new("PackageSpec".to_string(), "PackageNode".to_string(), Some(1), Some(10)),
        NodeData::new("ProcedureSpec".to_string(), "ProcedureNode".to_string(), Some(2), Some(5)),
        NodeData::new("IfStatement".to_string(), "IfStatement".to_string(), Some(3), Some(4)),
        NodeData::new("EndProcedure".to_string(), "EndNode".to_string(), Some(5), None),
        NodeData::new("EndPackage".to_string(), "EndNode".to_string(), Some(10), None),
        NodeData::new("WhileLoop".to_string(), "WhileLoop".to_string(), Some(6), Some(9)),
        NodeData::new("EndWhileLoop".to_string(), "EndNode".to_string(), Some(9), None),

    ];

    let mut ast = AST::new(nodes_data);

    if let Err(e) = ast.build() {
        eprintln!("Error building AST: {}", e);
    } else {
        ast.print_tree();
        println!("\n--- Nodes Info ---");
        if let Err(e) = ast.print_nodes_info() {
             eprintln!("Error printing node info: {}", e);
        }
    }
}




