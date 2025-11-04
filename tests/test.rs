#[cfg(test)]
mod tests {
    use ADA_Standards::{AST, Expression, Unaries, Binaries, Memberships};

    #[test]
    fn test_parse_condition_expression_literal() {
        let condition_str = "true";
        let conditions = AST::parse_condition_expression(condition_str);
        //println!("{:?}",conditions);
        //println!("------------------------------------");
        //println!("{:?}",conditions.list);
        //AST::leggitree(&conditions.albero.unwrap(),0,"Root: ");
        assert_eq!(conditions.list.len(), 0, "Expecting 0 expressions in the list");
        
    }

    #[test]
    fn test_parse_condition_expression_unary_not() {
        let condition_str = "not Found";
        let conditions = AST::parse_condition_expression(condition_str);
        assert_eq!(conditions.list.len(), 1, "Expected one expression in the list");
        if let Some(Expression::Unary(unary_expr)) = conditions.list.get(0) {
            assert_eq!(unary_expr.op, Unaries::NOT, "Expected Unary operator NOT");
            if let Expression::Literal(operand_literal) = &*unary_expr.operand {
                assert_eq!(operand_literal, "Found", "Expected operand 'Found'");
            } else {
                panic!("Expected Literal operand, but got {:?}", unary_expr.operand);
            }
        } else {
            panic!("Expected Unary expression, but got {:?}", conditions.list.get(0));
        }
    }

    #[test]
    fn test_parse_condition_expression_binary_and() {
        let condition_str = "Arg > 10 and Arg < 20";
        let conditions = AST::parse_condition_expression(condition_str);
        //println!("{:?}",conditions);
        //println!("------------------------------------");
        //println!("{:?}",conditions.list);
        //AST::leggitree(&conditions.albero.unwrap(),0,"Root: ");
        assert_eq!(conditions.list.len(), 3, "Expected three expressions in the list");
        if let Some(Expression::Binary(binary_expr_left)) = conditions.list.get(0) {
            assert_eq!(binary_expr_left.op, Binaries::SUPERIOR, "Expected Binary operator SUPERIOR");
            if let Expression::Literal(left_operand) = &*binary_expr_left.left {
                assert_eq!(left_operand, "Arg", "Expected left operand Literal Arg");
                if let Expression::Literal(right_operand) = &*binary_expr_left.right {
                    assert_eq!(right_operand, "10", "Expected right operand '10'");
                } else {
                    panic!("Expected right operand, got {:?}", binary_expr_left.right);
                }
            } else {
                panic!("Expected left operand, got {:?}", binary_expr_left.left);
            }}else {
                panic!("Expected Binary expression, but got {:?}", conditions.list.get(0));
            }

        if let Some(Expression::Binary(binary_expr_right)) = conditions.list.get(1) {
            assert_eq!(binary_expr_right.op, Binaries::INFERIOR, "Expected Binary operator INFERIOR");
            if let Expression::Literal(left_operand) = &*binary_expr_right.left {
                assert_eq!(left_operand, "Arg", "Expected right Binary operator 'Arg'");
                if let Expression::Literal(right_operand) = &*binary_expr_right.right {
                assert_eq!(right_operand, "20", "Expected right-right operand '20'");
            } else {
                    panic!("Expected right operand, got {:?}", binary_expr_right.right);
                }
            } else {
                panic!("Expected left operand, got {:?}", binary_expr_right.left);
            }
        }else {
            panic!("Expected Binary expression, but got {:?}", conditions.list.get(1));
        }
         
    }

    #[test]
    fn test_parse_condition_expression_binary_or() {
        let condition_str = "A or B";
        let conditions = AST::parse_condition_expression(condition_str);
        assert_eq!(conditions.list.len(), 1, "Expected one expression in the list");
        if let Some(Expression::Binary(binary_expr)) = conditions.list.get(0) {
            assert_eq!(binary_expr.op, Binaries::OR, "Expected Binary operator OR");
            if let Expression::Literal(left_literal) = &*binary_expr.left {
                assert_eq!(left_literal, "A", "Expected left operand 'A'");
            } else {
                panic!("Expected Literal left operand, got {:?}", binary_expr.left);
            }
            if let Expression::Literal(right_literal) = &*binary_expr.right {
                assert_eq!(right_literal, "B", "Expected right operand 'B'");
            } else {
                panic!("Expected Literal right operand, got {:?}", binary_expr.right);
            }
        } else {
            panic!("Expected Binary expression, but got {:?}", conditions.list.get(0));
        }
    }

    #[test]
    fn test_parse_condition_expression_membership_in() {
        let condition_str = "X in Y";
        let conditions = AST::parse_condition_expression(condition_str);
        assert_eq!(conditions.list.len(), 1, "Expected one expression in the list");
        if let Some(Expression::Membership(membership_expr)) = conditions.list.get(0) {
            assert_eq!(membership_expr.op, Memberships::IN, "Expected Membership operator IN");
            if let Expression::Literal(left_literal) = &*membership_expr.left {
                assert_eq!(left_literal, "X", "Expected left operand 'X'");
            } else {
                panic!("Expected Literal left operand, got {:?}", membership_expr.left);
            }
            if let Expression::Literal(right_literal) = &*membership_expr.right {
                assert_eq!(right_literal, "Y", "Expected right operand 'Y'");
            } else {
                panic!("Expected Literal right operand, got {:?}", membership_expr.right);
            }
        } else {
            panic!("Expected Membership expression, but got {:?}", conditions.list.get(0));
        }
    }

    #[test]
    fn test_parse_condition_expression_membership_not_in() {
        let condition_str = "Z not in W";
        let conditions = AST::parse_condition_expression(condition_str);
        assert_eq!(conditions.list.len(), 1, "Expected one expression in the list");
        if let Some(Expression::Membership(membership_expr)) = conditions.list.get(0) {
            assert_eq!(membership_expr.op, Memberships::NOT_IN, "Expected Membership operator NOT_IN");
            if let Expression::Literal(left_literal) = &*membership_expr.left {
                assert_eq!(left_literal, "Z", "Expected left operand 'Z'");
            } else {
                panic!("Expected Literal left operand, got {:?}", membership_expr.left);
            }
            if let Expression::Literal(right_literal) = &*membership_expr.right {
                assert_eq!(right_literal, "W", "Expected right operand 'W'");
            } else {
                panic!("Expected Literal right operand, got {:?}", membership_expr.right);
            }
        } else {
            panic!("Expected Membership expression, but got {:?}", conditions.list.get(0));
        }
    }

    #[test]
    fn test_parse_condition_expression_complex_and_or_not() {
        let condition_str = "(Arg > 10 and Arg < 20) or else not Found";
        let conditions = AST::parse_condition_expression(condition_str);
        AST::leggitree(&conditions.albero.unwrap(),0,"Root: ");
        assert_eq!(conditions.list.len(), 5, "Expected 5 expressions in the list");

    // 1. Binary(BinaryExpression { op: SUPERIOR, left: Literal("Arg"), right: Literal("10"), condstring: "Arg > 10" })
    if let Some(Expression::Binary(expr1)) = conditions.list.get(0) {
        assert_eq!(expr1.op, Binaries::SUPERIOR, "Expr1: Expected operator SUPERIOR (>)");
        if let Expression::Literal(left_lit) = &*expr1.left {
            assert_eq!(left_lit, "Arg", "Expr1: Expected left operand 'Arg'");
        } else { panic!("Expr1: Left operand is not Literal"); }
        if let Expression::Literal(right_lit) = &*expr1.right {
            assert_eq!(right_lit, "10", "Expr1: Expected right operand '10'");
        } else { panic!("Expr1: Right operand is not Literal"); }
        assert_eq!(expr1.condstring, "Arg > 10", "Expr1: Expected condstring 'Arg > 10'");
    } else { panic!("Expr1: Not a Binary expression"); }

    // 2. Binary(BinaryExpression { op: INFERIOR, left: Literal("Arg"), right: Literal("20"), condstring: "Arg < 20" })
    if let Some(Expression::Binary(expr2)) = conditions.list.get(1) {
        assert_eq!(expr2.op, Binaries::INFERIOR, "Expr2: Expected operator INFERIOR (<)");
        if let Expression::Literal(left_lit) = &*expr2.left {
            assert_eq!(left_lit, "Arg", "Expr2: Expected left operand 'Arg'");
        } else { panic!("Expr2: Left operand is not Literal"); }
        if let Expression::Literal(right_lit) = &*expr2.right {
            assert_eq!(right_lit, "20", "Expr2: Expected right operand '20'");
        } else { panic!("Expr2: Right operand is not Literal"); }
        assert_eq!(expr2.condstring, "Arg < 20", "Expr2: Expected condstring 'Arg < 20'");
    } else { panic!("Expr2: Not a Binary expression"); }

    // 3. Binary(BinaryExpression { op: AND, ... , condstring: "Arg > 10 and Arg < 20" }) - Nested Binary
    if let Some(Expression::Binary(expr3)) = conditions.list.get(2) {
        assert_eq!(expr3.op, Binaries::AND, "Expr3: Expected operator AND");
        assert_eq!(expr3.condstring, "Arg > 10 and Arg < 20", "Expr3: Expected condstring 'Arg > 10 and Arg < 20'");

        // Left of AND (Expr1): Binary(BinaryExpression { op: SUPERIOR, ... })
        if let Expression::Binary(left_bin_expr) = &*expr3.left {
            assert_eq!(left_bin_expr.op, Binaries::SUPERIOR, "Expr3 Left: Expected operator SUPERIOR (>)");
             if let Expression::Literal(left_lit) = &*left_bin_expr.left {
                assert_eq!(left_lit, "Arg", "Expr3 Left: Expected left operand 'Arg'");
            } else { panic!("Expr3 Left: Left operand is not Literal"); }
            if let Expression::Literal(right_lit) = &*left_bin_expr.right {
                assert_eq!(right_lit, "10", "Expr3 Left: Expected right operand '10'");
            } else { panic!("Expr3 Left: Right operand is not Literal"); }
            assert_eq!(left_bin_expr.condstring, "Arg > 10", "Expr3 Left: Expected condstring 'Arg > 10'");
        } else { panic!("Expr3: Left is not a Binary expression"); }

        // Right of AND (Expr2): Binary(BinaryExpression { op: INFERIOR, ... })
        if let Expression::Binary(right_bin_expr) = &*expr3.right {
            assert_eq!(right_bin_expr.op, Binaries::INFERIOR, "Expr3 Right: Expected operator INFERIOR (<)");
             if let Expression::Literal(left_lit) = &*right_bin_expr.left {
                assert_eq!(left_lit, "Arg", "Expr3 Right: Expected left operand 'Arg'");
            } else { panic!("Expr3 Right: Left operand is not Literal"); }
            if let Expression::Literal(right_lit) = &*right_bin_expr.right {
                assert_eq!(right_lit, "20", "Expr3 Right: Expected right operand '20'");
            } else { panic!("Expr3 Right: Right operand is not Literal"); }
             assert_eq!(right_bin_expr.condstring, "Arg < 20", "Expr3 Right: Expected condstring 'Arg < 20'");
        } else { panic!("Expr3: Right is not a Binary expression"); }

    } else { panic!("Expr3: Not a Binary expression"); }

    // 4. Unary(UnaryExpression { op: NOT, operand: Literal("Found"), condstring: "not Found" })
    if let Some(Expression::Unary(expr4)) = conditions.list.get(3) {
        assert_eq!(expr4.op, Unaries::NOT, "Expr4: Expected operator NOT");
        if let Expression::Literal(operand_lit) = &*expr4.operand {
            assert_eq!(operand_lit, "Found", "Expr4: Expected operand 'Found'");
        } else { panic!("Expr4: Operand is not Literal"); }
        assert_eq!(expr4.condstring, "not Found", "Expr4: Expected condstring 'not Found'");
    } else { panic!("Expr4: Not a Unary expression"); }

    // 5. Binary(BinaryExpression { op: OR_ELSE, ... , condstring: "(Arg > 10 and Arg < 20) or else not Found" }) - Outer Binary
    if let Some(Expression::Binary(expr5)) = conditions.list.get(4) {
        assert_eq!(expr5.op, Binaries::OR_ELSE, "Expr5: Expected operator OR_ELSE");
        assert_eq!(expr5.condstring, "(Arg > 10 and Arg < 20) or else not Found", "Expr5: Expected condstring '(Arg > 10 and Arg < 20) or else not Found'");

        // Left of OR_ELSE (Expr3): Binary(BinaryExpression { op: AND, ... })
        if let Expression::Binary(left_bin_expr) = &*expr5.left {
             assert_eq!(left_bin_expr.op, Binaries::AND, "Expr5 Left: Expected operator AND");
             assert_eq!(left_bin_expr.condstring, "Arg > 10 and Arg < 20", "Expr5 Left: Expected condstring 'Arg > 10 and Arg < 20'");
             // We don't need to re-assert the deeper structure of Expr3's left and right, as Expr3 test already covers it

        } else { panic!("Expr5: Left is not a Binary expression"); }

        // Right of OR_ELSE (Expr4): Unary(UnaryExpression { op: NOT, ... })
        if let Expression::Unary(right_unary_expr) = &*expr5.right {
            assert_eq!(right_unary_expr.op, Unaries::NOT, "Expr5 Right: Expected operator NOT");
            assert_eq!(right_unary_expr.condstring, "not Found", "Expr5 Right: Expected condstring 'not Found'");
            // We don't need to re-assert the deeper structure of Expr4, as Expr4 test already covers it
        } else { panic!("Expr5: Right is not a Unary expression"); }

    } else { panic!("Expr5: Not a Binary expression"); }
    }


    #[test]
    fn test_clean_code_no_tabs_no_comments_no_strings() {
        let raw_code = r#"procedure My_Procedure is
begin
    null;
end My_Procedure;"#;
        let cleaned_code = AST::clean_code(raw_code);
        let expected_code = r#"procedure My_Procedure is
begin
    null;
end My_Procedure;"#;
        assert_eq!(cleaned_code, expected_code, "Test Case: No tabs, no comments, no strings");
    }

    #[test]
    fn test_clean_code_tabs_replaced_with_spaces() {
        let raw_code = r#"procedure My_Procedure is
begin
	null;
end My_Procedure;"#;
        let cleaned_code = AST::clean_code(raw_code);
        let expected_code = r#"procedure My_Procedure is
begin
    null;
end My_Procedure;"#;
        assert_eq!(cleaned_code, expected_code, "Test Case: Tabs replaced with spaces");
    }

    #[test]
    fn test_clean_code_single_line_comments_replaced_with_spaces() {
        let raw_code = r#"procedure My_Procedure is -- This is a comment
begin
    null; -- Another comment here
end My_Procedure;"#;
        let cleaned_code = AST::clean_code(raw_code);
        let expected_code = r#"procedure My_Procedure is                     
begin
    null;                        
end My_Procedure;"#;
        assert_eq!(cleaned_code, expected_code, "Test Case: Single-line comments replaced with spaces");
    }

    #[test]
    fn test_clean_code_string_literals_preserved() {
        let raw_code = r#"procedure My_Procedure is
    Message : String := "Hello, World!";
begin
    Put_Line (Message);
end My_Procedure;"#;
        let cleaned_code = AST::clean_code(raw_code);
        let expected_code = r#"procedure My_Procedure is
    Message : String := "Hello, World!";
begin
    Put_Line (Message);
end My_Procedure;"#;
        assert_eq!(cleaned_code, expected_code, "Test Case: String literals preserved");
    }

    #[test]
    fn test_clean_code_comments_and_strings_combined() {
        let raw_code = r#"procedure My_Procedure is -- Comment with "string inside"
    Message : String := "String with -- comment inside"; -- End of line comment
begin
    Put_Line ("Another string"); -- Comment after string
end My_Procedure;"#;
        let cleaned_code = AST::clean_code(raw_code);
        let expected_code = r#"procedure My_Procedure is                                
    Message : String := "String with -- comment inside";                       
begin
    Put_Line ("Another string");                        
end My_Procedure;"#;
        assert_eq!(cleaned_code, expected_code, "Test Case: Comments and strings combined");
    }

    #[test]
    fn test_clean_code_empty_input() {
        let raw_code = "";
        let cleaned_code = AST::clean_code(raw_code);
        let expected_code = "";
        assert_eq!(cleaned_code, expected_code, "Test Case: Empty input");
    }

    #[test]
    fn test_clean_code_multiple_lines_complex() {
        let raw_code = r#"procedure Complex_Code is
	-- Multi-line comment start
	-- This is line 1 of comment
	-- This is line 2 of comment
    Message1 : String := "String 1 with tab\t and -- comment"; -- Comment after string 1
    Message2 : String := "String 2"; -- Another comment
begin
    Put_Line (Message1); -- Inline comment
    Put_Line (Message2);
end Complex_Code; -- End procedure comment"#;
        let cleaned_code = AST::clean_code(raw_code);
        let expected_code = r#"procedure Complex_Code is
                               
                                
                                
    Message1 : String := "String 1 with tab\t and -- comment";                          
    Message2 : String := "String 2";                   
begin
    Put_Line (Message1);                  
    Put_Line (Message2);
end Complex_Code;                         "#;
        assert_eq!(cleaned_code, expected_code, "Test Case: Multiple lines, complex");
    }

    #[test]
    fn test_clean_code_carriage_return_newline() {
        let raw_code = "Line 1\r\nLine 2 -- comment\r\nLine 3";
        let cleaned_code = AST::clean_code(raw_code);
        let expected_code = "Line 1\r\nLine 2            \nLine 3";
        assert_eq!(cleaned_code, expected_code, "Test Case: Carriage return and newline (CRLF)");
    }

     #[test]
    fn test_clean_code_only_comment_lines() {
        let raw_code = "-- This is a comment line 1\n-- This is comment line 2";
        let cleaned_code = AST::clean_code(raw_code);
        let expected_code = "                           \n                         ";
        assert_eq!(cleaned_code, expected_code, "Test Case: Only comment lines");
    }

    /// Tests the full end-to-end process:
    /// 1. `clean_code`
    /// 2. `extract_all_nodes`
    /// 3. `build` (which runs `associate_end_lines_in_arena`)
    /// 4. `populate_...` functions
    /// It then verifies the final tree structure and node metadata.
    #[test]
    fn test_full_ast_build_and_nesting() {
        let raw_code = r#"
    package My_Pkg is
    procedure Do_Nothing; -- spec
    end My_Pkg;

    package body My_Pkg is
    procedure Do_Nothing is
    begin -- procedure body
        loop
        exit when True;
        end loop;
    end Do_Nothing;
    end My_Pkg;
    "#;
        let cleaned_code = AST::clean_code(raw_code);
        let nodes = AST::extract_all_nodes(&cleaned_code).unwrap();
        
        let mut ast = AST::new(nodes);
        ast.build(&cleaned_code).unwrap();
        ast.populate_simple_loop_conditions(&cleaned_code).unwrap();
        ast.populate_cases(&cleaned_code).unwrap();

        // 1. Find the Package Spec
        let pkg_spec_id = ast.find_node_by_name_and_type("My_Pkg", "PackageNode")
            .expect("Could not find PackageNode 'My_Pkg'");
        let pkg_spec_node = ast.arena.get(pkg_spec_id).unwrap().get();
        
        assert_eq!(pkg_spec_node.is_body, Some(false));
        assert_eq!(pkg_spec_node.start_line, Some(2));
        assert_eq!(pkg_spec_node.end_line, Some(4)); // Check end line association

        // 2. Find the Package Body
        // We need to find the *second* "My_Pkg" PackageNode.
        let pkg_body_id = ast.root_id.descendants(&ast.arena).filter(|&id| {
            let node = ast.arena.get(id).unwrap().get();
            node.name == "My_Pkg" && node.node_type == "PackageNode"
        }).nth(1).expect("Could not find PackageNode 'My_Pkg' (body)");
        
        let pkg_body_node = ast.arena.get(pkg_body_id).unwrap().get();
        assert_eq!(pkg_body_node.is_body, Some(true));
        assert_eq!(pkg_body_node.start_line, Some(6));
        assert_eq!(pkg_body_node.end_line, Some(13));

        // 3. Find the Procedure Spec (child of Package Spec)
        let proc_spec_id = ast.find_node_by_name_and_type("Do_Nothing", "ProcedureNode")
            .expect("Could not find ProcedureNode 'Do_Nothing' (spec)");
        let proc_spec_node = ast.arena.get(proc_spec_id).unwrap().get();
        
        assert_eq!(proc_spec_node.is_body, Some(false));
        assert_eq!(proc_spec_id.ancestors(&ast.arena).any(|id| id == pkg_spec_id), true, "Proc spec should be child of Pkg spec");

        // 4. Find the Simple Loop (grandchild of Package Body)
        let loop_id = ast.find_node_by_name_and_type("SimpleLoop", "SimpleLoop")
            .expect("Could not find SimpleLoop");
        let loop_node = ast.arena.get(loop_id).unwrap().get();

        assert_eq!(loop_id.ancestors(&ast.arena).any(|id| id == pkg_body_id), true, "Loop should be child of Pkg body");
        
        // 5. Check populated loop conditions
        let conds = loop_node.conditions.as_ref().expect("Loop conditions not populated");
        assert!(matches!(conds.albero.as_deref(), Some(Expression::Literal(_))));
        if let Some(Expression::Literal(lit)) = conds.albero.as_ref().map(|b| b.as_ref()) {
            assert_eq!(lit.trim(), "True");
        }
    }

    #[test]

    fn test_extractors_ignore_strings_and_comments() {
    let raw_code = r#"
    procedure Test_Strings is
    S1 : String := "this is a declare block";
    S2 : String := "this is a case My_Var is";
    S3 : String := "this is an if X then";
    S4 : String := "this is a loop";
    begin
    -- This is the only real one
    declare
        X : Integer;
    begin
        null;
    end;
    end Test_Strings;
    "#;
    let cleaned_code = AST::clean_code(raw_code);
    let nodes = AST::extract_all_nodes(&cleaned_code).unwrap();

    // --- FIX IS HERE ---
    // The test must account for the 5 variable declarations now being found.
    // 1 (Procedure) + 1 (Declare) + 5 (Variables) = 7
    let expected_node_count = 7;
    assert_eq!(nodes.len(), expected_node_count, "Should find 7 nodes (1 proc, 1 declare, 5 vars)");
    // --- END FIX ---

    // Check that our string-skipping logic worked
    let case_nodes = nodes.iter().filter(|n| n.node_type == "CaseStatement").count();
    let if_nodes = nodes.iter().filter(|n| n.node_type == "IfStatement").count();
    let loop_nodes = nodes.iter().filter(|n| n.node_type == "SimpleLoop").count();
    let declare_nodes = nodes.iter().filter(|n| n.node_type == "DeclareNode").count();

    // --- ADD THIS FIX ---
    let variable_nodes = nodes.iter().filter(|n| n.node_type == "VariableDeclaration").count();
    assert_eq!(variable_nodes, 5, "Should find all 5 variable declarations");
    // --- END ADD ---

    assert_eq!(case_nodes, 0, "Should not find 'case' in string");
    assert_eq!(if_nodes, 0, "Should not find 'if' in string");
    assert_eq!(loop_nodes, 0, "Should not find 'loop' in string");
    assert_eq!(declare_nodes, 1, "Should find the one real 'declare' block");

    let declare_node = nodes.iter().find(|n| n.node_type == "DeclareNode").unwrap();
    assert_eq!(declare_node.start_line, Some(9)); // Check line number
    }

    #[test]
    fn test_parse_condition_ada_attributes() {
        let condition_str = "My_Array'Length > 0";
        let conditions = AST::parse_condition_expression(condition_str);
        
        // Check the root node
        let root = conditions.albero.as_ref().expect("AST 'albero' is None");
        if let Expression::Binary(bin_expr) = root.as_ref() {
            assert_eq!(bin_expr.op, Binaries::SUPERIOR);
            
            // Check left operand
            if let Expression::Literal(left) = &*bin_expr.left {
                assert_eq!(left, "My_Array'Length");
            } else {
                panic!("Left operand was not a Literal");
            }
            
            // Check right operand
            if let Expression::Literal(right) = &*bin_expr.right {
                assert_eq!(right, "0");
            } else {
                panic!("Right operand was not a Literal");
            }
        } else {
            panic!("Root expression is not a BinaryExpression");
        }
    }

    #[test]
    fn test_parse_condition_function_call() {
        // Your parser treats function calls as literals, which is fine.
        // This test just confirms that behavior.
        let condition_str = "My_Func(A, B) = True";
        let conditions = AST::parse_condition_expression(condition_str);

        let root = conditions.albero.as_ref().expect("AST 'albero' is None");
        if let Expression::Binary(bin_expr) = root.as_ref() {
            assert_eq!(bin_expr.op, Binaries::EQUAL);
            if let Expression::Literal(left) = &*bin_expr.left {
                assert_eq!(left, "My_Func(A, B)");
            } else {
                panic!("Left operand (function call) was not a Literal");
            }
        } else {
            panic!("Root expression is not a BinaryExpression");
        }
    }

    use std::fs;
    use std::collections::HashMap;

    /// This is the main integration test.
    /// It parses the entire `blop.ada` file and verifies the counts
    /// of all node types and the final tree structure.
    #[test]
    fn test_full_integration_on_blob_ada() {
        let raw_code = fs::read_to_string("blop.ada")
            .expect("Failed to read blop.ada. Make sure it's in the project root.");
        
        let cleaned_code = AST::clean_code(&raw_code);
        let nodes = AST::extract_all_nodes(&cleaned_code).unwrap();
        
        let mut ast = AST::new(nodes);
        ast.build(&cleaned_code).unwrap();
        ast.populate_cases(&cleaned_code).unwrap();
        ast.populate_simple_loop_conditions(&cleaned_code).unwrap();

        // --- Store all nodes for easy searching ---
        let mut node_counts: HashMap<String, usize> = HashMap::new();
        let mut all_node_ids = Vec::new();
        for node_id in ast.root_id.descendants(&ast.arena) {
            all_node_ids.push(node_id); // Store the ID
            let node = ast.arena.get(node_id).unwrap().get();
            *node_counts.entry(node.node_type.clone()).or_insert(0) += 1;
        }
        *node_counts.entry("RootNode".to_string()).or_insert(0) -= 1;

        // --- All counts are now corrected ---
        assert_eq!(*node_counts.get("PackageNode").unwrap_or(&0), 2, "Expected 2 PackageNodes");
        assert_eq!(*node_counts.get("ProcedureNode").unwrap_or(&0), 10, "Expected 10 ProcedureNodes");
        assert_eq!(*node_counts.get("FunctionNode").unwrap_or(&0), 7, "Expected 7 FunctionNodes");
        assert_eq!(*node_counts.get("TypeDeclaration").unwrap_or(&0), 5, "Expected 5 TypeDeclarations");
        assert_eq!(*node_counts.get("SimpleLoop").unwrap_or(&0), 3, "Expected 3 SimpleLoops");
        assert_eq!(*node_counts.get("WhileLoop").unwrap_or(&0), 9, "Expected 9 WhileLoops");
        assert_eq!(*node_counts.get("ForLoop").unwrap_or(&0), 4, "Expected 4 ForLoops");
        assert_eq!(*node_counts.get("IfStatement").unwrap_or(&0), 9, "Expected 9 IfStatements");
        assert_eq!(*node_counts.get("ElsifStatement").unwrap_or(&0), 1, "Expected 1 ElsifStatement");
        assert_eq!(*node_counts.get("ElseStatement").unwrap_or(&0), 2, "Expected 2 ElseStatements");
        assert_eq!(*node_counts.get("CaseStatement").unwrap_or(&0), 2, "Expected 2 CaseStatements");
        assert_eq!(*node_counts.get("TaskNode").unwrap_or(&0), 2, "Expected 2 TaskNodes");
        assert_eq!(*node_counts.get("EntryNode").unwrap_or(&0), 2, "Expected 2 EntryNodes");
        assert_eq!(*node_counts.get("VariableDeclaration").unwrap_or(&0), 19, "Expected 19 VariableDeclarations");
        assert_eq!(*node_counts.get("DeclareNode").unwrap_or(&0), 0, "Expected 0 DeclareNodes");

        // --- VERIFY TREE STRUCTURE (Corrected Find Logic) ---
        
        let estocazz_id = all_node_ids.iter().find(|&&id| {
            let node = ast.arena.get(id).unwrap().get();
            node.name == "estocazz" && node.node_type == "ProcedureNode"
        }).expect("Failed to find 'estocazz' procedure").clone();

        // --- FIX: Find the 'case Day' node by its unique switch_expression ---
        let case_day_id = all_node_ids.iter().find(|&&id| {
            let node = ast.arena.get(id).unwrap().get();
            node.node_type == "CaseStatement" && node.switch_expression == Some("Day".to_string())
        }).expect("Failed to find 'case Day is'").clone();
        
        let case_day_node = ast.arena.get(case_day_id).unwrap().get();

        assert_eq!(case_day_node.cases.as_ref().unwrap().len(), 8, "Expected 8 'when' clauses in 'case Day'");
        assert_eq!(case_day_node.start_line, Some(162)); // This assertion will now pass
        assert_eq!(case_day_node.end_line, Some(179));
        
        assert_eq!(
            ast.arena.get(case_day_id).unwrap().parent(), 
            Some(estocazz_id),
            "'case Day' should be a child of 'estocazz'"
        );

        // --- FIX: Find the 'if Count = 3' node by its start line ---
        let if_count_id = all_node_ids.iter().find(|&&id| {
            let node = ast.arena.get(id).unwrap().get();
            node.node_type == "IfStatement" && node.start_line == Some(180)
        }).expect("Failed to find 'if Count = 3'").clone();

        // Find the `elsif` statement
        let elsif_id = all_node_ids.iter().find(|&&id| {
            let node = ast.arena.get(id).unwrap().get();
            node.node_type == "ElsifStatement"
        }).expect("Failed to find 'elsif' statement").clone();
        
        let elsif_node = ast.arena.get(elsif_id).unwrap().get();
        assert_eq!(elsif_node.start_line, Some(182));
        
        assert_eq!(
            ast.arena.get(elsif_id).unwrap().parent(),
            Some(if_count_id),
            "'elsif' should be a child of 'if'"
        );
    }



}
