#[cfg(test)]
mod tests {
    use ADA_Standards::{AST, NodeData, Expression, ConditionExpr, UnaryExpression, BinaryExpression, MembershipExpression, Unaries, Binaries, Memberships};

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
        let expected_code = "Line 1\nLine 2           \nLine 3";
        assert_eq!(cleaned_code, expected_code, "Test Case: Carriage return and newline (CRLF)");
    }

     #[test]
    fn test_clean_code_only_comment_lines() {
        let raw_code = "-- This is a comment line 1\n-- This is comment line 2";
        let cleaned_code = AST::clean_code(raw_code);
        let expected_code = "                           \n                         ";
        assert_eq!(cleaned_code, expected_code, "Test Case: Only comment lines");
    }

}
