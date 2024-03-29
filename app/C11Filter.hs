module C11Filter(canSearch) where

import CFG
import Token
import AVL

import Prelude hiding (EQ,LT,GT)

canSearch :: String -> Bool
canSearch =
  let avl =
        foldr (\x t -> ins t x)
          (foldr (\x t -> ins t x) Nil c11_filter_nonterminals)
            c11_filter_terminals
  in \s -> find s avl 

c11_filter_nonterminals =
  map (\(ProductionRule lhs _) -> lhs)
     [
      ProductionRule "translation_unit_file'" [Nonterminal "translation_unit_file"],
      ProductionRule "typedef_name" [Terminal "NAME", Terminal "TYPE"],
      ProductionRule "var_name" [Terminal "NAME", Terminal "VARIABLE"],
      ProductionRule "typedef_name_spec" [Nonterminal "typedef_name"],
      ProductionRule "general_identifier" [Nonterminal "typedef_name"],
      ProductionRule "general_identifier" [Nonterminal "var_name"],
      ProductionRule "save_context" [],
      ProductionRule "scoped_parameter_type_list" [Nonterminal "save_context", Nonterminal "parameter_type_list"],
      ProductionRule "scoped_compound_statement" [Nonterminal "save_context", Nonterminal "compound_statement"],
      ProductionRule "scoped_selection_statement" [Nonterminal "save_context", Nonterminal "selection_statement"],
      ProductionRule "scoped_iteration_statement" [Nonterminal "save_context", Nonterminal "iteration_statement"],
      ProductionRule "scoped_statement" [Nonterminal "save_context", Nonterminal "statement"],
      ProductionRule "scoped_parameter_type_list" [Nonterminal "save_context", Nonterminal "parameter_type_list"],
      ProductionRule "declarator_varname" [Nonterminal "declarator"],
      ProductionRule "declarator_typedefname" [Nonterminal "declarator"],
      ProductionRule "primary_expression" [Nonterminal "var_name"],
      ProductionRule "primary_expression" [Terminal "CONSTANT"],
      ProductionRule "primary_expression" [Terminal "STRING_LITERAL"],
      ProductionRule "primary_expression" [Terminal "(", Nonterminal "expression", Terminal ")"],
      ProductionRule "primary_expression" [Nonterminal "generic_selection"],
      -- ProductionRule "generic_selection" [Terminal "_Generic", Terminal "(", Nonterminal "assignment_expression", Terminal ",", Nonterminal "generic_assoc_list", Terminal ")"],
      -- ProductionRule "generic_assoc_list" [Nonterminal "generic_association"],
      -- ProductionRule "generic_assoc_list" [Nonterminal "generic_assoc_list", Terminal ",", Nonterminal "generic_association"],
      -- ProductionRule "generic_association" [Nonterminal "type_name", Terminal ":", Nonterminal "assignment_expression"],
      -- ProductionRule "generic_association" [Terminal "default", Terminal ":", Nonterminal "assignment_expression"],
      ProductionRule "postfix_expression" [Nonterminal "primary_expression"],
      ProductionRule "postfix_expression" [Nonterminal "postfix_expression", Terminal "[", Nonterminal "expression", Terminal "]"],
      ProductionRule "postfix_expression" [Nonterminal "postfix_expression", Terminal "(", Nonterminal "option_argument_expression_list", Terminal ")"],
      ProductionRule "postfix_expression" [Nonterminal "postfix_expression", Terminal ".", Nonterminal "general_identifier"],
      ProductionRule "postfix_expression" [Nonterminal "postfix_expression", Terminal "->", Nonterminal "general_identifier"],
      ProductionRule "postfix_expression" [Nonterminal "postfix_expression", Terminal "++"],
      ProductionRule "postfix_expression" [Nonterminal "postfix_expression", Terminal "--"],
      ProductionRule "postfix_expression" [Terminal "(", Nonterminal "type_name", Terminal ")", Terminal "{", Nonterminal "initializer_list", Nonterminal "option_comma", Terminal "}"],
      ProductionRule "argument_expression_list" [Nonterminal "assignment_expression"],
      ProductionRule "argument_expression_list" [Nonterminal "argument_expression_list", Terminal ",", Nonterminal "assignment_expression"],
      ProductionRule "unary_expression" [Nonterminal "postfix_expression"],
      ProductionRule "unary_expression" [Terminal "++", Nonterminal "unary_expression"],
      ProductionRule "unary_expression" [Terminal "--", Nonterminal "unary_expression"],
      ProductionRule "unary_expression" [Nonterminal "unary_operator", Nonterminal "cast_expression"],
      ProductionRule "unary_expression" [Terminal "sizeof", Nonterminal "unary_expression"],
      ProductionRule "unary_expression" [Terminal "sizeof", Terminal "(", Nonterminal "type_name", Terminal ")"],
      ProductionRule "unary_expression" [Terminal "_Alignof", Terminal "(", Nonterminal "type_name", Terminal ")"],
      ProductionRule "unary_operator" [Terminal "&"],
      ProductionRule "unary_operator" [Terminal "*"],
      ProductionRule "unary_operator" [Terminal "+"],
      ProductionRule "unary_operator" [Terminal "-"],
      ProductionRule "unary_operator" [Terminal "~"],
      ProductionRule "unary_operator" [Terminal "!"],
      ProductionRule "cast_expression" [Nonterminal "unary_expression"],
      ProductionRule "cast_expression" [Terminal "(", Nonterminal "type_name", Terminal ")", Nonterminal "cast_expression"],
      ProductionRule "multiplicative_operator" [Terminal "*"],
      ProductionRule "multiplicative_operator" [Terminal "/"],
      ProductionRule "multiplicative_operator" [Terminal "%"],
      ProductionRule "multiplicative_expression" [Nonterminal "cast_expression"],
      ProductionRule "multiplicative_expression" [Nonterminal "multiplicative_expression", Nonterminal "multiplicative_operator", Nonterminal "cast_expression"],
      ProductionRule "additive_operator" [Terminal "+"],
      ProductionRule "additive_operator" [Terminal "-"],
      ProductionRule "additive_expression" [Nonterminal "multiplicative_expression"],
      ProductionRule "additive_expression" [Nonterminal "additive_expression", Nonterminal "additive_operator", Nonterminal "multiplicative_expression"],
      ProductionRule "shift_operator" [Terminal "<<"],
      ProductionRule "shift_operator" [Terminal ">>"],
      ProductionRule "shift_expression" [Nonterminal "additive_expression"],
      ProductionRule "shift_expression" [Nonterminal "shift_expression", Nonterminal "shift_operator", Nonterminal "additive_expression"],
      ProductionRule "relational_operator" [Terminal "<"],
      ProductionRule "relational_operator" [Terminal ">"],
      ProductionRule "relational_operator" [Terminal "<="],
      ProductionRule "relational_operator" [Terminal ">="],
      ProductionRule "relational_expression" [Nonterminal "shift_expression"],
      ProductionRule "relational_expression" [Nonterminal "relational_expression", Nonterminal "relational_operator", Nonterminal "shift_expression"],
      ProductionRule "equality_operator" [Terminal "=="],
      ProductionRule "equality_operator" [Terminal "!="],
      ProductionRule "equality_expression" [Nonterminal "relational_expression"],
      ProductionRule "equality_expression" [Nonterminal "equality_expression", Nonterminal "equality_operator", Nonterminal "relational_expression"],
      ProductionRule "and_expression" [Nonterminal "equality_expression"],
      ProductionRule "and_expression" [Nonterminal "and_expression", Terminal "&", Nonterminal "equality_expression"],
      ProductionRule "exclusive_or_expression" [Nonterminal "and_expression"],
      ProductionRule "exclusive_or_expression" [Nonterminal "exclusive_or_expression", Terminal "^", Nonterminal "and_expression"],
      ProductionRule "inclusive_or_expression" [Nonterminal "exclusive_or_expression"],
      ProductionRule "inclusive_or_expression" [Nonterminal "inclusive_or_expression", Terminal "|", Nonterminal "exclusive_or_expression"],
      ProductionRule "logical_and_expression" [Nonterminal "inclusive_or_expression"],
      ProductionRule "logical_and_expression" [Nonterminal "logical_and_expression", Terminal "&&", Nonterminal "inclusive_or_expression"],
      ProductionRule "logical_or_expression" [Nonterminal "logical_and_expression"],
      ProductionRule "logical_or_expression" [Nonterminal "logical_or_expression", Terminal "||", Nonterminal "logical_and_expression"],
      ProductionRule "conditional_expression" [Nonterminal "logical_or_expression"],
      ProductionRule "conditional_expression" [Nonterminal "logical_or_expression", Terminal "?", Nonterminal "expression", Terminal ":", Nonterminal "conditional_expression"],
      ProductionRule "assignment_expression" [Nonterminal "conditional_expression"],
      ProductionRule "assignment_expression" [Nonterminal "unary_expression", Nonterminal "assignment_operator", Nonterminal "assignment_expression"],
      ProductionRule "assignment_operator" [Terminal "="],
      ProductionRule "assignment_operator" [Terminal "*="],
      ProductionRule "assignment_operator" [Terminal "/="],
      ProductionRule "assignment_operator" [Terminal "%="],
      ProductionRule "assignment_operator" [Terminal "+="],
      ProductionRule "assignment_operator" [Terminal "-="],
      ProductionRule "assignment_operator" [Terminal "<<="],
      ProductionRule "assignment_operator" [Terminal ">>="],
      ProductionRule "assignment_operator" [Terminal "&="],
      ProductionRule "assignment_operator" [Terminal "^="],
      ProductionRule "assignment_operator" [Terminal "|="],
      ProductionRule "expression" [Nonterminal "assignment_expression"],
      ProductionRule "expression" [Nonterminal "expression", Terminal ",", Nonterminal "assignment_expression"],
      ProductionRule "constant_expression" [Nonterminal "conditional_expression"],
      ProductionRule "declaration" [Nonterminal "declaration_specifiers", Nonterminal "option_init_declarator_list_declarator_varname", Terminal ";"],
      ProductionRule "declaration" [Nonterminal "declaration_specifiers_typedef", Nonterminal "option_init_declarator_list_declarator_typedefname", Terminal ";"],
      ProductionRule "declaration" [Nonterminal "static_assert_declaration"],
      ProductionRule "declaration_specifier" [Nonterminal "storage_class_specifier"],
      ProductionRule "declaration_specifier" [Nonterminal "type_qualifier"],
      ProductionRule "declaration_specifier" [Nonterminal "function_specifier"],
      ProductionRule "declaration_specifier" [Nonterminal "alignment_specifier"],
      ProductionRule "declaration_specifiers" [Nonterminal "list_eq1_type_specifier_unique_declaration_specifier"],
      ProductionRule "declaration_specifiers" [Nonterminal "list_ge1_type_specifier_nonunique_declaration_specifier"],
      ProductionRule "declaration_specifiers_typedef" [Nonterminal "list_eq1_eq1_typedef_type_specifier_unique_declaration_specifier"],
      ProductionRule "declaration_specifiers_typedef" [Nonterminal "list_eq1_ge1_typedef_type_specifier_nonunique_declaration_specifier"],
      ProductionRule "init_declarator_list_declarator_varname" [Nonterminal "init_declarator_declarator_varname"],
      ProductionRule "init_declarator_list_declarator_varname" [Nonterminal "init_declarator_list_declarator_varname", Terminal ",", Nonterminal "init_declarator_declarator_varname"],
      ProductionRule "init_declarator_list_declarator_typedefname" [Nonterminal "init_declarator_declarator_typedefname"],
      ProductionRule "init_declarator_list_declarator_typedefname" [Nonterminal "init_declarator_list_declarator_typedefname", Terminal ",", Nonterminal "init_declarator_declarator_typedefname"],
      ProductionRule "init_declarator_declarator_varname" [Nonterminal "declarator_varname"],
      ProductionRule "init_declarator_declarator_varname" [Nonterminal "declarator", Terminal "=", Nonterminal "c_initializer"],
      ProductionRule "init_declarator_declarator_typedefname" [Nonterminal "declarator_typedefname"],
      ProductionRule "init_declarator_declarator_typedefname" [Nonterminal "declarator", Terminal "=", Nonterminal "c_initializer"],
      ProductionRule "storage_class_specifier" [Terminal "extern"],
      ProductionRule "storage_class_specifier" [Terminal "static"],
      -- ProductionRule "storage_class_specifier" [Terminal "_Thread_local"],
      ProductionRule "storage_class_specifier" [Terminal "auto"],
      ProductionRule "storage_class_specifier" [Terminal "register"],
      ProductionRule "type_specifier_nonunique" [Terminal "char"],
      ProductionRule "type_specifier_nonunique" [Terminal "short"],
      ProductionRule "type_specifier_nonunique" [Terminal "int"],
      ProductionRule "type_specifier_nonunique" [Terminal "long"],
      ProductionRule "type_specifier_nonunique" [Terminal "float"],
      ProductionRule "type_specifier_nonunique" [Terminal "double"],
      ProductionRule "type_specifier_nonunique" [Terminal "signed"],
      ProductionRule "type_specifier_nonunique" [Terminal "unsigned"],
      -- ProductionRule "type_specifier_nonunique" [Terminal "_Complex"],
      ProductionRule "type_specifier_unique" [Terminal "void"],
      -- ProductionRule "type_specifier_unique" [Terminal "_Bool"],
      ProductionRule "type_specifier_unique" [Nonterminal "atomic_type_specifier"],
      ProductionRule "type_specifier_unique" [Nonterminal "struct_or_union_specifier"],
      ProductionRule "type_specifier_unique" [Nonterminal "enum_specifier"],
      ProductionRule "type_specifier_unique" [Nonterminal "typedef_name_spec"],
      ProductionRule "struct_or_union_specifier" [Nonterminal "struct_or_union", Nonterminal "option_general_identifier", Terminal "{", Nonterminal "struct_declaration_list", Terminal "}"],
      ProductionRule "struct_or_union_specifier" [Nonterminal "struct_or_union", Nonterminal "general_identifier"],
      ProductionRule "struct_or_union" [Terminal "struct"],
      ProductionRule "struct_or_union" [Terminal "union"],
      ProductionRule "struct_declaration_list" [Nonterminal "struct_declaration"],
      ProductionRule "struct_declaration_list" [Nonterminal "struct_declaration_list", Nonterminal "struct_declaration"],
      ProductionRule "struct_declaration" [Nonterminal "specifier_qualifier_list", Nonterminal "option_struct_declarator_list", Terminal ";"],
      ProductionRule "struct_declaration" [Nonterminal "static_assert_declaration"],
      ProductionRule "specifier_qualifier_list" [Nonterminal "list_eq1_type_specifier_unique_type_qualifier"],
      ProductionRule "specifier_qualifier_list" [Nonterminal "list_eq1_type_specifier_unique_alignment_specifier"],
      ProductionRule "specifier_qualifier_list" [Nonterminal "list_ge1_type_specifier_nonunique_type_qualifier"],
      ProductionRule "specifier_qualifier_list" [Nonterminal "list_ge1_type_specifier_nonunique_alignment_specifier"],
      ProductionRule "struct_declarator_list" [Nonterminal "struct_declarator"],
      ProductionRule "struct_declarator_list" [Nonterminal "struct_declarator_list", Terminal ",", Nonterminal "struct_declarator"],
      ProductionRule "struct_declarator" [Nonterminal "declarator"],
      ProductionRule "struct_declarator" [Nonterminal "option_declarator", Terminal ":", Nonterminal "constant_expression"],
      ProductionRule "enum_specifier" [Terminal "enum", Nonterminal "option_general_identifier", Terminal "{", Nonterminal "enumerator_list", Nonterminal "option_comma", Terminal "}"],
      ProductionRule "enum_specifier" [Terminal "enum", Nonterminal "general_identifier"],
      ProductionRule "enumerator_list" [Nonterminal "enumerator"],
      ProductionRule "enumerator_list" [Nonterminal "enumerator_list", Terminal ",", Nonterminal "enumerator"],
      ProductionRule "enumerator" [Nonterminal "enumeration_constant"],
      ProductionRule "enumerator" [Nonterminal "enumeration_constant", Terminal "=", Nonterminal "constant_expression"],
      ProductionRule "enumeration_constant" [Nonterminal "general_identifier"],
      -- ProductionRule "atomic_type_specifier" [Terminal "_Atomic", Terminal "(", Nonterminal "type_name", Terminal ")"],
      -- ProductionRule "atomic_type_specifier" [Terminal "_Atomic", Terminal "ATOMIC_LPAREN", Nonterminal "type_name", Terminal ")"],
      ProductionRule "type_qualifier" [Terminal "const"],
      ProductionRule "type_qualifier" [Terminal "restrict"],
      ProductionRule "type_qualifier" [Terminal "volatile"],
      ProductionRule "type_qualifier" [Terminal "_Atomic"],
      ProductionRule "function_specifier" [Terminal "inline"],
      ProductionRule "function_specifier" [Terminal "_Noreturn"],
      -- ProductionRule "alignment_specifier" [Terminal "_Alignas", Terminal "(", Nonterminal "type_name", Terminal ")"],
      -- ProductionRule "alignment_specifier" [Terminal "_Alignas", Terminal "(", Nonterminal "constant_expression", Terminal ")"],
      ProductionRule "declarator" [Nonterminal "direct_declarator"],
      ProductionRule "declarator" [Nonterminal "pointer", Nonterminal "direct_declarator"],
      ProductionRule "direct_declarator" [Nonterminal "general_identifier"],
      ProductionRule "direct_declarator" [Terminal "(", Nonterminal "save_context", Nonterminal "declarator", Terminal ")"],
      ProductionRule "direct_declarator" [Nonterminal "direct_declarator", Terminal "[", Nonterminal "option_type_qualifier_list", Nonterminal "option_assignment_expression", Terminal "]"],
      ProductionRule "direct_declarator" [Nonterminal "direct_declarator", Terminal "[", Terminal "static", Nonterminal "option_type_qualifier_list", Nonterminal "assignment_expression", Terminal "]"],
      ProductionRule "direct_declarator" [Nonterminal "direct_declarator", Terminal "[", Nonterminal "type_qualifier_list", Terminal "static", Nonterminal "assignment_expression", Terminal "]"],
      ProductionRule "direct_declarator" [Nonterminal "direct_declarator", Terminal "[", Nonterminal "option_type_qualifier_list", Terminal "*", Terminal "]"],
      ProductionRule "direct_declarator" [Nonterminal "direct_declarator", Terminal "(", Nonterminal "scoped_parameter_type_list", Terminal ")"],
      ProductionRule "direct_declarator" [Nonterminal "direct_declarator", Terminal "(", Nonterminal "save_context", Nonterminal "option_identifier_list", Terminal ")"],
      ProductionRule "pointer" [Terminal "*", Nonterminal "option_type_qualifier_list", Nonterminal "option_pointer"],
      ProductionRule "type_qualifier_list" [Nonterminal "option_type_qualifier_list", Nonterminal "type_qualifier"],
      ProductionRule "parameter_type_list" [Nonterminal "parameter_list", Nonterminal "option_comma_dotdotdot", Nonterminal "save_context"],
      ProductionRule "parameter_list" [Nonterminal "parameter_declaration"],
      ProductionRule "parameter_list" [Nonterminal "parameter_list", Terminal ",", Nonterminal "parameter_declaration"],
      ProductionRule "parameter_declaration" [Nonterminal "declaration_specifiers", Nonterminal "declarator_varname"],
      ProductionRule "parameter_declaration" [Nonterminal "declaration_specifiers", Nonterminal "option_abstract_declarator"],
      ProductionRule "identifier_list" [Nonterminal "var_name"],
      ProductionRule "identifier_list" [Nonterminal "identifier_list", Terminal ",", Nonterminal "var_name"],
      ProductionRule "type_name" [Nonterminal "specifier_qualifier_list", Nonterminal "option_abstract_declarator"],
      ProductionRule "abstract_declarator" [Nonterminal "pointer"],
      ProductionRule "abstract_declarator" [Nonterminal "direct_abstract_declarator"],
      ProductionRule "abstract_declarator" [Nonterminal "pointer", Nonterminal "direct_abstract_declarator"],
      ProductionRule "direct_abstract_declarator" [Terminal "(", Nonterminal "save_context", Nonterminal "abstract_declarator", Terminal ")"],
      ProductionRule "direct_abstract_declarator" [Nonterminal "option_direct_abstract_declarator", Terminal "[", Nonterminal "option_assignment_expression", Terminal "]"],
      ProductionRule "direct_abstract_declarator" [Nonterminal "option_direct_abstract_declarator", Terminal "[", Nonterminal "type_qualifier_list", Nonterminal "option_assignment_expression", Terminal "]"],
      ProductionRule "direct_abstract_declarator" [Nonterminal "option_direct_abstract_declarator", Terminal "[", Terminal "static", Nonterminal "option_type_qualifier_list", Nonterminal "assignment_expression", Terminal "]"],
      ProductionRule "direct_abstract_declarator" [Nonterminal "option_direct_abstract_declarator", Terminal "[", Nonterminal "type_qualifier_list", Terminal "static", Nonterminal "assignment_expression", Terminal "]"],
      ProductionRule "direct_abstract_declarator" [Nonterminal "option_direct_abstract_declarator", Terminal "[", Terminal "*", Terminal "]"],
      ProductionRule "direct_abstract_declarator" [Terminal "(", Nonterminal "option_scoped_parameter_type_list", Terminal ")"],
      ProductionRule "direct_abstract_declarator" [Nonterminal "direct_abstract_declarator", Terminal "(", Nonterminal "option_scoped_parameter_type_list", Terminal ")"],
      ProductionRule "c_initializer" [Nonterminal "assignment_expression"],
      ProductionRule "c_initializer" [Terminal "{", Nonterminal "initializer_list", Nonterminal "option_comma", Terminal "}"],
      ProductionRule "initializer_list" [Nonterminal "option_designation", Nonterminal "c_initializer"],
      ProductionRule "initializer_list" [Nonterminal "initializer_list", Terminal ",", Nonterminal "option_designation", Nonterminal "c_initializer"],
      ProductionRule "designation" [Nonterminal "designator_list", Terminal "="],
      ProductionRule "designator_list" [Nonterminal "option_designator_list", Nonterminal "designator"],
      ProductionRule "designator" [Terminal "[", Nonterminal "constant_expression", Terminal "]"],
      ProductionRule "designator" [Terminal ".", Nonterminal "general_identifier"],
      -- ProductionRule "static_assert_declaration" [Terminal "_Static_assert", Terminal "(", Nonterminal "constant_expression", Terminal ",", Terminal "STRING_LITERAL", Terminal ")", Terminal ";"],
      ProductionRule "statement" [Nonterminal "labeled_statement"],
      ProductionRule "statement" [Nonterminal "scoped_compound_statement"],
      ProductionRule "statement" [Nonterminal "expression_statement"],
      ProductionRule "statement" [Nonterminal "scoped_selection_statement"],
      ProductionRule "statement" [Nonterminal "scoped_iteration_statement"],
      ProductionRule "statement" [Nonterminal "jump_statement"],
      ProductionRule "labeled_statement" [Nonterminal "general_identifier", Terminal ":", Nonterminal "statement"],
      ProductionRule "labeled_statement" [Terminal "case", Nonterminal "constant_expression", Terminal ":", Nonterminal "statement"],
      ProductionRule "labeled_statement" [Terminal "default", Terminal ":", Nonterminal "statement"],
      ProductionRule "compound_statement" [Terminal "{", Nonterminal "option_block_item_list", Terminal "}"],
      ProductionRule "block_item_list" [Nonterminal "option_block_item_list", Nonterminal "block_item"],
      ProductionRule "block_item" [Nonterminal "declaration"],
      ProductionRule "block_item" [Nonterminal "statement"],
      ProductionRule "expression_statement" [Nonterminal "option_expression", Terminal ";"],
      ProductionRule "selection_statement" [Terminal "if", Terminal "(", Nonterminal "expression", Terminal ")", Nonterminal "scoped_statement", Terminal "else", Nonterminal "scoped_statement"],
      ProductionRule "selection_statement" [Terminal "if", Terminal "(", Nonterminal "expression", Terminal ")", Nonterminal "scoped_statement"],
      ProductionRule "selection_statement" [Terminal "switch", Terminal "(", Nonterminal "expression", Terminal ")", Nonterminal "scoped_statement"],
      ProductionRule "iteration_statement" [Terminal "while", Terminal "(", Nonterminal "expression", Terminal ")", Nonterminal "scoped_statement"],
      ProductionRule "iteration_statement" [Terminal "do", Nonterminal "scoped_statement", Terminal "while", Terminal "(", Nonterminal "expression", Terminal ")", Terminal ";"],
      ProductionRule "iteration_statement" [Terminal "for", Terminal "(", Nonterminal "option_expression", Terminal ";", Nonterminal "option_expression", Terminal ";", Nonterminal "option_expression", Terminal ")", Nonterminal "scoped_statement"],
      ProductionRule "iteration_statement" [Terminal "for", Terminal "(", Nonterminal "declaration", Nonterminal "option_expression", Terminal ";", Nonterminal "option_expression", Terminal ")", Nonterminal "scoped_statement"],
      ProductionRule "jump_statement" [Terminal "goto", Nonterminal "general_identifier", Terminal ";"],
      ProductionRule "jump_statement" [Terminal "continue", Terminal ";"],
      ProductionRule "jump_statement" [Terminal "break", Terminal ";"],
      ProductionRule "jump_statement" [Terminal "return", Nonterminal "option_expression", Terminal ";"],
      ProductionRule "translation_unit_file" [Nonterminal "external_declaration", Nonterminal "translation_unit_file"],
      ProductionRule "translation_unit_file" [Nonterminal "external_declaration", Terminal "$"], -- Note: $ for EOF
      ProductionRule "external_declaration" [Nonterminal "function_definition"],
      ProductionRule "external_declaration" [Nonterminal "declaration"],
      ProductionRule "function_definition1" [Nonterminal "declaration_specifiers", Nonterminal "declarator_varname"],
      ProductionRule "function_definition" [Nonterminal "function_definition1", Nonterminal "option_declaration_list", Nonterminal "compound_statement"],
      ProductionRule "declaration_list" [Nonterminal "declaration"],
      ProductionRule "declaration_list" [Nonterminal "declaration_list", Nonterminal "declaration"],
      ProductionRule "option_argument_expression_list" [],
      ProductionRule "option_argument_expression_list" [Nonterminal "argument_expression_list"],
      ProductionRule "option_comma" [],
      ProductionRule "option_comma" [Terminal ","],
      ProductionRule "option_comma_dotdotdot" [],
      ProductionRule "option_comma_dotdotdot" [Terminal ",", Terminal "..."],
      ProductionRule "option_init_declarator_list_declarator_varname" [],
      ProductionRule "option_init_declarator_list_declarator_varname" [Nonterminal "init_declarator_list_declarator_varname"],
      ProductionRule "option_init_declarator_list_declarator_typedefname" [],
      ProductionRule "option_init_declarator_list_declarator_typedefname" [Nonterminal "init_declarator_list_declarator_typedefname"],
      ProductionRule "option_general_identifier" [],
      ProductionRule "option_general_identifier" [Nonterminal "general_identifier"],
      ProductionRule "option_struct_declarator_list" [],
      ProductionRule "option_struct_declarator_list" [Nonterminal "struct_declarator_list"],
      ProductionRule "option_declarator" [],
      ProductionRule "option_declarator" [Nonterminal "declarator"],
      ProductionRule "option_general_identifier" [],
      ProductionRule "option_general_identifier" [Nonterminal "general_identifier"],
      ProductionRule "option_type_qualifier_list" [],
      ProductionRule "option_type_qualifier_list" [Nonterminal "type_qualifier_list"],
      ProductionRule "option_assignment_expression" [],
      ProductionRule "option_assignment_expression" [Nonterminal "assignment_expression"],
      ProductionRule "option_type_qualifier_list" [],
      ProductionRule "option_type_qualifier_list" [Nonterminal "type_qualifier_list"],
      ProductionRule "option_identifier_list" [],
      ProductionRule "option_identifier_list" [Nonterminal "identifier_list"],
      ProductionRule "option_pointer" [],
      ProductionRule "option_pointer" [Nonterminal "pointer"],
      ProductionRule "option_abstract_declarator" [],
      ProductionRule "option_abstract_declarator" [Nonterminal "abstract_declarator"],
      ProductionRule "option_direct_abstract_declarator" [],
      ProductionRule "option_direct_abstract_declarator" [Nonterminal "direct_abstract_declarator"],
      ProductionRule "option_assignment_expression" [],
      ProductionRule "option_assignment_expression" [Nonterminal "assignment_expression"],
      ProductionRule "option_scoped_parameter_type_list" [],
      ProductionRule "option_scoped_parameter_type_list" [Nonterminal "scoped_parameter_type_list"],
      ProductionRule "option_designation" [],
      ProductionRule "option_designation" [Nonterminal "designation"],
      ProductionRule "option_designator_list" [],
      ProductionRule "option_designator_list" [Nonterminal "designator_list"],
      ProductionRule "option_block_item_list" [],
      ProductionRule "option_block_item_list" [Nonterminal "block_item_list"],
      ProductionRule "option_expression" [],
      ProductionRule "option_expression" [Nonterminal "expression"],
      ProductionRule "option_declaration_list" [],
      ProductionRule "option_declaration_list" [Nonterminal "declaration_list"],
      ProductionRule "list_eq1_type_specifier_unique_declaration_specifier" [Nonterminal "type_specifier_unique", Nonterminal "list_declaration_specifier"],
      ProductionRule "list_eq1_type_specifier_unique_declaration_specifier" [Nonterminal "declaration_specifier", Nonterminal "list_eq1_type_specifier_unique_declaration_specifier"],
      ProductionRule "list_declaration_specifier" [],
      ProductionRule "list_declaration_specifier" [Nonterminal "declaration_specifier", Nonterminal "list_declaration_specifier"],
      ProductionRule "list_eq1_type_specifier_unique_type_qualifier" [Nonterminal "type_specifier_unique", Nonterminal "list_type_qualifier"],
      ProductionRule "list_eq1_type_specifier_unique_type_qualifier" [Nonterminal "type_qualifier", Nonterminal "list_eq1_type_specifier_unique_type_qualifier"],
      ProductionRule "list_type_qualifier" [],
      ProductionRule "list_type_qualifier" [Nonterminal "type_qualifier", Nonterminal "list_type_qualifier"],
      ProductionRule "list_eq1_type_specifier_unique_alignment_specifier" [Nonterminal "type_specifier_unique", Nonterminal "list_alignment_specifier"],
      ProductionRule "list_eq1_type_specifier_unique_alignment_specifier" [Nonterminal "alignment_specifier", Nonterminal "list_eq1_type_specifier_unique_alignment_specifier"],
      ProductionRule "list_alignment_specifier" [],
      ProductionRule "list_alignment_specifier" [Nonterminal "alignment_specifier", Nonterminal "list_alignment_specifier"],
      ProductionRule "list_ge1_type_specifier_nonunique_declaration_specifier" [Nonterminal "type_specifier_nonunique", Nonterminal "list_declaration_specifier"],
      ProductionRule "list_ge1_type_specifier_nonunique_declaration_specifier" [Nonterminal "type_specifier_nonunique", Nonterminal "list_ge1_type_specifier_nonunique_declaration_specifier"],
      ProductionRule "list_ge1_type_specifier_nonunique_declaration_specifier" [Nonterminal "declaration_specifier", Nonterminal "list_ge1_type_specifier_nonunique_declaration_specifier"],
      ProductionRule "list_declaration_specifier" [],
      ProductionRule "list_declaration_specifier" [Nonterminal "declaration_specifier", Nonterminal "list_declaration_specifier"],
      ProductionRule "list_ge1_type_specifier_nonunique_type_qualifier" [Nonterminal "type_specifier_nonunique", Nonterminal "list_type_qualifier"],
      ProductionRule "list_ge1_type_specifier_nonunique_type_qualifier" [Nonterminal "type_specifier_nonunique", Nonterminal "list_ge1_type_specifier_nonunique_type_qualifier"],
      ProductionRule "list_ge1_type_specifier_nonunique_type_qualifier" [Nonterminal "type_qualifier", Nonterminal "list_ge1_type_specifier_nonunique_type_qualifier"],
      ProductionRule "list_type_qualifier" [],
      ProductionRule "list_type_qualifier" [Nonterminal "type_qualifier", Nonterminal "list_type_qualifier"],
      ProductionRule "list_ge1_type_specifier_nonunique_alignment_specifier" [Nonterminal "type_specifier_nonunique", Nonterminal "list_alignment_specifier"],
      ProductionRule "list_ge1_type_specifier_nonunique_alignment_specifier" [Nonterminal "type_specifier_nonunique", Nonterminal "list_ge1_type_specifier_nonunique_alignment_specifier"],
      ProductionRule "list_ge1_type_specifier_nonunique_alignment_specifier" [Nonterminal "alignment_specifier", Nonterminal "list_ge1_type_specifier_nonunique_alignment_specifier"],
      ProductionRule "list_alignment_specifier" [],
      ProductionRule "list_alignment_specifier" [Nonterminal "alignment_specifier", Nonterminal "list_alignment_specifier"],
      ProductionRule "list_eq1_eq1_typedef_type_specifier_unique_declaration_specifier" [Terminal "typedef", Nonterminal "list_eq1_type_specifier_unique_declaration_specifier"],
      ProductionRule "list_eq1_eq1_typedef_type_specifier_unique_declaration_specifier" [Nonterminal "type_specifier_unique", Nonterminal "list_eq1_typedef_declaration_specifier"],
      ProductionRule "list_eq1_eq1_typedef_type_specifier_unique_declaration_specifier" [Nonterminal "declaration_specifier", Nonterminal "list_eq1_eq1_typedef_type_specifier_unique_declaration_specifier"],
      ProductionRule "list_eq1_type_specifier_unique_declaration_specifier" [Nonterminal "type_specifier_unique", Nonterminal "list_declaration_specifier"],
      ProductionRule "list_eq1_type_specifier_unique_declaration_specifier" [Nonterminal "declaration_specifier", Nonterminal "list_eq1_type_specifier_unique_declaration_specifier"],
      ProductionRule "list_eq1_typedef_declaration_specifier" [Terminal "typedef", Nonterminal "list_declaration_specifier"],
      ProductionRule "list_eq1_typedef_declaration_specifier" [Nonterminal "declaration_specifier", Nonterminal "list_eq1_typedef_declaration_specifier"],
      ProductionRule "list_eq1_ge1_typedef_type_specifier_nonunique_declaration_specifier" [Terminal "typedef", Nonterminal "list_ge1_type_specifier_nonunique_declaration_specifier"],
      ProductionRule "list_eq1_ge1_typedef_type_specifier_nonunique_declaration_specifier" [Nonterminal "type_specifier_nonunique", Nonterminal "list_eq1_typedef_declaration_specifier"],
      ProductionRule "list_eq1_ge1_typedef_type_specifier_nonunique_declaration_specifier" [Nonterminal "type_specifier_nonunique", Nonterminal "list_eq1_ge1_typedef_type_specifier_nonunique_declaration_specifier"],
      ProductionRule "list_eq1_ge1_typedef_type_specifier_nonunique_declaration_specifier" [Nonterminal "declaration_specifier", Nonterminal "list_eq1_ge1_typedef_type_specifier_nonunique_declaration_specifier"],
      ProductionRule "list_ge1_type_specifier_nonunique_declaration_specifier" [Nonterminal "type_specifier_nonunique", Nonterminal "list_declaration_specifier"],
      ProductionRule "list_ge1_type_specifier_nonunique_declaration_specifier" [Nonterminal "type_specifier_nonunique", Nonterminal "list_ge1_type_specifier_nonunique_declaration_specifier"],
      ProductionRule "list_ge1_type_specifier_nonunique_declaration_specifier" [Nonterminal "declaration_specifier", Nonterminal "list_ge1_type_specifier_nonunique_declaration_specifier"],
      ProductionRule "list_eq1_typedef_declaration_specifier" [Terminal "typedef", Nonterminal "list_declaration_specifier"],
      ProductionRule "list_eq1_typedef_declaration_specifier" [Nonterminal "declaration_specifier", Nonterminal "list_eq1_typedef_declaration_specifier"]
      ]
    

c11_filter_terminals =
  map snd
    [
      -- (EOF, "EOF"),

      (NAME, "NAME"),
      (VARIABLE, "VARIABLE"), (TYPE, "TYPE"),
      (CONSTANT, "CONSTANT"), (STRING_LITERAL, "STRING_LITERAL"),

      -- (ALIGNAS, "_Alignas"), (ALIGNOF, "_Alignof"),
      -- (ATOMIC, "_Atomic"),
      (AUTO, "auto"),
      (BOOL, "bool"),
      (BREAK, "break"),
      (CASE, "case"),
      (CHAR, "char"),
      -- (COMPLEX, "_Complex"),
      (CONST, "const"),
      (CONTINUE, "continue"),
      (DEFAULT, "default"),
      (DO, "do"),
      (DOUBLE, "double"),
      (ELSE, "else"),
      (ENUM, "enum"),
      (EXTERN, "extern"),
      (FLOAT, "float"),
      (FOR, "for"),
      (GENERIC, "_Generic"),
      (GOTO, "goto"),
      (IF, "if"),
      -- (IMAGINARY, "_Imaginary"),
      (INLINE, "inline"),
      (INT, "int"),
      (LONG, "long"),
      -- (NORETURN, "_Noreturn"),
      (REGISTER, "register"),
      -- (RESTRICT, "restrict"),
      (RETURN, "return"),
      (SHORT, "short"),
      (SIGNED, "signed"),
      (SIZEOF, "sizeof"),
      (STATIC, "static"),
      -- (STATIC_ASSERT, "_Static_assert"),
      (STRUCT, "struct"),
      (SWITCH, "switch"),
      -- (THREAD_LOCAL, "_Thread_local"),
      (TYPEDEF, "typedef"),
      (UNION, "union"),
      (UNSIGNED, "unsigned"),
      (VOID, "void"),
      (VOLATILE, "volatile"),
      (WHILE, "while"),

      (ELLIPSIS, "..."),
      (ADD_ASSIGN, "+="),
      (SUB_ASSIGN, "-="),
      (MUL_ASSIGN, "*="),
      (DIV_ASSIGN, "/="),
      (MOD_ASSIGN, "%="),
      (OR_ASSIGN, "|="),
      (AND_ASSIGN, "&="),
      (XOR_ASSIGN, "^="),
      (LEFT_ASSIGN, "<<="),
      (RIGHT_ASSIGN, ">>="),
      (LEFT, "<<"),
      (RIGHT, ">>"),
      (EQEQ, "=="),
      (NEQ, "!="),
      (LEQ, "<="),
      (GEQ, ">="),
      (EQ, "="),
      (LT, "<"),
      (GT, ">"),
      (INC, "++"),
      (DEC, "--"),
      (PTR, "->"),
      (PLUS, "+"),
      (MINUS, "-"),
      (STAR, "*"),
      (SLASH, "/"),
      (PERCENT, "%"),
      (BANG, "!"),
      (ANDAND, "&&"),
      (BARBAR, "||"),
      (AND, "&"),
      (BAR, "|"),
      (HAT, "^"),
      (QUESTION, "?"),
      (COLON, ":"),
      (TILDE, "~"),
      (LBRACE, "{"),
      (RBRACE, "}"),
      (LBRACK, "["),
      (RBRACK, "]"),
      (LPAREN, "("),
      (RPAREN, ")"),
      (SEMICOLON, ";"),
      (COMMA, ","),
      (DOT, ".")
    ]
