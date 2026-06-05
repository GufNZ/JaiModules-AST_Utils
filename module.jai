#import "Basic";
#import "Compiler";
#import "File_Utilities";
#import "Hash_Table";
PP :: #import "Program_Print";
#import "String";

//TODO: add utilities for copying/creating AST node trees & nodes.

NodeKindToNodeType : []Type = .[
	//	UNINITIALIZED				:: 0;
	void,
	//	BLOCK						:: 1;
	Code_Block,
	//	LITERAL						:: 2;
	Code_Literal,
	//	IDENT						:: 3;
	Code_Ident,
	//	UNARY_OPERATOR				:: 4;
	Code_Unary_Operator,
	//	BINARY_OPERATOR				:: 5;
	Code_Binary_Operator,
	//	PROCEDURE_BODY				:: 6;
	Code_Procedure_Body,
	//	PROCEDURE_CALL				:: 7;
	Code_Procedure_Call,
	//	CONTEXT						:: 8;
	Code_Context,
	//	WHILE						:: 9;
	Code_While,
	//	IF							:: 10;
	Code_If,
	//	LOOP_CONTROL				:: 11;
	Code_Loop_Control,
	//	CASE						:: 12;
	Code_Case,
	//	// 13 is currently unused.
	void,
	//	RETURN						:: 14;
	Code_Return,
	//	FOR							:: 15;
	Code_For,
	//
	//	TYPE_DEFINITION				:: 16;
	Code_Type_Definition,
	//	TYPE_INSTANTIATION			:: 17;
	Code_Type_Instantiation,
	//	ENUM						:: 18;
	Code_Enum,
	//
	//	PROCEDURE_HEADER			:: 19;
	Code_Procedure_Header,
	//	STRUCT						:: 20;
	Code_Struct,
	//	COMMA_SEPARATED_ARGUMENTS	:: 21;
	Code_Comma_Separated_Arguments,
	//	EXTRACT						:: 22;
	Code_Extract,
	//	DIRECTIVE_BYTES				:: 23;
	Code_Directive_Bytes,
	//	MAKE_VARARGS				:: 24;
	Code_Make_Varargs,
	//
	//	DECLARATION					:: 25;
	Code_Declaration,
	//
	//	CAST						:: 26;
	Code_Cast,
	//
	//	DIRECTIVE_IMPORT			:: 27;
	Code_Directive_Import,
	//	DIRECTIVE_THIS				:: 28;
	void,	//NOTE: there doesn't seem to be a compiler node for this [yet].
	//	DIRECTIVE_THROUGH			:: 29;
	Code_Directive_Through,
	//	DIRECTIVE_LOAD				:: 30;
	Code_Directive_Load,
	//
	//	DIRECTIVE_RUN				:: 31;
	Code_Directive_Run,
	//	DIRECTIVE_CODE				:: 32;
	Code_Directive_Code,
	//	DIRECTIVE_POKE_NAME			:: 33;
	Code_Directive_Poke_Name,
	//	ASM							:: 34;  // We might reorder this later so it's not in the middle of the directives.
	Code_Asm,
	//	DIRECTIVE_BAKE				:: 35;
	Code_Directive_Bake,
	//	DIRECTIVE_MODIFY			:: 36;
	Code_Directive_Modify,
	//	DIRECTIVE_LIBRARY			:: 37;
	Code_Directive_Library,
	//
	//	EXPRESSION_QUERY			:: 38;
	Code_Expression_Query,
	//	PUSH_CONTEXT				:: 39;
	Code_Push_Context,
	//
	//	NOTE						:: 40;
	Code_Note,
	//
	//	DIRECTIVE_PLACE				:: 41;
	void,	//NOTE: not a thing anymore.
	//	DIRECTIVE_SCOPE				:: 42;
	Code_Directive_Scope,
	//	TYPE_QUERY					:: 43;
	Code_Type_Query,
	//	DIRECTIVE_LOCATION			:: 44;
	Code_Directive_Location,
	//	DIRECTIVE_MODULE_PARAMETERS	:: 45;
	Code_Directive_Module_Parameters,
	//	DIRECTIVE_ADD_CONTEXT		:: 46;
	Code_Directive_Add_Context,
	//	DIRECTIVE_COMPILE_TIME		:: 47;
	void,	//NOTE: unused in Compiler module.
	//
	//	COMPOUND_DECLARATION		:: 48;
	Code_Compound_Declaration,
	//	DEFER						:: 49;
	Code_Defer,
	//	USING						:: 50;
	Code_Using,
	//	PLACEHOLDER					:: 51;
	Code_Placeholder,
	//
	//	DIRECTIVE_INSERT			:: 52;
	Code_Directive_Insert,
	//	DIRECTIVE_PROCEDURE_NAME	:: 53;
	Code_Directive_Procedure_Name,
	//	DIRECTIVE_WILDCARD			:: 54;
	Code_Directive_Wildcard,
	//	DIRECTIVE_EXISTS			:: 55;
	Code_Directive_Exists,
	//	DIRECTIVE_CONTEXT_TYPE		:: 56;
	Code_Directive_Context_Type,
	//
	//	RESOLVED_OVERLOAD			:: 57;
	Code_Resolved_Overload,
	//	DIRECTIVE_OVERLAY			:: 58;
	Code_Directive_Overlay,
];
NodeTypeToNodeKind: Table(Type, NodeKind, given_hash_function=(k) => k.(*Type_Info).(u32,trunc));

CopyEnum :: (enumName: string, src: *Type_Info) -> string {
	b: String_Builder;
	srcInfo := src.(*Type_Info_Enum);
	print(*b, "% :: enum {\n", enumName);
	for srcInfo.names {
		print(*b, "\t% :: %;", it, srcInfo.values[it_index]);
	}
	print(*b, "}\n");
	return builder_to_string(*b);
}

#insert #run CopyEnum("NodeKind", type_info(Code_Node.Kind));

nodeKindColours: Table(s64, string, given_hash_function=(k) => k.(u32,trunc));

nodeKindColour :: (nodeKind: s64) -> string {
	s :: 255;
	a :: 255 - s;
	if !nodeKindColours.count {
		values := enum_values_as_s64(NodeKind);
		for values {
			table_add(
				*nodeKindColours,
				it,
				tprint(
					"%;%;%",
					(it_index * s * 53 / values.count) & s + a,
					(it_index * s * 29 / values.count) & s + a,
					(it_index * s * 41 / values.count) & s + a
				)
			);
		}
		//for v, k : nodeKindColours {
		//	log("Node kind colour: [%] % -> % \e[38;2;%3m%3\e[0m\n", k, k.(NodeKind), v);
		//}
	}

	found, colour := table_find(*nodeKindColours, nodeKind);
	assert(found, tprint("nodeKindColour: No colour found for node kind %", nodeKind.(NodeKind)));
	return colour;
}


unwrapTypechecked :: (typechecked: []Typechecked($T)) -> []*T {
	result: [..]*T;
	for typechecked {
		array_add(*result, it.expression);
	}

	return result;
}


HasNote :: struct {
	notes: [] *Code_Note;
}

hasNote :: (node: *$T/interface HasNote, noteName: string) -> bool {
	for node.notes {
		if it.text == noteName {
			return true;
		}
	}


	return false;
}

hasNotePrefix :: (node: *$T/interface HasNote, notePrefix: string) -> bool {
	for node.notes {
		if starts_with(it.text, notePrefix) {
			return true;
		}
	}


	return false;
}

hasNoteContaining :: (node: *$T/interface HasNote, noteContent: string) -> bool {
	for node.notes {
		if contains(it.text, noteContent) {
			return true;
		}
	}


	return false;
}


filenameIs :: (node: *Code_Node, filename: string) -> bool {
	return path_filename(node.location.enclosing_load.fully_pathed_filename) == filename;
}

pathContains :: (node: *Code_Node, needle: string) -> bool {
	path := parse_path(node.location.enclosing_load.fully_pathed_filename);
	for path.words {
		if it == needle {
			return true;
		}
	}


	return false;
}

filterASTNodes :: (nodes: []*$NodeType, predicate: (node: *$FilteredNodeType) -> bool) -> []*FilteredNodeType {
	if !NodeTypeToNodeKind.count {
		for NodeKindToNodeType {
			table_add(*NodeTypeToNodeKind, it, xx it_index);
		}
	}

	found, targetKind := table_find(*NodeTypeToNodeKind, FilteredNodeType);
	assert(found, tprint("filterAST: Invalid node type %", FilteredNodeType));

	result: [..]*FilteredNodeType;
	for nodes {
		if it.kind == xx targetKind {
			node := it.(*FilteredNodeType);
			if predicate(node) {
				array_add(*result, node);
			}
		}
	}

	return result;
}

TransformData :: struct (FilteredNodeType: Type, TransformedNodeType: Type) {
	targetKind: NodeKind;
	predicate: (node: *FilteredNodeType) -> bool;
	transform: (node: *FilteredNodeType) -> *TransformedNodeType;
}

transformAST :: (node: *$T/Code_Node, predicate: (node: *$FilteredNodeType) -> bool, transform: (node: *FilteredNodeType) -> *$TransformedNodeType, log := false) -> *T {
	if !NodeTypeToNodeKind.count {
		for NodeKindToNodeType {
			table_add(*NodeTypeToNodeKind, it, xx it_index);
		}
	}

	found, targetKind := table_find(*NodeTypeToNodeKind, FilteredNodeType);
	assert(found, tprint("transformAST: Invalid node type %", FilteredNodeType));

	transformData := TransformData(FilteredNodeType, TransformedNodeType).{
		targetKind,
		predicate,
		transform
	};

	return walkAST(
		!log,
		node,
		transformData,
		(t, candidate) => ifx candidate.kind == xx t.targetKind && t.predicate(candidate.(*FilteredNodeType))
			then t.transform(candidate.(*FilteredNodeType)).(*Code_Node)
			else candidate
	);
}

repeat :: (n: int) -> string {
	s: String_Builder;
	for 1..n {
		append(*s, " ");
	}
	return builder_to_string(*s);
}

nodeDetail :: (node: *$T/Code_Node) -> string {
	if node.kind == {
		case .UNARY_OPERATOR;
			b: String_Builder;
			append(*b, " \e[93m");
			op := node.(*Code_Unary_Operator).operator_type;
			if op == #char "." {
				append(*b, ".");
			} else {
				print(*b, "%", node.(*Code_Unary_Operator).operator_type);
			}
			return builder_to_string(*b);
		case .BINARY_OPERATOR;
			b: String_Builder;
			append(*b, " \e[93m");
			op := node.(*Code_Binary_Operator).operator_type;
			if op < xx 128 {
				print(*b, "%", string.{1, (*op).(*u8)});
			} else {
				print(*b, "%", node.(*Code_Binary_Operator).operator_type);
			}
			return builder_to_string(*b);
		case .LITERAL; #through;
		case .IDENT; #through;
		case .LOOP_CONTROL; #through;
		case .NOTE; //#through;
			b: String_Builder;
			append(*b, " \e[93m");
			PP.print_expression(*b, node);
			return builder_to_string(*b);
	}
	return "";
}

nodeInfo :: (node: *$T/Code_Node) -> string {
	return tprint(
		"\e[38;2;%m% \e[36m@ \e[34m%\e[90m:\e[32m%\e[90m:\e[31m%\e[37m (0x%)%\e[0m",
		nodeKindColour(xx node.kind),
		node.kind,
		ifx node.location.enclosing_load  node.location.enclosing_load.fully_pathed_filename else "<no file>",
		node.location.l0,
		node.location.c0,
		formatInt(node.(u64), base=16),
		nodeDetail(node)
	);
}

seen: Table(*Code_Node, bool);
walkAST :: (quiet: bool, node: *$T/Code_Node, userData: $D, handler: (userData: D, node: *Code_Node) -> *Code_Node, path := "root", depth := 0, trim := false) -> *T {
	if !depth {
		table_reset(*seen);
	}
	if !node {
		return null;
	}
	if table_find(*seen, node) {
		if !quiet {
			print(
				"%\e[95m%\e[101;97mAlready seen node:\e[0m %\n",
				repeat(depth),
				path,
				nodeInfo(node)
			);
		}
		return node;
	}


	table_add(*seen, node, true);

	callback :: (quiet: bool, node: *$T/Code_Node, userData: D, handler: (userData: D, node: *Code_Node) -> *Code_Node, depth: int, path: string) -> *T {
		if !quiet {
			print(
				"%Visiting %: %\n",
				repeat(depth),
				path,
				nodeInfo(node)
			);
		}
		return handler(userData, node).(*T);
	};

	node = callback(quiet, node, userData, handler, depth, path);
	depth += 1;

	if #complete node.kind == {
		case .UNINITIALIZED;
			assert(false, "walkAST: encountered uninitialized node");
		case .BLOCK;
			block := node.(*Code_Block);
			if !trim {
				block.belongs_to_struct = walkAST(quiet, block.belongs_to_struct, userData, handler, "belongs_to_struct", depth, true);
			}
			//.parent
			//.owning_statement
			for block.members {
				block.members[it_index] = walkAST(quiet, it, userData, handler, tprint("members[%]", it_index), depth);
			}
			for block.statements {
				block.statements[it_index] = walkAST(quiet, it, userData, handler, tprint("statements[%]", it_index), depth);
			}

		case .LITERAL;
		case .IDENT;

		case .UNARY_OPERATOR;
			unary := node.(*Code_Unary_Operator);
			unary.subexpression = walkAST(quiet, unary.subexpression, userData, handler, "subexpression", depth);

		case .BINARY_OPERATOR;
			bin := node.(*Code_Binary_Operator);
			bin.left = walkAST(quiet, bin.left, userData, handler, "left", depth);
			bin.right = walkAST(quiet, bin.right, userData, handler, "right", depth);

		case .PROCEDURE_BODY;
			body := node.(*Code_Procedure_Body);
			if !trim {
				body.header = walkAST(quiet, body.header, userData, handler, "header", depth, true);
			}
			body.block = walkAST(quiet, body.block, userData, handler, "block", depth);

		case .PROCEDURE_CALL;
			call := node.(*Code_Procedure_Call);
			call.procedure_expression = walkAST(quiet, call.procedure_expression, userData, handler, "procedure_expression", depth);
			if call.overloads {
				for call.overloads.* {
					call.overloads.*[it_index] = walkAST(quiet, it, userData, handler, tprint("overloads[%]", it_index), depth);
				}
			}
			call.resolved_procedure_expression = walkAST(quiet, call.resolved_procedure_expression, userData, handler, "resolved_procedure_expression", depth);
			for call.arguments_unsorted {
				call.arguments_unsorted[it_index].expression = walkAST(quiet, it.expression, userData, handler, tprint("arguments_unsorted[%]", it_index), depth);
			}
			call.macro_expansion_block = walkAST(quiet, call.macro_expansion_block, userData, handler, "macro_expansion_block", depth);
			if call.context_modification {
				for call.context_modification.modification_expressions {
					call.context_modification.modification_expressions[it_index] = walkAST(quiet, it, userData, handler, tprint("modification_expressions[%]", it_index), depth);
				}
			}

		case .CONTEXT;

		case .WHILE;
			whileNode := node.(*Code_While);
			whileNode.condition = walkAST(quiet, whileNode.condition, userData, handler, "condition", depth);
			whileNode.block = walkAST(quiet, whileNode.block, userData, handler, "block", depth);

		case .IF;
			ifNode := node.(*Code_If);
			ifNode.condition = walkAST(quiet, ifNode.condition, userData, handler, "condition", depth);
			ifNode.then_block = walkAST(quiet, ifNode.then_block, userData, handler, "then_block", depth);
			ifNode.else_block = walkAST(quiet, ifNode.else_block, userData, handler, "else_block", depth);
			ifNode.static_if_accepted_case = walkAST(quiet, ifNode.static_if_accepted_case, userData, handler, "static_if_accepted_case", depth);

		case .LOOP_CONTROL;
			loop := node.(*Code_Loop_Control);
			loop.target_ident = walkAST(quiet, loop.target_ident, userData, handler, "target_ident", depth);

		case .CASE;
			caseNode := node.(*Code_Case);
			caseNode.condition = walkAST(quiet, caseNode.condition, userData, handler, "condition", depth);
			caseNode.then_block = walkAST(quiet, caseNode.then_block, userData, handler, "then_block", depth);

		case .RETURN;
			returnNode := node.(*Code_Return);
			for returnNode.arguments_unsorted {
				returnNode.arguments_unsorted[it_index].expression = walkAST(quiet, it.expression, userData, handler, tprint("arguments_unsorted[%]", it_index), depth);
			}

		case .FOR;
			forNode := node.(*Code_For);
			forNode.iteration_expression = walkAST(quiet, forNode.iteration_expression, userData, handler, "iteration_expression", depth);
			forNode.iteration_expression_right = walkAST(quiet, forNode.iteration_expression_right, userData, handler, "iteration_expression_right", depth);
			forNode.ident_it = walkAST(quiet, forNode.ident_it, userData, handler, "ident_it", depth);
			forNode.ident_it_index = walkAST(quiet, forNode.ident_it_index, userData, handler, "ident_it_index", depth);
			forNode.want_replacement_for_expansion = walkAST(quiet, forNode.want_replacement_for_expansion, userData, handler, "want_replacement_for_expansion", depth);
			forNode.want_pointer_expression = walkAST(quiet, forNode.want_pointer_expression, userData, handler, "want_pointer_expression", depth);
			forNode.want_reverse_expression = walkAST(quiet, forNode.want_reverse_expression, userData, handler, "want_reverse_expression", depth);
			forNode.macro_expansion_procedure_call = walkAST(quiet, forNode.macro_expansion_procedure_call, userData, handler, "macro_expansion_procedure_call", depth);
			forNode.block = walkAST(quiet, forNode.block, userData, handler, "block", depth);

		case .TYPE_DEFINITION;

		case .TYPE_INSTANTIATION;
			typeInstantiation := node.(*Code_Type_Instantiation);
			typeInstantiation.type_valued_expression = walkAST(quiet, typeInstantiation.type_valued_expression, userData, handler, "type_valued_expression", depth);
			typeInstantiation.must_implement = walkAST(quiet, typeInstantiation.must_implement, userData, handler, "must_implement", depth);
			typeInstantiation.pointer_to = walkAST(quiet, typeInstantiation.pointer_to, userData, handler, "pointer_to", depth);
			typeInstantiation.type_directive_target = walkAST(quiet, typeInstantiation.type_directive_target, userData, handler, "type_directive_target", depth);
			typeInstantiation.array_element_type = walkAST(quiet, typeInstantiation.array_element_type, userData, handler, "array_element_type", depth);
			typeInstantiation.array_dimension = walkAST(quiet, typeInstantiation.array_dimension, userData, handler, "array_dimension", depth);

		case .ENUM;
			enumNode := node.(*Code_Enum);
			enumNode.internal_type_inst = walkAST(quiet, enumNode.internal_type_inst, userData, handler, "internal_type_inst", depth);
			enumNode.block = walkAST(quiet, enumNode.block, userData, handler, "block", depth);
			for enumNode.notes {
				enumNode.notes[it_index] = walkAST(quiet, it, userData, handler, tprint("notes[%]", it_index), depth);
			}

		case .PROCEDURE_HEADER;
			header := node.(*Code_Procedure_Header);
			header.constants_block = walkAST(quiet, header.constants_block, userData, handler, "constants_block", depth);
			for header.arguments {
				header.arguments[it_index] = walkAST(quiet, it, userData, handler, tprint("arguments[%]", it_index), depth);
			}
			for header.returns {
				header.returns[it_index] = walkAST(quiet, it, userData, handler, tprint("returns[%]", it_index), depth);
			}
			for header.parameter_usings {
				header.parameter_usings[it_index] = walkAST(quiet, it, userData, handler, tprint("parameter_usings[%]", it_index), depth);
			}
			header.library_identifier = walkAST(quiet, header.library_identifier, userData, handler, "library_identifier", depth);
			header.polymorph_source_header = walkAST(quiet, header.polymorph_source_header, userData, handler, "polymorph_source_header", depth);
			for header.modify_directives {
				header.modify_directives[it_index] = walkAST(quiet, it, userData, handler, tprint("modify_directives[%]", it_index), depth);
			}
			if !trim {
				header.body_or_null = walkAST(quiet, header.body_or_null, userData, handler, "body_or_null", depth, true);
			}
			for header.notes {
				header.notes[it_index] = walkAST(quiet, it, userData, handler, tprint("notes[%]", it_index), depth);
			}

		case .STRUCT;
			structNode := node.(*Code_Struct);
			for structNode.modify_directives {
				structNode.modify_directives[it_index] = walkAST(quiet, it, userData, handler, tprint("modify_directives[%]", it_index), depth);
			}
			structNode.arguments_block = walkAST(quiet, structNode.arguments_block, userData, handler, "arguments_block", depth);
			structNode.constants_block = walkAST(quiet, structNode.constants_block, userData, handler, "constants_block", depth);
			//structNode.defined_type =
			structNode.block = walkAST(quiet, structNode.block, userData, handler, "block", depth, true);
			for structNode.notes {
				structNode.notes[it_index] = walkAST(quiet, it, userData, handler, tprint("notes[%]", it_index), depth);
			}

		case .COMMA_SEPARATED_ARGUMENTS;
			commaSeparatedArguments := node.(*Code_Comma_Separated_Arguments);
			for commaSeparatedArguments.arguments {
				commaSeparatedArguments.arguments[it_index].node = walkAST(quiet, it.node, userData, handler, tprint("arguments[%]", it_index), depth);
			}

		case .EXTRACT;
			extractNode := node.(*Code_Extract);
			extractNode.from = walkAST(quiet, extractNode.from, userData, handler, "from", depth);

		case .DIRECTIVE_BYTES;
			directiveBytesNode := node.(*Code_Directive_Bytes);
			directiveBytesNode.expression = walkAST(quiet, directiveBytesNode.expression, userData, handler, "expression", depth);

		case .MAKE_VARARGS;
			makeVarargsNode := node.(*Code_Make_Varargs);
			for makeVarargsNode.expressions {
				makeVarargsNode.expressions[it_index] = walkAST(quiet, it, userData, handler, tprint("expressions[%]", it_index), depth);
			}

		case .DECLARATION;
			declaration := node.(*Code_Declaration);
			declaration.type_inst = walkAST(quiet, declaration.type_inst, userData, handler, "type_inst", depth);
			declaration.expression = walkAST(quiet, declaration.expression, userData, handler, "expression", depth);
			declaration.alignment_expression = walkAST(quiet, declaration.alignment_expression, userData, handler, "alignment_expression", depth);
			for declaration.notes {
				declaration.notes[it_index] = walkAST(quiet, it, userData, handler, tprint("notes[%]", it_index), depth);
			}

		case .CAST;
			castNode := node.(*Code_Cast);
			castNode.target_type = walkAST(quiet, castNode.target_type, userData, handler, "target_type", depth);
			castNode.expression = walkAST(quiet, castNode.expression, userData, handler, "expression", depth);

		case .DIRECTIVE_IMPORT;
			import := node.(*Code_Directive_Import);
			import.module_parameters_call = walkAST(quiet, import.module_parameters_call, userData, handler, "module_parameters_call", depth);
			import.program_parameters_call = walkAST(quiet, import.program_parameters_call, userData, handler, "program_parameters_call", depth);

		case .DIRECTIVE_THIS;

		case .DIRECTIVE_THROUGH;

		case .DIRECTIVE_LOAD;

		case .DIRECTIVE_RUN;
			run := node.(*Code_Directive_Run);
			run.procedure = walkAST(quiet, run.procedure, userData, handler, "procedure", depth);

		case .DIRECTIVE_CODE;
			code := node.(*Code_Directive_Code);
			code.expression = walkAST(quiet, code.expression, userData, handler, "expression", depth);

		case .DIRECTIVE_POKE_NAME;
			pokeName := node.(*Code_Directive_Poke_Name);
			pokeName.module_struct = walkAST(quiet, pokeName.module_struct, userData, handler, "module_struct", depth);

		case .ASM;

		case .DIRECTIVE_BAKE;
			bake := node.(*Code_Directive_Bake);
			bake.procedure_call = walkAST(quiet, bake.procedure_call, userData, handler, "procedure_call", depth);

		case .DIRECTIVE_MODIFY;
			modify := node.(*Code_Directive_Modify);
			modify.block = walkAST(quiet, modify.block, userData, handler, "block", depth);

		case .DIRECTIVE_LIBRARY;

		case .EXPRESSION_QUERY;
			query := node.(*Code_Expression_Query);
			query.expression_to_query = walkAST(quiet, query.expression_to_query, userData, handler, "expression_to_query", depth);

		case .PUSH_CONTEXT;
			pushContext := node.(*Code_Push_Context);
			pushContext.to_push = walkAST(quiet, pushContext.to_push, userData, handler, "to_push", depth);
			pushContext.block = walkAST(quiet, pushContext.block, userData, handler, "block", depth);

		case .NOTE;

		case .DIRECTIVE_PLACE;

		case .DIRECTIVE_SCOPE;

		case .TYPE_QUERY;
			typeQuery := node.(*Code_Type_Query);
			typeQuery.type_to_query = walkAST(quiet, typeQuery.type_to_query, userData, handler, "type_to_query", depth);

		case .DIRECTIVE_LOCATION;
			location := node.(*Code_Directive_Location);
			location.expression = walkAST(quiet, location.expression, userData, handler, "expression", depth);

		case .DIRECTIVE_MODULE_PARAMETERS;
			params := node.(*Code_Directive_Module_Parameters);
			params.module_parameters = walkAST(quiet, params.module_parameters, userData, handler, "module_parameters", depth);
			params.program_parameters = walkAST(quiet, params.program_parameters, userData, handler, "program_parameters", depth);
			params.common_code = walkAST(quiet, params.common_code, userData, handler, "common_code", depth);

		case .DIRECTIVE_ADD_CONTEXT;
			addContext := node.(*Code_Directive_Add_Context);
			addContext.expression = walkAST(quiet, addContext.expression, userData, handler, "expression", depth);

		case .DIRECTIVE_COMPILE_TIME;

		case .COMPOUND_DECLARATION;
			compound := node.(*Code_Compound_Declaration);
			compound.comma_separated_assignment = walkAST(quiet, compound.comma_separated_assignment, userData, handler, "comma_separated_assignment", depth);
			compound.declaration_properties = walkAST(quiet, compound.declaration_properties, userData, handler, "declaration_properties", depth);
			compound.alignment_expression = walkAST(quiet, compound.alignment_expression, userData, handler, "alignment_expression", depth);
			for compound.notes {
				compound.notes[it_index] = walkAST(quiet, it, userData, handler, tprint("notes[%]", it_index), depth);
			}

		case .DEFER;
			d := node.(*Code_Defer);
			d.block = walkAST(quiet, d.block, userData, handler, "block", depth);

		case .USING;
			u := node.(*Code_Using);
			u.expression = walkAST(quiet, u.expression, userData, handler, "expression", depth);
			u.filter_expression = walkAST(quiet, u.filter_expression, userData, handler, "filter_expression", depth);

		case .PLACEHOLDER;

		case .DIRECTIVE_INSERT;
			insert := node.(*Code_Directive_Insert);
			insert.expression = walkAST(quiet, insert.expression, userData, handler, "expression", depth);
			insert.scope_redirection = walkAST(quiet, insert.scope_redirection, userData, handler, "scope_redirection", depth);
			insert.break_replacement = walkAST(quiet, insert.break_replacement, userData, handler, "break_replacement", depth);
			insert.continue_replacement = walkAST(quiet, insert.continue_replacement, userData, handler, "continue_replacement", depth);
			insert.remove_replacement = walkAST(quiet, insert.remove_replacement, userData, handler, "remove_replacement", depth);
			insert.expansion = walkAST(quiet, insert.expansion, userData, handler, "expansion", depth);

		case .DIRECTIVE_PROCEDURE_NAME;
			directiveProcName := node.(*Code_Directive_Procedure_Name);
			directiveProcName.argument = walkAST(quiet, directiveProcName.argument, userData, handler, "argument", depth);

		case .DIRECTIVE_WILDCARD;

		case .DIRECTIVE_EXISTS;
			exists := node.(*Code_Directive_Exists);
			exists.query_expression = walkAST(quiet, exists.query_expression, userData, handler, "query_expression", depth);
			exists.sync_expression = walkAST(quiet, exists.sync_expression, userData, handler, "sync_expression", depth);

		case .DIRECTIVE_CONTEXT_TYPE;

		case .RESOLVED_OVERLOAD;
			overload := node.(*Code_Resolved_Overload);
			overload.source_expression = walkAST(quiet, overload.source_expression, userData, handler, "source_expression", depth);
			overload.result = walkAST(quiet, overload.result, userData, handler, "result", depth);

		case .DIRECTIVE_OVERLAY;
			overlay := node.(*Code_Directive_Overlay);
			overlay.declaration_or_using = walkAST(quiet, overlay.declaration_or_using, userData, handler, "declaration_or_using", depth);
			overlay.target_expression = walkAST(quiet, overlay.target_expression, userData, handler, "target_expression", depth);
	}

	return node;
}
