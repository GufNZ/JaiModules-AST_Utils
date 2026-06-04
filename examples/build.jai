#import "Basic";
#import "Compiler";
#import "String";
#import "Program_Print";
#import "AST_Utils";

#run {
	w := compiler_create_workspace("Target Program");

	options := get_build_options(w);
	copy_commonly_propagated_fields(get_build_options(), *options);

	set_working_directory(#filepath);

	options.output_executable_name = "astTests";

	set_build_options(options, w);

	compiler_begin_intercept(w);
	add_build_file("astTests.jai", w);

	set_build_options_dc(.{do_output = false, interactive_bytecode_debugger = true});

	message_loop(w);

	compiler_end_intercept(w);
}

convertToLiteral :: (expr: *Code_Procedure_Call) -> *Code_Literal {
	b: String_Builder;
	print_expression(*b, expr);

	result := New(Code_Literal);
	result.value_type = .STRING;
	result._string = builder_to_string(*b);

	return result;
}

formatLong :: (v: Any) -> FormatStruct {
	return FormatStruct.{
		value = v,
		draw_type_name = true,
		use_long_form_if_more_than_this_many_members = -1,
		use_newlines_if_long_form = true,
		begin_string = "\e[91m.{\e[92m",
		end_string = "\e[91m}\e[0m",
		separator_between_name_and_value = "\e[93m = \e[94m",
		long_form_separator_between_fields = "\e[95m;\e[92m",
		recursive_long_form_depth = -1,
	};
}

message_loop :: (w: Workspace) {
	while true {
		message := compiler_wait_for_message();
		if message.kind == {
			case .COMPLETE;
				break;

			//case .IMPORT;
			//	import := message.(*Message_Import);
			//	log("Import: '%'\n", import.module_name);

			//case .FILE;
			//	file := message.(*Message_File);
			//	log("File: '%'\n", file.fully_pathed_filename);

			case .TYPECHECKED;
				typechecked := message.(*Message_Typechecked);
				for body: filterASTNodes(
					unwrapTypechecked(typechecked.procedure_bodies),
					(body: *Code_Procedure_Body) -> bool {
						return hasNote(body.header, "doConvert") && filenameIs(body.header, "astTests.jai");
					}
				) {
					log("Typechecked procedure header: %'%' from '%:%:%'\n",
						body.body_flags,
						body.header.name,
						body.header.location.enclosing_load.fully_pathed_filename,
						body.header.location.l0,
						body.header.location.c1
					);
					if body.body_flags & .ALREADY_MODIFIED {
						log("Already modified, skipping.\n");
						continue;
					}


					b: String_Builder;
					print_expression(*b, body.block);
					print("Before:\n%\n\n", builder_to_string(*b));

					convertedBody := transformAST(
						body,
						(call: *Code_Procedure_Call) -> bool {
							//print("Checking call: \e[94m%\e[0m\n", call.procedure_expression.(*Code_Ident).name);
							return call.resolved_procedure_expression
								&& call.resolved_procedure_expression.kind == .PROCEDURE_HEADER
								&& call.resolved_procedure_expression.(*Code_Procedure_Header).name == "convert";
						},
						call => convertToLiteral(xx call.arguments_unsorted[0].expression)
					);
					print_expression(*b, convertedBody.block);
					print("After:\n%\n\n", builder_to_string(*b));
					compiler_modify_procedure(w, convertedBody);
				}

			//case .PHASE;
			//	phase := message.(*Message_Phase);
			//	log("Phase: '%'\n", phase.phase);
				//if (phase.phase == .TYPECHECKED_ALL_WE_CAN) && !added_strings {
				//	added_strings = true;
				//	add_strings(w);
				//}
		}
	}
}
