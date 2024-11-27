#include <limits.h>
#include <string.h>
#include "spl.tab.h"
#include "ast.h"
#include "code.h"
#include "code_seq.h"
#include "code_utils.h"
#include "id_use.h"
#include "literal_table.h"
#include "gen_code.h"
#include "utilities.h"
#include "regname.h"
#include "machine_types.h"   // Include this header for word_type
#include "bof.h"             // Include this header for bof_write_word

#define STACK_SPACE 4096

// Initialize the code generator
void gen_code_initialize() {
	literal_table_initialize();
}

// Return a header appropriate for the given code
BOFHeader gen_code_program_header(code_seq main_cs)
{
	BOFHeader ret;
	strncpy(ret.magic, "BO32", MAGIC_BUFFER_SIZE + 1);
	ret.text_start_address = 0;
	ret.text_length = code_seq_size(main_cs);
	ret.data_start_address = MAX(ret.text_length, 1024);
	ret.data_length = literal_table_size();
	ret.stack_bottom_addr = ret.data_start_address + ret.data_length + STACK_SPACE;
	return ret;
}


// Output code sequence to the BOF file
void gen_code_output_seq(BOFFILE bf, code_seq cs)
{
	while (!code_seq_is_empty(cs)) {
		bin_instr_t inst = code_seq_first(cs)->instr;
		instruction_write_bin_instr(bf, inst);
		cs = code_seq_rest(cs);
	}
}

// Output literals to the BOF file
void gen_code_output_literals(BOFFILE bf)
{
	literal_table_start_iteration();
	while (literal_table_iteration_has_next()) {
		word_type w = literal_table_iteration_next();
		bof_write_word(bf, w);
	}
	literal_table_end_iteration(); // not necessary
}

// Output the program to the BOF file
void gen_code_output_program(BOFFILE bf, code_seq main_cs) {
	BOFHeader bf_header = gen_code_program_header(main_cs);
	bof_write_header(bf, bf_header);
	gen_code_output_seq(bf, main_cs);
	gen_code_output_literals(bf);
	bof_close(bf);
}

// Generate code for the main program
void gen_code_program(BOFFILE bf, block_t prog) {
	fprintf(stderr, "The type tag is %d", prog.type_tag);
	// Initialize the main code sequence with variable declarations
	code_seq main_cs = gen_code_var_decls(prog.var_decls);
	fprintf(stderr, "Returned from the var decls\n");
	// Calculate stack space in bytes based on the size of the code sequence
	int vars_len_in_bytes = (code_seq_size(main_cs) / 2) * BYTES_PER_WORD;

	// Allocate stack space for variables
	code_seq_concat(&main_cs, code_utils_allocate_stack_space(vars_len_in_bytes));

	code_seq_concat(&main_cs, code_utils_set_up_program());
	fprintf(stderr, "Going into making our first block stmt\n");
	fprintf(stderr, "Attempting to not crash\n");
	// stmt_t *sp = prog.stmts.stmt_list.start;

	// while (sp != NULL) {
	//     fprintf(stderr, "Our curr stmt_kind is: %d\n", sp->stmt_kind);
	//     sp = sp->next;
	// }
	// I think we need to gen all stmts
	code_seq_concat(&main_cs, gen_code_stmts(prog.stmts));
	fprintf(stderr, "Back in gen_code_program from gen_code_block_stmt\n");

	// Deallocate stack space for variables
	code_seq_concat(&main_cs, code_utils_deallocate_stack_space(vars_len_in_bytes));

	// Add an exit instruction to terminate the program
	code_seq_concat(&main_cs, code_utils_tear_down_program());
	// Output the generated program
	gen_code_output_program(bf, main_cs);
}

// Not needed in hw4
code_seq gen_code_proc_decls(proc_decls_t pds) {
	fprintf(stderr, "Function gen_code_proc_decls not implemented!\n");
	code_seq res = code_seq_empty();
	return res;
}

code_seq gen_code_const_decls(const_decls_t cds) {
	code_seq ret = code_seq_empty();
	// const_decl_t *cdp = cds.start;
	// while (cdp != NULL) {
	//     // Generate code for each variable declaration
	//     code_seq_concat(&ret, gen_code_const_decl(*cdp));
	//     cdp = cdp->next;
	// }
	return ret;
}

// Generate code for variable declarations
code_seq gen_code_var_decls(var_decls_t vds) {
	code_seq ret = code_seq_empty();
	var_decl_t* vdp = vds.var_decls;
	while (vdp != NULL) {
		// Generate code for each variable declaration
		code_seq_concat(&ret, gen_code_var_decl(*vdp));
		vdp = vdp->next;
	}
	return ret;
}

// Generate code for a single constant declaration
code_seq gen_code_const_decl(const_decl_t cd) {
	// Add constant value to the literal table and get the offset
	// offset_type offset = literal_table_lookup(NULL, cd.value);

	// Generate code to push the constant (its offset) onto the stack
	code_seq ret = code_seq_empty();
	// code *push_instr = code_addi(SP, SP, -BYTES_PER_WORD); // Allocate stack space
	// code_seq_concat(&ret, code_seq_singleton(push_instr));

	// // Load the constant into the stack (using its offset from the literal table)
	// code *load_instr = code_load(SP, offset);
	// code_seq_concat(&ret, code_seq_singleton(load_instr));
	return ret;
}

// Generate code for a single variable declaration
code_seq gen_code_var_decl(var_decl_t vd) {
	return gen_code_idents(vd.ident_list, vd.type_tag);
}

code_seq gen_code_idents(ident_list_t idents, AST_type type) {
	fprintf(stderr, "In gen_code_idents\n");
	code_seq ret = code_seq_empty();
	ident_t* id = idents.start;
	code_seq new = code_seq_empty();
	while (id != NULL) {
		code_seq new = code_seq_singleton(code_addi(SP, SP, -BYTES_PER_WORD));
		//fprintf(stderr, "RAHHHHHHH %d\n", type);  // type gives 6
		switch (type) {
			case var_decl_ast:
				fprintf(stderr, "Making a New Variable\n");
				//offset_type offset = literal_table_lookup(id->name, 0);  // Initialize the value to 0
				code_seq_add_to_end(&new, code_swr(SP, 0, 0));
				break;
			default:
				fprintf(stderr, "Have not implemented the case for %d in gen_code_idents", type);
		}
		code_seq_concat(&new, ret);
		id = id->next;
	}
	code_seq_concat(&new, ret);
	return ret;
}

// Generate code for the list of statements
code_seq gen_code_stmts(stmts_t stmts)
{
	fprintf(stderr, "MAKING STATEMENTS\n");
	if (stmts.stmt_list.start->stmt_kind == NULL) {
		fprintf(stderr, "No statements to process.\n");
		return code_seq_empty();
	}
	else {
		fprintf(stderr, "we have statements to process\n");
	}

	code_seq ret = code_seq_empty();
	stmt_t* sp = stmts.stmt_list.start;
	fprintf(stderr, "we dont make it here\n");

	while (sp != NULL) {
		code_seq stmt_code = gen_code_stmt(*sp);
		code_seq_concat(&ret, stmt_code);
		sp = sp->next;
	}

	return ret;
}

// Generate code for a statement
code_seq gen_code_stmt(stmt_t stmt)
{
	// fprintf(stderr, "MAKING A NEW STATEMENT\n");
	if (stmt.stmt_kind == NULL) {
		fprintf(stderr, "UHOH EMPTY\n");
		return code_seq_empty();
	}
	switch (stmt.stmt_kind) {
		case assign_stmt:
			fprintf(stderr, "going to call assign_stmt\n");
			return gen_code_assign_stmt(stmt.data.assign_stmt);
			// Not used in HW4
		case call_stmt:
			fprintf(stderr, "going to call call_stmt\n");
			return gen_code_call_stmt(stmt.data.call_stmt);
		case if_stmt:
			fprintf(stderr, "going to call if_stmt\n");
			return gen_code_if_stmt(stmt.data.if_stmt);
		case while_stmt:
			fprintf(stderr, "going to call while_stmt\n");
			return gen_code_while_stmt(stmt.data.while_stmt);
		case read_stmt:
			fprintf(stderr, "going to call read_stmt\n");
			return gen_code_read_stmt(stmt.data.read_stmt);
		case print_stmt:
			fprintf(stderr, "going to call print_stmt\n");
			return gen_code_print_stmt(stmt.data.print_stmt);
		case block_stmt:
			fprintf(stderr, "going to call block_stmt\n");
			return gen_code_block_stmt(stmt.data.block_stmt);
		default:
			bail_with_error("Call to gen_code_stmt with an AST that is not a statement!");
			break;
	}
	// The following can never execute, but this quiets warnings
	return code_seq_empty();
}

// Does not need to be implemented in hw4
code_seq gen_code_call_stmt(call_stmt_t stmt) {
	code_seq ret = code_seq_empty();
	fprintf(stderr, "Function gen_code_call_stmt not implemented\n");
	return ret;
}

// Does not need to be implemented in hw4
code_seq gen_code_read_stmt(read_stmt_t stmt) {
	code_seq ret = code_seq_empty();
	fprintf(stderr, "Function gen_code_read_stmt not implemented\n");
	return ret;
}

code_seq gen_code_while_stmt(while_stmt_t stmt) {
	code_seq ret = code_seq_empty();
	fprintf(stderr, "Function gen_code_while_stmt not implemented\n");
	return ret;
}

code_seq gen_code_print_stmt(print_stmt_t stmt) {
	code_seq ret = code_seq_empty();
	ret = gen_code_expr(stmt.expr);
	if (stmt.expr.type_tag == number_ast) {
		// If the expression is a number, lookup the literal value in the table
		offset_type offset = literal_table_lookup(NULL, stmt.expr.data.number.value);

		// Add the code to print the number
		code_seq_concat(&ret, code_seq_singleton(code_pint(SP, offset)));
	}
	else if (stmt.expr.type_tag == ident_ast) {
		// If the expression is an identifier, lookup its value in the table
		offset_type offset = literal_table_lookup(stmt.expr.data.ident.name, 0);

		// Add the code to print the identifier (use the offset)
		code_seq_concat(&ret, code_seq_singleton(code_pint(SP, offset)));
	}
	else {
		// If the expression is neither a number nor an identifier, print 0 (fallback case)
		code_seq_concat(&ret, code_seq_singleton(code_pint(SP, 0)));
	}
	return ret;
}

// Generate code for an assignment statement
code_seq gen_code_assign_stmt(assign_stmt_t stmt) {
	fprintf(stderr, "Handling assignment statement for variable: %s\n", stmt.name);
	// Generate code for the RHS expression
	code_seq rhs_code = gen_code_expr(*stmt.expr);

	// Lookup or add the variable in the literal table
	offset_type offset = literal_table_lookup(stmt.name, 0);

	// Generate store instruction (store the result into the variable)
	code_seq_add_to_end(&rhs_code, code_swr(SP, offset, 4));
	return rhs_code;
}


// Generate code for an if statement
code_seq gen_code_if_stmt(if_stmt_t stmt) {
	code_seq ret = code_seq_empty();
	fprintf(stderr, "In gen_code_if_stmt\n");
	fprintf(stderr, "The type of this statement is %d", stmt.type_tag);
	//code_seq_concat(&ret, );
	code_seq then_block = gen_code_stmts(*stmt.then_stmts);
	int then_block_size = code_seq_size(then_block);
	code_seq else_block = gen_code_stmts(*stmt.else_stmts);
	return ret;
}


code_seq gen_code_block_stmt(block_stmt_t stmt) {
	fprintf(stderr, "HANDLING A BLOCK STMT\n");
	code_seq ret = code_utils_copy_regs(3, FP);
	code_seq vars = gen_code_var_decls(stmt.block->var_decls);
	int vars_len_in_bytes = (code_seq_size(vars) / 2) * BYTES_PER_WORD;
	code_seq_concat(&ret, vars);
	code_seq_concat(&ret, gen_code_const_decls(stmt.block->const_decls));

	code_seq_concat(&ret, code_utils_save_registers_for_AR());
	//code_seq_add_to_end(&ret, code_add(SP, 0, SP, vars_len_in_bytes));
	//code_seq_concat(&ret, gen_code_proc_decls(stmt.block->proc_decls)); // Not needed in hw4
	//fprintf(stderr, "returned from gen_code_proc_decls\n");
	code_seq_concat(&ret, gen_code_stmts(stmt.block->stmts));
	code_seq_concat(&ret, code_utils_restore_registers_from_AR());
	code_seq_concat(&ret, code_utils_deallocate_stack_space(vars_len_in_bytes));
	fprintf(stderr, "RETURNING FROM THE BLOCK STMT\n");
	return ret;
}

// Generate code for an expression
code_seq gen_code_expr(expr_t exp)
{
	switch (exp.expr_kind) {
		case expr_bin:
			return gen_code_binary_op_expr(exp.data.binary);
		case expr_ident:
			return gen_code_ident(exp.data.ident);
		case expr_number:
			return gen_code_number(exp.data.number);
		case expr_negated:
			return gen_code_logical_not_expr((exp.data.negated));
		default:
			bail_with_error("Unexpected expr_kind_e (%d) in gen_code_expr", exp.expr_kind);
			break;
	}
	// Never happens, but suppresses warnings
	return code_seq_empty();
}

code_seq gen_code_arith_op(token_t op) {
	code_seq res = code_seq_empty();
	fprintf(stderr, "Function gen_code_arith_op not implemented\n");
	return res;
}

code_seq gen_code_rel_op(token_t op, expr_kind_e exp_type) {
	code_seq res = code_seq_empty();
	fprintf(stderr, "Function gen_code_rel_op not implemented\n");
	return res;
}

// Generate code to apply op to the
// 2nd from top and top of the stack,
// putting the result on top of the stack in their place,
// and using V0 and AT as temporary registers
// Modifies SP when executed
code* gen_code_op(token_t op, expr_kind_e exp_type)
{
	fprintf(stderr, "MAKING AN OP\n");
	switch (op.code) {
		case eqsym:
		case neqsym:
		case ltsym:
		case leqsym:
		case gtsym:
		case geqsym:
			fprintf(stderr, "THE REL OPS ARENT IMPLEMENTED\n");
		case plussym:
			return code_add(SP, 1, SP, 1);
		case minussym:
			return code_sub(SP, 1, SP, 1);
		case multsym:
			return code_mul(SP, 1);
		case divsym:
			return code_div(SP, 1);
		default:
			bail_with_error("Unknown token code (%d) in gen_code_op", op.code);
			break;
	}
	fprintf(stderr, "Function gen_code_op not implemented\n");

	return NULL;
}

code_seq gen_code_binary_op_expr(binary_op_expr_t expr) {
	fprintf(stderr, "MAKING A BINARY OP EXPR\n");
	// Generate code for both operands
	code_seq ret = gen_code_expr(*expr.expr1);
	code_seq right_code = gen_code_expr(*expr.expr2);

	// Combine both operand codes
	code_seq_concat(&ret, right_code);

	// Generate the operation code
	code* op_code = gen_code_op(expr.arith_op, expr.expr1->expr_kind);

	// Concatenate all sequences
	code_seq_add_to_end(&ret, op_code);
	code_seq_concat(&ret, code_utils_deallocate_stack_space(1));

	return ret;
}


code_seq gen_code_ident(ident_t expr) {
	// Lookup or add the identifier in the literal table
	offset_type offset = literal_table_lookup(expr.name, 0);

	// Allocate stack space and load the identifier's value
	code_seq ret = code_utils_allocate_stack_space(1);
	code* load_instr = code_lwr(SP, 4, offset); // Load value from offset
	code_seq_add_to_end(&ret, load_instr);

	return ret;
}


code_seq gen_code_number(number_t expr) {
	fprintf(stderr, "MAKING A NUMBER\n");
	// Lookup or add the number to the literal table
	offset_type offset = literal_table_lookup(expr.text, expr.value);

	// Allocate space and push the literal's offset onto the stack
	code_seq ret = code_utils_allocate_stack_space(1);
	code_seq_add_to_end(&ret, code_cpw(SP, 0, GP, offset));
	return ret;
}

code_seq gen_code_logical_not_expr(negated_expr_t expr) {
	fprintf(stderr, "Function gen_code_logical_not_expr might not be implemented properly\n");
	code_seq inner_code = gen_code_expr(*expr.expr); // Evaluate inner expression
	code* not_instruction = code_notr(); // Apply NOT to the top of the stack
	code_seq_concat(&inner_code, code_seq_singleton(not_instruction));
	return inner_code;
}