/* $Id: gen_code.h,v 1.7 2023/11/14 04:41:00 leavens Exp $ */
#ifndef _GEN_CODE_H
#define _GEN_CODE_H

#include "ast.h"
#include "bof.h"
#include "code.h"
#include "code_seq.h"
#include "instruction.h"
#include <stdio.h>

// Initialize the code generator
void gen_code_initialize();


BOFHeader gen_code_program_header(code_seq main_cs);


code_seq gen_code_var_decls(var_decls_t v_decls);


void gen_code_output_program(BOFFILE bf, code_seq main);

// Generate BOF code for the given ast
void gen_code_program(BOFFILE bf, block_t progast);

code_seq gen_code_stmts(stmts_t stmts);

code_seq gen_code_stmt(stmt_t stmt);

code_seq gen_code_call_stmt(call_stmt_t stmt);

code_seq gen_code_read_stmt(read_stmt_t stmt);

code_seq gen_code_while_stmt(while_stmt_t stmt);

code_seq gen_code_print_stmt(print_stmt_t stmt);

code_seq gen_code_block_stmt(block_stmt_t stmt);

code_seq gen_code_assign_stmt(assign_stmt_t stmt);

code_seq gen_code_if_stmt(if_stmt_t stmt);

code_seq gen_code_expr(expr_t exp);

code_seq gen_code_binary_op_expr(binary_op_expr_t expr);

code_seq gen_code_ident(ident_t expr);

code_seq gen_code_number(number_t expr);

code_seq gen_code_logical_not_expr(negated_expr_t expr);

code_seq gen_code_var_decl(var_decl_t vd);

code_seq gen_code_idents(ident_list_t idents, AST_type type);

code_seq gen_code_rel_op(token_t op, expr_kind_e exp_type);

code_seq gen_code_arith_op(token_t op);

code* gen_code_op(token_t op, expr_kind_e exp_type);

code_seq gen_code_const_decl(const_decl_t cd);

code_seq gen_code_const_decls(const_decls_t cds);

code_seq gen_code_proc_decls(proc_decls_t pds);

#endif