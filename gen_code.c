#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "bof.h"
#include "lexer.h"
#include "parser.h"
#include "unparser.h"
#include "ast.h"
#include "utilities.h"
#include "symtab.h"
#include "scope_check.h"
#include "literal_table.h"
#include "instruction.h"
#include "code.h"

/*
 *		take in bof file
 *
 */



 /**
  *	Gencode init
  *
  *	not sure what wee do here yet...
  *
  */
void gen_code_initialize() {
	printf("Hello, from gen_code_init!\n");

	// literal table / AR init goes here
}


/**
 *	Gencode Program
 *
 *	generate code and store in bof
 *
 */
void gen_code_program(BOFFILE bf, block_t prog) {

	// Header to bof
	BOFHeader bf_header;



	while loop through ast{

		// figure out correct registers

		// generate bin instructions with code.h

		// code d = blah
		// make next node idk


		switch (ast type)
		{
			case something something ast type :
				d.instr = code_add();
				break;

			default:
				break;
		}
	}

	bf_header.data_length = 999;

	/*
		Write BOF
	*/

	// Write header
	bof_write_header(bf, bf_header);

	// write to bof
	code c; // head of code* linked list
	while (c.next != NULL)
	{

		switch (instruction_type(c.instr))
		{
		case comp_instr_type:
			instruction_write_compInstr(bf, c.instr.comp);
			break;

		case other_comp_instr_type:
			instruction_write_otherCompInstr(bf, c.instr.othc);
			break;

		case immed_instr_type:
			instruction_write_immedInstr(bf, c.instr.immed);
			break;

		case jump_instr_type:
			instruction_write_jumpInstr(bf, c.instr.jump);
			break;

		case syscall_instr_type:
			instruction_write_syscallInstr(bf, c.instr.syscall);
			break;

		case error_instr_type:
			// error? idk what this one is for
			break;

		default:
			bail_with_error("ERROR: invalid instr type while writing code to bof file gencode.c\n");
			break;
		}

		c = c.next;

	}

	//

	// data?


}


// BOF FILE STRUCTURE
/**
 *	Header
 *
 *	word_type text_start_address;  // word address to start running (PC)
	word_type text_length;         // size of the text section in words
	word_type data_start_address;  // word address of static data (GP)
	word_type data_length;         // size of data section in words
	word_type stack_bottom_addr;   // word address of stack "bottom" (FP)
 *
 * 	============
 *
 *	Instructions
 *
 *
 *
 *	=============
 *
 *	Data
 *
 *
 *   */