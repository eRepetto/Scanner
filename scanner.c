/*
*File name: scanner.c
*Compiler: MS Visual Studio 2015
*Author: Gabriel Richard [student number], Exequiel Repetto, 040885774
*Course: CST 8152 – Compilers, Lab Section: 11
*Assignment: 2
*Date: 2018-11-08
*Professor: Sv. Ranev
*Purpose:
*Function list:
*/

/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
* to suppress the warnings about using "unsafe" functions like fopen()
* and standard sting library functions defined in string.h.
* The define does not have any effect in Borland compiler projects.
*/
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

/*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
It is defined in platy_st.c */
extern Buffer * str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/
static pBuffer sc_buf; /*pointer to input source buffer*/
					   /* No other global variable declarations/definitions are allowed */

					   /* scanner.c static(local) function  prototypes */
static int char_class(char c); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
static int iskeyword(char * kw_lexeme); /*keywords lookup function */
static int isAndOr();
static Token generateErrorToken();

/*Initializes scanner */
int scanner_init(Buffer * psc_buf) {
	if (b_isempty(psc_buf)) return EXIT_FAILURE;/*1*/
												/* in case the buffer has been read previously  */
	b_rewind(psc_buf);
	b_clear(str_LTBL);
	line = 1;
	sc_buf = psc_buf;
	return EXIT_SUCCESS;/*0*/
						/*   scerrnum = 0;  *//*no need - global ANSI C */
}

/*
* Purpose: Reads from the buffer one character at a time and returns a token 
*	when a lexeme matches with a token pattern. 
* Author: Gabriel Richard, 040-880-482; Exequiel Repetto, 040-885-774
* History/Versions: 1.0
* Called functions: b_getc(), b_retract(), isAndOr(), b_mark(), b_getcoffset(),
*	get_next_state(), b_allocate(), b_reset(),b_addc(), b_free()
* Parameters: void
* Return value: Returns an error token if it finds an illegal symbol; else a
*	run-time error token if there is a run-time error; else a token matching 
*	the lexeme found in the buffer. 
* Algorithm: A character is retrieved from the source buffer. The character is 
*	compared against simple token patterns first. If there is no match, a
*	transition table is then used to determine if there is a lexical match with
*	variable identifiers, integer literals, floating-point literals and string
*	literals. 
*/
Token malar_next_token(void)
{
	Token t = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
	unsigned char c; /* input symbol */
	int state = 0; /* initial state of the FSM */
	short lexstart = 0;  /*start offset of a lexeme in the input char buffer (array) */
	short lexend = 0;    /*end   offset of a lexeme in the input char buffer (array)*/
	int accept = NOAS; /* type of state - initially not accepting */
	int isRelationalOperator = 0; /* used to store temporary result of isAndOr function */

	while (1) { /* endless loop broken by token returns it will generate a warning */

		if (!sc_buf)
			return generateErrorToken();
		/* Get the next symbol from the buffer */
		c = b_getc(sc_buf);

		/* Compare character for token match */
		switch (c) {

		case '\0':
			t.attribute.seof = SEOF1;
			t.code = SEOF_T;
			return t;
		case 255:
			t.attribute.seof = SEOF2;
			t.code = SEOF_T;
			return t;
		case '\n':
			line++;
		case '\t':
		case ' ':
			continue;
		case '!':
			/*gets next character to see if is a comment line */
			c = b_getc(sc_buf);

			if (c == '!') {
				while (c != '\n')
					c = b_getc(sc_buf);
				line++;
				continue;
			}
			else {
				t.attribute.err_lex[0] = '!';
				t.attribute.err_lex[1] = c;
				while (c != '\n')
					c = b_getc(sc_buf);
				line++;
				t.code = ERR_T;
				return t;
			}
			/*Separators*/
		case '(':
			t.code = LPR_T;
			return t;
		case ')':
			t.code = RPR_T;
			return t;
		case '{':
			t.code = LBR_T;
			return t;
		case '}':
			t.code = RBR_T;
			return t;
		case ',':
			t.code = COM_T;
			return t;
		case ';':
			t.code = EOS_T;
			return t;
			/*operators*/
		case '+':
			t.code = ART_OP_T;
			t.attribute.arr_op = PLUS;
			return t;
		case '-':
			t.code = ART_OP_T;
			t.attribute.arr_op = MINUS;
			return t;
		case '/':
			t.code = ART_OP_T;
			t.attribute.arr_op = DIV;
			return t;
		case '*':
			t.code = ART_OP_T;
			t.attribute.arr_op = MULT;
			return t;
		case '<':
			c = b_getc(sc_buf);
			if (c == '>')
				t.attribute.rel_op = NE;
			else {
				b_retract(sc_buf);
				t.attribute.rel_op = LT;
			}
			t.code = REL_OP_T;
			return t;
		case '>':
			t.code = REL_OP_T;
			t.attribute.rel_op = GT;
			return t;
		case '#':
			t.code = SCC_OP_T;
			return t;
		case '=':
			c = b_getc(sc_buf);
			if (c == '=') {
				t.code = REL_OP_T;
				t.attribute.rel_op = EQ;
			}
			else {
				b_retract(sc_buf);
				t.code = ASS_OP_T;
			}
			return t;
			/* Logical operators */
		case '.':
			isRelationalOperator = isAndOr(); 
			/* Check if isAndOr was successful */
			if (isRelationalOperator == RT_FAIL_1)
				return generateErrorToken();
			/* if function return 1 is .AND. if return 2 is .OR.*/
			if (isRelationalOperator) {
				/*lexeme is .AND.*/
				if (isRelationalOperator == 1)
					t.attribute.log_op = AND;
				/*lexeme is .OR.*/
				else if (isRelationalOperator == 2)
					t.attribute.log_op = OR;

				t.code = LOG_OP_T;
				return t;
			}
		}

		/* Part 2: Implementation of Finite State Machine (DFA)*/
		lexstart = b_mark(sc_buf, b_getcoffset(sc_buf) - 1);
		/* Check for run-time error */
		if (lexstart == RT_FAIL_1) {
			return generateErrorToken();
		}
		short capacity;

		while (accept == NOAS) {
			state = get_next_state(state, c, &accept);
			/* Check if get_next_state was successful */
			if (state == RT_FAIL_1) 
				return generateErrorToken();
			
			if (accept == ASWR || accept == ASNR) 
				break;
			
			c = b_getc(sc_buf);
		}

		if (accept == ASWR) {
			/* Check for run-time error */
			if (b_retract(sc_buf) == RT_FAIL_1) 
				return generateErrorToken();
		}

		lexend = b_getcoffset(sc_buf);
		/* Check if b_getcoffset was successful */
		if (lexend == RT_FAIL_1) 
			return generateErrorToken();

		capacity = lexend - lexstart;

		/* Check if memory allocation was successful */
		lex_buf = b_allocate(capacity + 1, 0, 'f');
		if (!lex_buf) 
			return generateErrorToken();

		/*retract getc_offset to the mark set previously 
		 and check for run-time error*/
		if ((b_reset(sc_buf)) == RT_FAIL_1)
			return generateErrorToken();

		for (int i = lexstart; i < lexend; i++) {
			c = b_getc(sc_buf);
			if (!b_addc(lex_buf, c))
				return generateErrorToken();
		}
		b_addc(lex_buf, '\0');

		t = aa_table[state](b_location(lex_buf, 0));
		b_free(lex_buf);
		return t;
	} /* end while(1) */
} /* end malar_next_token */


/*
*Purpose:
*Author: Sv. Ranev
*History/Versions:
*Called functions: char_class()
*Parameters:
*Return value:
*Algorithm:
*/
int get_next_state(int state, char c, int *accept)
{
	/* If accept is null pointer return -1 */
	if (!accept) 
		return RT_FAIL_1;
	
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif

	assert(next != IS);

#ifdef DEBUG
	if (next == IS) {
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
}

/*
*Purpose:
*Authors: Gabriel Richard, 040-880-482; Exequiel Repetto, 040-885-774
*History/Versions: 1.0
*Called functions: isalpha(), isdigit()
*Parameters:
*Return value:
*Algorithm:
*/
int char_class(char c)
{
	int column;

	if (isalpha(c))
		column = 0;
	else if (c == '0')
		column = 1;
	else if (isdigit(c))
		column = 2;
	else if (c == '.')
		column = 3;
	else if (c == '$')
		column = 4;
	else if (c == '"')
		column = 5;
	else if (c == '\0' || c == 255)
		column = 6;
	else
		column = 7;

	return column;
}

/*
ACCEPTING FUNCTION FOR THE arithmentic variable identifier AND keywords(VID - AVID / KW)

/*
*Purpose:
*Author: Exequiel Repetto 040885774
*History/Versions: 1.0
*Called functions: isKeyword(), strlen()
*Parameters:
*Return value:
*Algorithm:
*/

Token aa_func02(char lexeme[]) {
	/* If lexeme is null return run-time error token */
	if (!lexeme)
		return generateErrorToken();

	Token t;

	int temp = iskeyword(lexeme);
	/* Check if iskeyword was successful */
	if (temp == RT_FAIL_2)
		return generateErrorToken();

	if (temp != -1) {
		t.code = KW_T;
		t.attribute.kwt_idx = temp;
		/*return t;*/
	}
	else if ((strlen(lexeme) > VID_LEN)) {
		t.code = AVID_T;

		for (int i = 0; i < VID_LEN; i++) {
			t.attribute.vid_lex[i] = lexeme[i];
			if (i + 1 == VID_LEN)
				t.attribute.vid_lex[i + 1] = '\0';
		}
	}
	else {
		t.code = AVID_T;
		for (size_t i = 0; i < strlen(lexeme); i++) {
			t.attribute.vid_lex[i] = lexeme[i];
			if (i + 1 == strlen(lexeme))
				t.attribute.vid_lex[i + 1] = '\0';
		}
	}
	return t;
}

/*
* Purpose: Accepting state function for string variable identifiers. Stores 
*	SVID inside token SVID token.
* Author: Gabriel Richard, 040-880-482
* History/Versions: 1.0
* Called functions: strlen()
* Parameters: char lexeme[] - No restrictions
* Return value: Returns a String Variable Identifier (SVID) token
* Algorithm: If the passed lexeme is longer than 8 characters, stores the first
*	7 characters inside token and appends '$' followed by null terminator 
*	('\0'). If lexeme is shorter than 8 characters, stores all the characters
*	inside token and appends with null terminator. 
*/

Token aa_func03(char lexeme[]) {
	/* If lexeme is null return run-time error token */
	if (!lexeme)
		return generateErrorToken();

	Token t;
	t.code = SVID_T;

	/* Only store first 7 characters in token if lexeme is longer then 8 
	 characters */
	if (strlen(lexeme) > VID_LEN) {
		for (int i = 0; i < VID_LEN; ++i) {
			/* '$' followed by null terminator ('\0') is appended to the
			 end of SVID. */
			if (i == VID_LEN - 1) {
				t.attribute.vid_lex[i] = '$';
				t.attribute.vid_lex[i + 1] = '\0';
			}
			else
				t.attribute.vid_lex[i] = lexeme[i];
		}
	}
	else {
		/* Store lexeme in token */
		for (size_t i = 0; i < strlen(lexeme); i++)
			t.attribute.vid_lex[i] = lexeme[i];
		/* Append with null terminator to create c-style string */
		t.attribute.vid_lex[strlen(lexeme)] = '\0';
	}

	return t;
}

/*ACCEPTING FUNCTION FOR THE floating - point literal (FPL)*/

/*
*Purpose:
*Author: Exequiel Repetto 040885774
*History/Versions: 1.0
*Called functions: atof(), strlen()
*Parameters:
*Return value:
*Algorithm:
*/

Token aa_func08(char lexeme[]) {
	/* If lexeme is null return run-time error token */
	if (!lexeme)
		return generateErrorToken();

	Token t;

	double num = atof(lexeme);
	size_t i = 0;

	if (num <= FLT_MAX  && num >= FLT_MIN || num == 0) {
		t.code = FPL_T;
		t.attribute.flt_value = (float)num;
	}

	else {

		if (strlen(lexeme) > ERR_LEN) {
			for (i = 0; i < ERR_LEN - 3; i++)
				t.attribute.err_lex[i] = lexeme[i];
			for (; i < ERR_LEN; i++)
				t.attribute.err_lex[i] = '.';
			t.attribute.err_lex[i] = '\0';
		}
		else {
			for (i = 0; i < strlen(lexeme); i++)
				t.attribute.err_lex[i] = lexeme[i];
			t.attribute.err_lex[i] = '\0';
		}
		t.code = ERR_T;
	}
	return t;

}

/*	ACCEPTING FUNCTION FOR THE integer literal(IL)-decimal constant(DIL)*/

/*
*Purpose:
*Author: Exequiel Repetto 040885774
*History/Versions: 1.0
*Called functions: atol(), strlen()
*Parameters:
*Return value:
*Algorithm:
*/
Token aa_func05(char lexeme[]) {
	/* If lexeme is null return run-time error token */
	if (!lexeme)
		return generateErrorToken();

	Token t;
	long num = atol(lexeme);
	size_t i = 0;

	if (num <= SHRT_MAX && num >= SHRT_MIN) {
		t.code = INL_T;
		t.attribute.int_value = (short)num;
	}

	else {
		if (strlen(lexeme) > ERR_LEN) {
			for (i = 0; i < ERR_LEN - 3; i++)
				t.attribute.err_lex[i] = lexeme[i];
			for (; i < ERR_LEN; i++)
				t.attribute.err_lex[i] = '.';
			t.attribute.err_lex[i] = '\0';
		}
		else {
			for (i = 0; i < strlen(lexeme); i++)
				t.attribute.err_lex[i] = lexeme[i];
			t.attribute.err_lex[i] = '\0';
		}
		t.code = ERR_T;
	}
	return t;

}

/*
* Purpose: Accepting state function for string literals. Inserts identified 
*	string literal inside string literal table. 
* Author: Gabriel Richard, 040-880-482
* History/Versions: 1.0
* Called functions: b_limit(), strlen(), b_addc()
* Parameters: char lexeme[] - No restrictions
* Return value: Returns a string literal token
* Algorithm: Loops through passed lexeme one character at a time and inserts
*	characters inside string literal table. Does not store quotation marks inside
*	string literal table.
*/

Token aa_func10(char lexeme[]) {
	/* If lexeme is null return run-time error token */
	if (!lexeme || !str_LTBL)
		return generateErrorToken();

	Token t;
	short offset = b_limit(str_LTBL);
	/* Check if b_limit returned a valid value */
	if (offset == RT_FAIL_1)
		return generateErrorToken();
	/* Set token string offset to location of last inserted character */
	t.attribute.str_offset = offset;

	/* Add characters inside quotation marks to string literal table */
	for (size_t i = 1; i < strlen(lexeme) - 1; ++i) {
		/* Increment the line number of the source code if line break is found */
		if (lexeme[i] == '\n') 
			++line;
		/* Attempt to insert characters into string literal table */
		if (!b_addc(str_LTBL, lexeme[i])) 
			return generateErrorToken();
	}
	/* Attempt to insert null terminator into string literal table */
	if (!b_addc(str_LTBL, '\0'))
		return generateErrorToken();

	t.code = STR_T;
	return t;
}

/*
* Purpose: Accepting state function for error token. Stores the lexeme 
*	responsible for the error before returning an error token. 
* Author: Gabriel Richard, 040-880-482
* History/Versions: 1.1
* Called functions: strlen()
* Parameters: char lexeme[] - No restrictions
* Return value: Returns an error token
* Algorithm: Loops through the lexeme passed on character at a time up to 20 
*	characters and stores it inside the error token. If the lexeme passed is 
*	longer than 20 characters, only the first 17 characters are stored,  
*	appended by three dots (...) at the end. Returns the error token once the
*	loop is completed.
*/

Token aa_func12(char lexeme[]) {
	/* If lexeme is null return run-time error token */
	if (!lexeme)
		return generateErrorToken();

	Token t;
	/* Place three dots at the end of the lexeme if longer than 20 characters */
	if (strlen(lexeme) > ERR_LEN) {
		for (int i = 17; i < 20; ++i) 
			lexeme[i] = '.';
	}

	/* Strings longer than 20 characters shall only show the first 17 characters
	and append three dots (...) to the end */
	for (size_t i = 0; i <= ERR_LEN || i <= strlen(lexeme); ++i) {
		/* Increment the line number of the source code if line break is found */
		if (lexeme[i] == '\n') 
			++line;
		/* Insert null terminator at the end of lexeme */
		else if (i == ERR_LEN || i == strlen(lexeme)) 
			t.attribute.err_lex[i] = '\0';
		/* Insert lexeme character by character into token */
		else 
			t.attribute.err_lex[i] = lexeme[i];
	}

	t.code = ERR_T;
	return t;
} 

/*Purpose:
*Author: Gabriel Richard [student num], Exequiel Repetto 040885774
*History/Versions: 1.0
*Called functions: strcmp()
*Parameters:
*Return value:
*Algorithm:
*/
int iskeyword(char * kw_lexeme) {
	/* If kw_lexeme is null return -1 */
	if (!kw_lexeme)
		return RT_FAIL_2;

	for (int i = 0; i < KWT_SIZE; i++) {
		if (strcmp(kw_lexeme, kw_table[i]) == 0)
			return i;
	}
	return -1;
} /* end iskeyword function */

/*Purpose:
*Author: Gabriel Richard, Exequiel Repetto 040885774
*History/Versions: 1.0
*Called functions: b_mark, b_getcoffset(), b_getc(), b_reset()
*Parameters:
*Return value:
*Algorithm:
*/
int isAndOr() {
	/* Check if b_mark was successful */
	if (b_mark(sc_buf, b_getcoffset(sc_buf)) == RT_FAIL_1)
		return RT_FAIL_1;

	unsigned char c;
	int isWord;
	
	c = b_getc(sc_buf);

	switch (c) {

	case 'A':
		c = b_getc(sc_buf);
		if (c != 'N')
			break;
		c = b_getc(sc_buf);
		if (c != 'D')
			break;
		c = b_getc(sc_buf);
		if (c != '.')
			break;
		else
			return isWord = 1;

	case 'O':
		c = b_getc(sc_buf);
		if (c != 'R')
			break;
		c = b_getc(sc_buf);
		if (c != '.')
			break;
		else
			return isWord = 2;
	}
	/* Check if b_reset was successful */
	if (b_reset(sc_buf) == RT_FAIL_1)
		return RT_FAIL_1;

	return 0;

} /* end isAndOr function */

Token generateErrorToken() {
	++scerrnum;
	Token t;

	char error[] = "RUN TIME ERROR: ";
	for (size_t i = 0; i <= strlen(error); ++i) {
		/* Insert null terminator at the end to create c-style string */
		if (i == strlen(error)) 
			t.attribute.err_lex[i] = '\0';
		/* Insert string characters inside token */
		t.attribute.err_lex[i] = error[i];
	}
	t.code = RTE_T;
	return t;
} /* end generateErrorToken function */