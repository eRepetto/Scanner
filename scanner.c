/* Filename: scanner.c
/* PURPOSE:
*    SCANNER.C: Functions implementing a Lexical Analyzer (Scanner)
*    as required for CST8152, Assignment #2
*    scanner_init() must be called before using the scanner.
*    The file is incomplete;
*    Provided by: Svillen Ranev
*    Version: 1.18.2
*    Date: 1 October 2018
*******************************************************************
*    REPLACE THIS HEADER WITH YOUR HEADER
*******************************************************************
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
					   /* No other global variable declarations/definitiond are allowed */

					   /* scanner.c static(local) function  prototypes */
static int char_class(char c); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
static int iskeyword(char * kw_lexeme); /*keywords lookup functuion */
static int isAndOr();


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

Token malar_next_token(void)
{
	Token t = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
	unsigned char c; /* input symbol */
	int state = 0; /* initial state of the FSM */
	short lexstart = 0;  /*start offset of a lexeme in the input char buffer (array) */
	short lexend = 0;    /*end   offset of a lexeme in the input char buffer (array)*/
	int accept = NOAS; /* type of state - initially not accepting */
	int temp = 0; /*used to store temporary integers*/

				  /*	DECLARE YOUR LOCAL VARIABLES HERE IF NEEDED*/

	while (1) { /* endless loop broken by token returns it will generate a warning */

				/*	GET THE NEXT SYMBOL FROM THE INPUT BUFFER*/

		c = b_getc(sc_buf);

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
			temp = isAndOr(); /* if function return 1 is .AND. if return 2 is .OR.*/
			if (temp) {
				/*lexeme is .AND.*/
				if (temp == 1)
					t.attribute.log_op = AND;
				/*lexeme is .OR.*/
				else if (temp == 2)
					t.attribute.log_op = OR;

				t.code = LOG_OP_T;
				return t;
			}
		}

		/* Part 2: Implementation of Finite State Machine (DFA)*/
		lexstart = b_mark(sc_buf, b_getcoffset(sc_buf) - 1);
		short capacity;

		while (accept == NOAS) {
			state = get_next_state(state, c, &accept);
			if (accept == ASWR || accept == ASNR)
				break;
			c = b_getc(sc_buf);

		}
		if (accept == ASWR)
			b_retract(sc_buf);

		lexend = b_getcoffset(sc_buf);
		capacity = lexend - lexstart;

		lex_buf = b_allocate(capacity + 1, 0, 'f');

		/*retract getc_offset to the mark set previously*/
		b_reset(sc_buf);
		for (int i = lexstart; i < lexend; i++) {
			c = b_getc(sc_buf);
			b_addc(lex_buf, c);
		}

		b_addc(lex_buf, '\0');
		t = aa_table[state](b_location(lex_buf, 0));

		b_free(lex_buf);
		return t;

	}//end while(1)
}


/*DO NOT MODIFY THE CODE OF THIS FUNCTION
YOU CAN REMOVE THE COMMENTS*/

int get_next_state(int state, char c, int *accept)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif
	/*
	The assert(int test) macro can be used to add run-time diagnostic to programs
	and to "defend" from producing unexpected results.
	assert() is a macro that expands to an if statement;
	if test evaluates to false (zero) , assert aborts the program
	(by calling abort()) and sends the following message on stderr:
	Assertion failed: test, file filename, line linenum
	The filename and linenum listed in the message are the source file name
	and line number where the assert macro appears.
	If you place the #define NDEBUG directive ("no debugging")
	in the source code before the #include <assert.h> directive,
	the effect is to comment out the assert statement.
	*/
	assert(next != IS);

	/*
	The other way to include diagnostics in a program is to use
	conditional preprocessing as shown bellow. It allows the programmer
	to send more details describing the run-time problem.
	Once the program is tested thoroughly #define DEBUG is commented out
	or #undef DEBUF is used - see the top of the file.
	*/
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



/*	HERE YOU WRITE THE DEFINITIONS FOR YOUR ACCEPTING FUNCTIONS.
************************************************************
ACCEPTING FUNCTION FOR THE arithmentic variable identifier AND keywords(VID - AVID / KW)
REPLACE XX WITH THE CORRESPONDING ACCEPTING STATE NUMBER*/

/*
* Author: Exequiel Repetto
*/
Token aa_func02(char lexeme[]) {

	/*WHEN CALLED THE FUNCTION MUST
	1. CHECK IF THE LEXEME IS A KEYWORD.
	IF YES, IT MUST RETURN A TOKEN WITH THE CORRESPONDING ATTRIBUTE
	FOR THE KEYWORD.THE ATTRIBUTE CODE FOR THE KEYWORD
	IS ITS INDEX IN THE KEYWORD LOOKUP TABLE(kw_table in table.h).
	IF THE LEXEME IS NOT A KEYWORD, GO TO STEP 2.
	2. SET a AVID TOKEN.
	IF THE lexeme IS LONGER than VID_LEN(see token.h) CHARACTERS,
	ONLY FIRST VID_LEN CHARACTERS ARE STORED
	INTO THE VARIABLE ATTRIBUTE ARRAY vid_lex[](see token.h) .
	ADD \0 AT THE END TO MAKE A C - type STRING.
	return t;*/

	Token t;

	int temp = iskeyword(lexeme);

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

/* ACCEPTING FUNCTION FOR THE string variable identifier(VID - SVID) */

/*
* Author: Gabriel Richard
*/
Token aa_func03(char lexeme[]) {
	Token t;
	t.code = SVID_T;
	int lenght = (strlen(lexeme));

	if (lenght > VID_LEN) {
		/* Only first vid_len - 1 characters are stored, followed by '$' and '\0' */
		for (int i = 0; i < VID_LEN; ++i) {
			if (i == VID_LEN - 1) {
				t.attribute.vid_lex[i] = '$';
				t.attribute.vid_lex[i + 1] = '\0';
			}
			else
				t.attribute.vid_lex[i] = lexeme[i];
		}
	}
	else {
		for (int i = 0; i < lenght - 1; i++)
			t.attribute.vid_lex[i] = lexeme[i];
		t.attribute.vid_lex[lenght - 1] = '$';
		t.attribute.vid_lex[lenght] = '\0';
	}

	/*b_getc(sc_buf); I added this because DFA is getting the variable name but not the  $ symbol so i am jumping this character*/
	return t;
}

/*ACCEPTING FUNCTION FOR THE floating - point literal (FPL)*/

/*
* Author: Exequiel Repetto
*/
Token aa_func08(char lexeme[]) {

	Token t;
	
	double num = atof(lexeme);
	int i = 0;

	if (num <= FLT_MAX  && num >= FLT_MIN  || num == 0)  {
		t.code = FPL_T;
		t.attribute.flt_value = (float) num;
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
* Author: Exequiel Repetto
*/
Token aa_func05(char lexeme[]) {

	Token t;
	long num = atol(lexeme);
	int i = 0;

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
* ACCEPTING FUNCTION FOR THE string literal(SL)
*
* Author: Gabriel Richard
*/
Token aa_func10(char lexeme[]) {
	Token t;
	t.attribute.str_offset = b_limit(str_LTBL);

	/* Add characters inside quotation marks to string literal table */
	for (size_t i = 1; i < strlen(lexeme) - 1; ++i) {
		if (lexeme[i] == '\n') {
			++line;
		}
		/* Attempt to insert characters into string literal table */
		if (!b_addc(str_LTBL, lexeme[i])) {
			break;
		}
	}
	b_addc(str_LTBL, '\0');
	t.code = STR_T;

	return t;
}

/*
* ACCEPTING FUNCTION FOR THE ERROR TOKEN
*
* Author: Gabriel Richard
*/
Token aa_func12(char lexeme[]) {
	Token t;

	/* Strings longer than 20 characters shall only show the first 17 characters
	and append three dots (...) to the end */
	if (strlen(lexeme) > ERR_LEN) {
		for (int i = 0; i <= ERR_LEN; ++i) {
			if (lexeme[i] == '\n') {
				++line;
			}
			if (i == ERR_LEN) {
				t.attribute.err_lex[i] = '\0';
			}
			else if (i >= ERR_LEN - 3) {
				t.attribute.err_lex[i] = '.';
			}
			else {
				t.attribute.err_lex[i] = lexeme[i];
			}
		}
	}
	else {
		for (size_t i = 0; i <= strlen(lexeme); ++i) {
			if (lexeme[i] == '\n') {
				++line;
			}
			if (i == strlen(lexeme)) {
				t.attribute.err_lex[i] = '\0';
			}
			else {
				t.attribute.err_lex[i] = lexeme[i];
			}
		}
	}
	t.code = ERR_T;
	return t;
}

/*should we add function 11 as well? not sure about it yet*/


/*HERE YOU WRITE YOUR ADDITIONAL FUNCTIONS(IF ANY).
FOR EXAMPLE*/

int iskeyword(char * kw_lexeme) {

	/*printf("%s", kw_lexeme);*/

	for (int i = 0; i < KWT_SIZE; i++) {
		if (strcmp(kw_lexeme, kw_table[i]) == 0)
			return i;
	}
	return -1;
}


int isAndOr() {
	b_mark(sc_buf, b_getcoffset(sc_buf));
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

	b_reset(sc_buf);
	return 0;

}