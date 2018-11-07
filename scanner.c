/*
*File name: scanner.c
*Compiler: MS Visual Studio 2015
*Author: Gabriel Richard [student number], Exequiel Repetto, 040885774
*Course: CST 8152 – Compilers, Lab Section: 11
*Assignment: 2
*Date: 2018-11-08
*Professor: Sv. Ranev
*Purpose:
*Function list: scanner_init(), malar_next_token(), get_next_state(),char_class(),aa_func02(), aa_func03(), aa_func08(), aa_func05(),
aa_func10(), aa_func12(), isKeyword(), isAndOr();
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
static int isAndOr(); /*.AND., .OR. lookup function*/


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
	int temp = 0; /*used to store temporary integers*/

	while (1) { /* endless loop broken by token returns it will generate a warning */

				/* Get the next symbol from the buffer */
		c = b_getc(sc_buf);
		/* Compare character to simple token patterns */
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
			/*if c is equal to '=' is a comment line*/
			if (c == '!') {
				/*loop will iterate until end of line*/
				while (c != '\n')
					c = b_getc(sc_buf);
				line++;
				continue;
			}
			/*not comment line error token is set*/
			else {
				t.attribute.err_lex[0] = '!';
				t.attribute.err_lex[1] = c;
				/*ignores everything until the end of line*/
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
			/*mathematical operators*/
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
			/*relational operators*/
		case '<':
			c = b_getc(sc_buf);
			/*if following character is '>' relational operator is '<>'*/
			if (c == '>')
				t.attribute.rel_op = NE;
			/*it retracts back one character*/
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
			/*if following character is '=' is relational operator*/
			if (c == '=') {
				t.code = REL_OP_T;
				t.attribute.rel_op = EQ;
			}
			/*character is an assignment operator*/
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
		short capacity; /*variable used to store the capacity of the lex_buf*/

						/*loop iterate until final state is accepting state*/
		while (accept == NOAS) {
			state = get_next_state(state, c, &accept);
			/*if final state is accepting it will break the loop*/
			if (accept == ASWR || accept == ASNR)
				break;
			c = b_getc(sc_buf);
		}
		/*if final state is accepting state with retract*/
		if (accept == ASWR)
			/* retracts the buffer one character*/
			b_retract(sc_buf);

		lexend = b_getcoffset(sc_buf);
		capacity = lexend - lexstart;

		lex_buf = b_allocate(capacity + 1, 0, 'f');

		/*retract getc_offset to the mark set previously*/
		b_reset(sc_buf);
		/*loop will iterate adding character buffer to the new allocated lex_buf*/
		for (int i = lexstart; i < lexend; i++) {
			c = b_getc(sc_buf);
			b_addc(lex_buf, c);
		}

		b_addc(lex_buf, '\0');
		t = aa_table[state](b_location(lex_buf, 0));

		b_free(lex_buf);
		return t;

	} //end while(1)
} // end malar_next_token


  /*
  *Purpose: the function is going to return the next state from the transition table
  *Author: Sv. Ranev
  *History/Versions:1.0
  *Called functions: char_class()
  *Parameters: int state - must be between 0 and 12; char c -
  *Return value:
  *Algorithm:
  */
int get_next_state(int state, char c, int *accept)
{
	int col; /*variable stores column number*/
	int next; /*variable stores next state*/
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif

	assert(next != IS);

#ifdef DEBUG
	/*if next is -1 it will print an error message and exit*/
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
*Purpose: function is going to get the column of the transition table according the character passed to the parameter
*Authors: Gabriel Richard, 040-880-482; Exequiel Repetto, 040-885-774
*History/Versions: 1.0
*Called functions: isalpha(), isdigit()
*Parameters: type char
*Return value: type int
*Algorithm: function runs trough if statements comparing char c with each character from the column table,
if both character math function is going to return the specific column number, else it will return number 7
*/
int char_class(char c)
{
	int column; /* variable stores the column number of the transition table*/

				/* if c is a letter assigns 0 to column*/
	if (isalpha(c))
		column = 0;
	/* if c is '0' assigns 1 to column*/
	else if (c == '0')
		column = 1;
	/* if c is a digit assigns 2 to column*/
	else if (isdigit(c))
		column = 2;
	/*if c is a dot ('.') assigns 3 to column*/
	else if (c == '.')
		column = 3;
	/*if c is character '&' assigns 4 to column*/
	else if (c == '$')
		column = 4;
	/*if c is equal to '""' assigns 5 to column*/
	else if (c == '"')
		column = 5;
	/*if c is a null terminator ('\0') or 255 it assigns 6 to column*/
	else if (c == '\0' || c == 255)
		column = 6;
	/*if c doesn't match any of the conditions it assigns 7 to column*/
	else
		column = 7;

	return column;
}

/*
ACCEPTING FUNCTION FOR THE arithmetic variable identifier AND keywords(VID - AVID / KW)

/*
*Purpose: the purpose of the function is to identified a variable name or a keyword from the platypus language
*Author: Exequiel Repetto 040-885-774
*History/Versions: 1.0
*Called functions: isKeyword(), strlen()
*Parameters: type char
*Return value: type Token
*Algorithm: function is going to call isKeyword() function with the lexeme as parameter to compare the lexeme with an array of keywords name, if lexeme is
a keyboard function is going to return the index from the array else it will return -1, if is keyboard is going to set a token code and set the index as attribute
if lexeme is not a keyboard is going to set it as a variable name, if lexeme is not longer than VID_LEN is going to set a token code and add the lexeme to the specific
attribute. If lexeme is longer than VID_LEN only the number of VID_LEN characters are going to be stored
*/

Token aa_func02(char lexeme[]) {

	Token t;
	int temp = iskeyword(lexeme); /*variable store the index of the array keyword*/

								  /*if temp is not equal to -1, the lexeme is a keyword*/
	if (temp != -1) {
		t.code = KW_T;
		t.attribute.kwt_idx = temp;

	}
	/*if lexeme is not a keyword, and is longer than VID_LEN*/
	else if ((strlen(lexeme) > VID_LEN)) {
		t.code = AVID_T;
		/*loop is going to add each character of the lexeme to the attribute vid_lex*/
		for (int i = 0; i < VID_LEN; i++) {
			t.attribute.vid_lex[i] = lexeme[i];
			/*if loop reached the end of iteration it will add a null terminator*/
			if (i + 1 == VID_LEN)
				t.attribute.vid_lex[i + 1] = '\0';
		}
	}
	/*if lexeme is shorter than VID_LEN*/
	else {
		t.code = AVID_T;
		/*loop is going to add each characther of the lexeme to the attribute vid_lex*/
		for (size_t i = 0; i < strlen(lexeme); i++) {
			t.attribute.vid_lex[i] = lexeme[i];
			/*if loop reached the end of iteration it will add a null terminator*/
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
	Token t;
	t.code = SVID_T;

	/*
	* Only store first 7 characters in token if lexeme is longer then 8
	* characters.
	*/
	if (strlen(lexeme) > VID_LEN) {
		for (int i = 0; i < VID_LEN; ++i) {

			/*
			* '$' followed by null terminator ('\0') is appended to the
			* end of SVID.
			*/
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
*Purpose: The purpose of the function is to convert the lexeme representing to a floating point value, set the specific token code and attribute
*Author: Exequiel Repetto 040-885-774
*History/Versions: 1.0
*Called functions: atof(), strlen()
*Parameters: type char, function expects to receive an array of char
*Return value: type Token
*Algorithm: the lexeme is converted to a floating point value, if the value converted is not greater or less than an 4 bytes floating point
FPL_T token code is set and the value is store in the flt_value attribute. if value is greater or less than the accepting 4 bytes floating point,
an error token will be set and value is going to be store in the err_lex attribute, if the length of the number is greater than
ERR_LEN only the first ERR_LEN -3 characters are going to be store followed by three dots (.)
*/

Token aa_func08(char lexeme[]) {

	Token t;

	double num = atof(lexeme);/*variable stores the lexeme converted to float*/
	size_t i = 0; /*used as index in loop*/

				  /*checks if num is inside the range of a float*/
	if (num <= FLT_MAX  && num >= FLT_MIN || num == 0) {
		t.code = FPL_T;
		t.attribute.flt_value = (float)num;
	}
	/*if num is not in the range of a floating point number*/
	else {
		/*if length of the lexeme is longer than ERR_LEN*/
		if (strlen(lexeme) > ERR_LEN) {
			/*loop will iterate until add ERR_LEN-3 characters*/
			for (i = 0; i < ERR_LEN - 3; i++)
				t.attribute.err_lex[i] = lexeme[i];
			/*loop will add three dots ('.') at the end of the array*/
			for (; i < ERR_LEN; i++)
				t.attribute.err_lex[i] = '.';
			t.attribute.err_lex[i] = '\0';
		}
		/*if lexeme is not longer than ERR_LEN*/
		else {
			/*loop is going to add the lexeme to the array attribute*/
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
*Purpose: The purpose of the function is to convert the lexeme representing to a decimal constant, set the specific token code and attribute
*Author: Exequiel Repetto 040-885-774
*History/Versions: 1.0
*Called functions: atol(), strlen()
*Parameters: type char, function expects to receive an array of char
*Return value: Type Token
*Algorithm: the lexeme is converted to a decimal value, if the value converted is not greater or less than an 2 bytes integer
INL_T token is set and the value is store in the int_value attribute. if value is greater or less than the accepting 2 bytes integer,
an error token will be set and value is going to be store in the err_lex attribute, if the length of the number is greater than
ERR_LEN only the first ERR_LEN -3 characters are going to be store followed by three dots (.)
*/
Token aa_func05(char lexeme[]) {

	Token t;
	long num = atol(lexeme);
	size_t i = 0;

	/*check if num is in the range of a 2 byte int*/
	if (num <= SHRT_MAX && num >= SHRT_MIN) {
		t.code = INL_T;
		t.attribute.int_value = (short)num;
	}
	/*if num is not in the range of a 2 bytes int*/
	else {
		/*if the length of the lexeme is longer than ERR_LEN*/
		if (strlen(lexeme) > ERR_LEN) {
			/*loop is going to add up to ERR_LEN-3 characters to the array attribute*/
			for (i = 0; i < ERR_LEN - 3; i++)
				t.attribute.err_lex[i] = lexeme[i];
			/*loop will add three dots ('.') at the end of the array attribute*/
			for (; i < ERR_LEN; i++)
				t.attribute.err_lex[i] = '.';
			t.attribute.err_lex[i] = '\0';
		}
		/*if lexeme is not longer than ERR_LEN*/
		else {
			/*loop is going to add the lexeme to the array attribute*/
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
	Token t;
	/* Set token string offset to location of last inserted character */
	t.attribute.str_offset = b_limit(str_LTBL);

	/* Add characters inside quotation marks to string literal table */
	for (size_t i = 1; i < strlen(lexeme) - 1; ++i) {
		/* Increment the line number of the source code if line break is found */
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
	Token t;
	/* Place three dots at the end of the lexeme if longer than 20 characters */
	if (strlen(lexeme) > ERR_LEN) {
		for (int i = 17; i < 20; ++i) {
			lexeme[i] = '.';
		}
	}

	/* Strings longer than 20 characters shall only show the first 17 characters
	and append three dots (...) to the end */
	for (size_t i = 0; i <= ERR_LEN || i <= strlen(lexeme); ++i) {
		/* Increment the line number of the source code if line break is found */
		if (lexeme[i] == '\n') {
			++line;
		}
		/* Insert null terminator at the end of lexeme */
		else if (i == ERR_LEN || i == strlen(lexeme)) {
			t.attribute.err_lex[i] = '\0';
		}
		/* Insert lexeme character by character into token */
		else {
			t.attribute.err_lex[i] = lexeme[i];
		}
	}

	t.code = ERR_T;
	return t;
}

/*Purpose: function checks if lexeme is a keyword
*Author: Gabriel Richard [student num], Exequiel Repetto 040-885-774
*History/Versions: 1.0
*Called functions: strcmp()
*Parameters: type char
*Return value: type int
*Algorithm: function runs trough a loop comparing the lexeme with an array of keywords, if lexeme is keyword is going to return
the index to the array, else it will return -1
*/
int iskeyword(char * kw_lexeme) {
	/*loop will iterate to find keyword*/
	for (int i = 0; i < KWT_SIZE; i++) {
		/*if keyword is found it will return the index*/
		if (strcmp(kw_lexeme, kw_table[i]) == 0)
			return i;
	}
	return -1;
}

/*Purpose: the purpose of the function is to identified logical operators .AND. & .OR.
*Author: Gabriel Richard, Exequiel Repetto 040-885-774
*History/Versions: 1.0
*Called functions: b_mark, b_getcoffset(), b_getc(), b_reset()
*Parameters: none
*Return value: type int
*Algorithm: Function is going to set a mark at the current position in the buffer, switch statement is going to run with only 2 cases 'A' or 'O' getting a new character every time
the following character is the right character, if any of the characters is not the right one, functions is going to reset the getc_offset to the value of the mark
set at the bigging of the function
*/
int isAndOr() {
	b_mark(sc_buf, b_getcoffset(sc_buf)); /*set mark to the current position of the buffer*/
	unsigned char c; /*variable used to store character buffer*/
	int isWord = 0; /*variable stores the return number of the function*/
	c = b_getc(sc_buf);

	switch (c) {

	case 'A':
		c = b_getc(sc_buf);
		/*if c is not equal to 'N' it will break from switch statement*/
		if (c != 'N')
			break;
		c = b_getc(sc_buf);
		/*if c is not equal to 'D' it will break from switch statement*/
		if (c != 'D')
			break;
		c = b_getc(sc_buf);
		/*if c is not equal to '.' it will break from switch statement*/
		if (c != '.')
			break;
		/*lexeme is a logical operator*/
		else
			return isWord = 1;

	case 'O':
		c = b_getc(sc_buf);
		/*if c is not equal to 'R' it will break from switch statement*/
		if (c != 'R')
			break;
		c = b_getc(sc_buf);
		/*if c is not equal to '.' it will break from switch statement*/
		if (c != '.')
			break;
		/*lexeme is a logical operator*/
		else
			return isWord = 2;
	}
	b_reset(sc_buf);
	return isWord;

}