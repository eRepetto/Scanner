#pragma once
/* Filename: table.h
* Transition Table and function declarations necessary for the scanner implementation
* as required for CST8152 - Assignment #2.
* Version: 1.18.2
* Date: 1 October 2018
* Provided by: Svillen Ranev
* The file is incomplete. You are to complete it.
***************************************************
* REPLACE THIS HEADER WITH YOUR HEADER
***************************************************
*/

#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

/*   Source end-of-file (SEOF) sentinel symbol
*    '\0' or one of 255,0xFF,EOF
*/

/*  Special case tokens processed separately one by one
*  in the token-driven part of the scanner
*  '=' , ' ' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' , ';',
*  white space
*  !!comment , ',' , ';' , '-' , '+' , '*' , '/', # ,
*  .AND., .OR. , SEOF, 'illegal symbol',
*/


REPLACE *ESN* and *ESR* WITH YOUR ERROR STATE NUMBER
#define ES  *ESN* /* Error state  with no retract */
#define ER  *ESR* /* Error state  with retract */
#define IS -1    /* Inavalid state */

/* State transition table definition */

REPLACE *CN* WITH YOUR COLUMN NUMBER

#define TABLE_COLUMNS 7
/*transition table - type of states defined in separate table */
int  st_table[][TABLE_COLUMNS] = {
	/* State 0 */ { 1,6,4,ES,ES,ES},
	/* State 1 */ { 1,1,1,ES,3,2 },
	/* State 2 */ {IS,IS,IS,IS,IS,IS}
	/* State 3 */ {IS,IS,IS,IS,IS,IS}
	/* State 4 */ {ES,4,4,7,5,5}
	/* State 5 */ {IS,IS,IS,IS,IS,IS}
	/* State 6 */ { , , , 7,ES,5}  /* I don't know yet if we need to add something in that empty space*/
	/* State 7 */ {ES,7,7,ES,8,8}
	/* State 8 */ {IS,IS,IS,IS,IS,IS}
	/* State 9 */ {}  /*we need to complete this once we finish the string literal tables*/
	/* State 10 */{}  /*we need to complete this once we finish the string literal tables*/
	/* State 11 */{IS,IS,IS,IS,IS,IS}
	/* State 12 */{IS,IS,IS,IS,IS,IS}
	/* State 13 */{} /*Reserved for future use*/


/*.YOUR TABLE INITIALIZATION HERE*/



/* Accepting state table definition */
REPLACE *N1*, *N2*, and *N3* WITH YOUR NUMBERS
#define ASWR     *N1*  /* accepting state with retract */
#define ASNR     *N2*  /* accepting state with no retract */
#define NOAS     *N3*  /* not accepting state */

	int as_table[] = {NOAS,NOAS,ASWR,ASNR,NOAS,ASWR,NOAS,NOAS,ASWR,NOAS, ,ASNR,ASWR };

/* Accepting action function declarations */

FOR EACH OF YOUR ACCEPTING STATES YOU MUST PROVIDE
ONE FUNCTION PROTOTYPE.THEY ALL RETURN Token AND TAKE
ONE ARGUMENT : A string REPRESENTING A TOKEN LEXEME.

Token aa_funcXX(char *lexeme);

Replace XX with the number of the accepting state : 02, 03 and so on.


Token aa_func02(char *lexeme);
Token aa_func03(char *lexeme);
Token aa_func05(char *lexeme);
Token aa_func08(char *lexeme);
Token aa_func10(char *lexeme); /* string literal?*/
Token aa_func11(char *lexeme);
Token aa_func12(char *lexeme);



/* defining a new type: pointer to function (of one char * argument)
returning Token
*/

typedef Token(*PTR_AAF)(char *lexeme);


/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equvalent declaration is:
* Token (*aa_table[])(char lexeme[]) = {
*/



/*

HERE YOU MUST PROVIDE AN INITIALIZATION FOR AN ARRAY OF POINTERS
TO ACCEPTING FUNCTIONS.THE ARRAY HAS THE SAME SIZE AS as_table[].
YOU MUST INITIALIZE THE ARRAY ELEMENTS WITH THE CORRESPONDING
ACCEPTING FUNCTIONS(FOR THE STATES MARKED AS ACCEPTING IN as_table[]).
THE REST OF THE ELEMENTS MUST BE SET TO NULL.*/

PTR_AAF aa_table[14] = {NULL,NULL,ASWR,ASNR,NULL,ASWR,NULL,NULL,ASWR,NULL,ASWR/*not sure about this one yet*/,ASNR,ASWR,NULL};

/* Keyword lookup table (.AND. and .OR. are not keywords) */

#define KWT_SIZE  10

char * kw_table[] =
{
	"ELSE",
	"FALSE",
	"IF",
	"PLATYPUS",
	"READ",
	"REPEAT",
	"THEN",
	"TRUE",
	"WHILE",
	"WRITE"
};

#endif
