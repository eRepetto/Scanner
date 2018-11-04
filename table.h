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


/*REPLACE *ESN* and *ESR* WITH YOUR ERROR STATE NUMBER*/

/*I put 9 and 10 here I dont know if it is us who decide this number, I tried to chose something that is not repeating*/
#define ES  12 /* Error state  with no retract */
#define ER  10 /* Error state  with retract */
#define IS -1    /* Inavalid state */

/* State transition table definition */

/*REPLACE *CN* WITH YOUR COLUMN NUMBER*/

#define TABLE_COLUMNS 7
/*transition table - type of states defined in separate table */

int  st_table[][TABLE_COLUMNS] = {
	/* State 0 */ {1, 6, 4, ES, ES, 9, ES},
	/* State 1 */ {1, 1, 1, ES, 3, ES, 2},
	/* State 2 */ {IS, IS, IS, IS, IS, IS, IS},
	/* State 3 */ {IS, IS, IS, IS, IS, IS, IS},
	/* State 4 */ {ES, 4, 4, 7, 5, ES, 5},
	/* State 5 */ {IS, IS, IS, IS, IS, IS, IS},
	/* State 6 */ {ES, 6, ES, 7, ES, ES, 5},  /* Where there was a choice of states, I put ES for the time being */
	/* State 7 */ {ES, 7, 7, 8, 8, ES, 8},
	/* State 8 */ {IS, IS, IS, IS, IS, IS, IS},
	/* State 9 */ {9, 9, 9, 9, 9, 10, 9}, 
	/* State 10 */{IS, IS, IS, IS, IS, IS}, 
	/* State 11 */{IS, IS, IS, IS, IS, IS},
	/* State 12 */{IS, IS, IS, IS, IS, IS},
	/* State 13 */{IS, IS, IS, IS, IS, IS} /*Reserved for future use*/ /*I added IS to complete the table and be able to run the program*/

};
/*.YOUR TABLE INITIALIZATION HERE*/



/* Accepting state table definition */
/*REPLACE *N1*, *N2*, and *N3* WITH YOUR NUMBERS*/
#define ASWR     11  /* accepting state with retract */
#define ASNR     12 /* accepting state with no retract */
#define NOAS     13  /* not accepting state */


/*I put ASWR for state number 10, I don't know if is that what we need to use, I just put it for testing purpose*/
int as_table[] = { 
	NOAS,NOAS,ASWR,ASNR,NOAS,ASWR,NOAS,
	NOAS,ASWR,NOAS,ASNR,ASNR,ASWR 
};

/* Accepting action function declarations */

/*FOR EACH OF YOUR ACCEPTING STATES YOU MUST PROVIDE
ONE FUNCTION PROTOTYPE.THEY ALL RETURN Token AND TAKE
ONE ARGUMENT : A string REPRESENTING A TOKEN LEXEME.*/

/*Token aa_funcXX(char *lexeme);*/

/*Replace XX with the number of the accepting state : 02, 03 and so on;*/


Token aa_func02(char *lexeme);
Token aa_func03(char *lexeme);
Token aa_func05(char *lexeme);
Token aa_func08(char *lexeme);
Token aa_func10(char *lexeme); /* string literal?*/
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

PTR_AAF aa_table[] = { 
	NULL, NULL, aa_func02, aa_func03, NULL, aa_func05, NULL, 
	NULL, aa_func08, NULL, aa_func10, NULL,aa_func12, NULL 
};

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
