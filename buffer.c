/*File name: buffer.c
Compiler: MS Visual Studio 2015
Author: Exequiel Repetto, 040885774
Course: CST 8152 – Compilers, Lab Section: 11
Assignment: 1
Date: 2018-10-03
Professor: Sv. Ranev
Purpose: The purpose of this program is to create a buffer using dynamic structures that is able to operate in three different mode a fixed size buffer an additive self incrementing and a multiplicative self incrementing buffer
program is going to load (read) a file and store it to the buffer, with the selected parameters and mode. buffer should not load if bad parameters are chosen.
Function list: b_allocate(), b_addc(), b_clear(),b_free(),b_isfull(), b_limit(), b_capacity(), b_mark(), b_mode(), b_incfactor(), b_load(), b_isempty(), b_getc(), b_eob(),
b_print(), b_compact(), b_rflag(), b_retract(), b_reset(), b_getcoffset(), b_rewind(), b_location()
*/

#include "buffer.h"

/*
Purpose: This function creates a new buffer in memory according to the mode chosen by the user, sets the flag field to default and return pointer to buffer structure
Author: Exequiel Repetto
History/Versions: 1.0
Called functions: calloc(), malloc(), free()
Parameters: function takes three parameters: short init_capacity; this parameter it cannot be negative and it has a maximum value of SHRT_MAX-1. char inc_factor; this parameter will only accept positive values from 0 to 255.
Char o_mode; parameter will only accept three different kind of characters 'f', 'a', 'm'.
Return value: function will returns a pointer to the buffer structure
Algorithm: This function creates a new buffer in memory, allocate memory for one buffer structure and allocate memory for one dynamic character buffer.
Sets the buffer operational mode with three different integers numbers: 0 indicates that the buffer operates in fixed-size mode;
1 indicates additive self incrementing mode and -1 indicates multiplicative self incrementing mode
assigns the incremental factor to the buffer inc_factor. if operation is a failure and no value has been assigned to the buffer function will free memory allocated and return null
If everything is successful it copies the init_capacity to the buffer structure capacity variable, it will set the flag field to the default value and finally it will return a pointer to the buffer structure
*/
Buffer *b_allocate(short init_capacity, char inc_factor, char o_mode) {

	Buffer *temp; /* pointer to buffer structure */

	/*if increment is negative it will return null*/
	if (inc_factor < 0)
		return NULL;

	/* statement will check if the init_capacity is in the acceptable range else will return null*/
	if (init_capacity >= 0 && init_capacity <= MAX_BUFF) {
		temp = (Buffer*)calloc(1, sizeof(Buffer)); /* allocates memory for one Buffer structure*/
		if (temp != NULL)
			temp->cb_head = (char*)malloc(init_capacity * sizeof(char)); /*allocate memory for one dynamic character buffer*/
	}
	else
		return NULL;

	/* if allocation of memory fails  it will free the memory and return null*/
	if (!temp) {
		return NULL;
	}

	/*if o_mode is equal to character "f" and inc_factor equal to 0 it will initialize mode and inc_factor members to 0*/
	if (o_mode == 'f' || inc_factor == 0) {
		temp->mode = 0;
		temp->inc_factor = 0;
	}

	/*if o_mode is equal to character "f" and inc_factor is not equal to 0 it will initialize mode and inc_factor members to 0 (fixed size)*/
	else if (o_mode == 'f' && inc_factor != 0) {
		temp->mode = 0;
		temp->inc_factor = 0;
	}

	/*if o_mode is equal to  character "a" and inc_factor is on the range of 1 and 255 it assigns the mode to 1 (additive mode) and inc_factor to parameter value */
	else if (o_mode == 'a' && inc_factor >= FACTOR_1 && inc_factor <= FACTOR_255) {
		temp->mode = MODE_A;
		temp->inc_factor = inc_factor;
	}

	/*if o_mode is equal to character "m" and inc_factor is on the range of 1 and 100 it assigns the mode to -1 (multiplicative mode) and inc_factor to parameter value */
	else if (o_mode == 'm' && inc_factor >= FACTOR_1 && inc_factor <= FACTOR_100) {
		temp->mode = MODE_M;
		temp->inc_factor = inc_factor;
	}

	/* if any of the cases are not possible it will free the memory and return null*/
	else {
		free(temp->cb_head);
		free(temp);
		return NULL;
	}

	/* if is a fixed size mode and the initial capacity is 0 it will free memory and return null because buffer cannot be created*/
	if (temp->mode == 0 && init_capacity == 0) {
		free(temp->cb_head);
		free(temp);
		return NULL;
	}

	temp->capacity = init_capacity; /* assign init_capacity to the data member capacity of the buffer structure*/
	temp->flags = DEFAULT_FALGS; /* initialize all flags to default*/

	return temp; /* return pointer with all values*/
}

/*
Purpose: the purpose of this function is to add a character to the buffer if is not full, otherwise it will expand buffer depending the mode if is allowed to do it
Author: Exequiel Repetto
History/Versions: 1.0
Called functions:free(), realloc()
Parameters: this functions takes 2 parameters: pointer to buffer descriptor and a char data type
Return value: function returns a pointer to the buffer structure
Algorithm: function resets the flag field r_flag bit to 0 if buffer is not full it will add symbol to the character buffer, increment addc_offset by one and return pointer.
if buffer is full it will depend of the operational mode how to continue
-if operational mode is 0 it will return null
-if operational mode is 1 (additive) it will add capacity with inc_factor  (new capacity) if as result of the operation the calculation is negative or larger than the max value allowed
it will return null, if the capacity is positive but is larger than the max allowed it will assign the max allowed to the capacity
-if operational mode is -1 (multiplicative), if capacity is equal to max value it will return null, else it will calculated the available space subtracting the maximum allowed with capacity
it will calculate the increment multiplying available space with inc_factor divide by 100, the new capacity it will be capacity + new increment
if the capacity can not be incremented anymore but still smaller than the max allowed it will assign the max allowed to capacity
-if mode is 1 or -1, it will reallocate new memory with the new capacity, it checks the reallocation has been successful otherwise it will return null, if reallocation is not null
cb_head will point to the new memory allocation, set r_flag bit is set to 1, it appends symbol to cb_head, increment addc_offset by one and it assigns the new capacity to capacity member structure
*/
pBuffer b_addc(pBuffer const pBD, char symbol) {

	short available_space = 0; /*Variable use to calculate the space available in the buffer*/
	long new_Increment = 0; /*Variable use to calculate the new increment in the buffer*/
	long new_Capacity = 0; /*Variable use to calculate a new capacity for the buffer*/
	char* tempHead; /*Temporary address use to reallocate the buffer with a new capacity*/

					/* if pBD is a null pointer it will return null*/
	if (pBD == NULL)
		return NULL;

	/*resets the flag field r_flag bit to 0*/
	pBD->flags &= RESET_R_FLAG;

	/* check if the buffer is not full and adds the character symbol to it */
	if (pBD->addc_offset != pBD->capacity) {
		pBD->cb_head[pBD->addc_offset] = symbol;
		++pBD->addc_offset;
		return pBD;
	}
	/*if buffer is full it will run this statement according the mode selected */
	else {

		/* if the operational mode is 0 the function returns null*/
		if (pBD->mode == 0)
			return  NULL;

		/*if mode is 1 tries to increase the capacity by adding inc_factor to capacity */
		else if (pBD->mode == MODE_A) {
			new_Capacity = (long)pBD->capacity + ((unsigned char)pBD->inc_factor*(sizeof(char)));

			/*check if new_Capacity is not negative and it doesn't exceeds the maximum allowed positive value-1 else return null*/
			if (new_Capacity < 0 || new_Capacity > MAX_BUFF)
				return NULL;

			if (new_Capacity > 0 && new_Capacity > MAX_BUFF)
				new_Capacity = MAX_BUFF;

			/*if new_Capacity is negative it will return null*/
			if (new_Capacity < 0)
				return NULL;
		}
		/* if operational mode is multiplicative it will run the following statements */
		else if (pBD->mode == MODE_M) {

			/* if capacity can not be incremented because reached max value it will return null*/
			if (pBD->capacity == MAX_BUFF)
				return NULL;

			available_space = MAX_BUFF - pBD->capacity;
			new_Increment = (long)(available_space * (unsigned char)pBD->inc_factor) / 100;
			new_Capacity = pBD->capacity + new_Increment;

			/* if capacity can not be incremented anymore but still small that the max value allowed it will assign the max value allowed*/
			if (new_Increment == 0) {
				if (pBD->capacity < MAX_BUFF)
					new_Capacity = MAX_BUFF;
			}
		}

		/* if mode is 1 or -1 new memory will be reallocated with the new_capacity*/
		if (pBD->mode == MODE_A || pBD->mode == MODE_M) {

			tempHead = (char*)realloc(pBD->cb_head, sizeof(char)* new_Capacity); /* reallocates new memory with the new capacity */
																				 /*if memory relocation failed it will return null*/
			if (tempHead == NULL) {
				free(tempHead);
				return NULL;
			}

			else {
				pBD->cb_head = tempHead; /* cb_head now points to new memory*/
				pBD->flags = pBD->flags | SET_R_FLAG; /* r_flag bit is set to 1*/
				pBD->cb_head[pBD->addc_offset] = symbol; /* character symbol is appended to the buffer content*/
				pBD->addc_offset++; /* addc_offset is incremented by one*/
				pBD->capacity = new_Capacity; /* new capacity calculated is assigned to capacity*/
			}
		}
	}
	return pBD;
}

/*
Purpose: purpose of this function is to re-initializes all data members of the given buffer structure
Author: Exequiel Repetto
History/Versions: 1.0
Called functions: none
Parameters: This function will take a pointer to BufferDescriptor structure
Return value: return value of this function is an int which will return 0 on success and -1 on failure
Algorithm:
*/
int b_clear(Buffer * const pBD) {

	/* if pBD is a null pointer it will return -1 else it will reset members from buffer structure*/
	if (pBD != NULL) {
		pBD->addc_offset = 0;
		pBD->flags = DEFAULT_FALGS;
		pBD->getc_offset = 0;
		pBD->markc_offset = 0;
	}
	else
		return RT_FAIL_1;
	return 0;
}
/*
Purpose: the purpose of this function is to deallocated memory occupied by the character buffer
Author: Exequiel Repetto
History/Versions: 1.0
Called functions: free()
Parameters: This function will take a pointer to BufferDescriptor structure
Return value: void
Algorithm:
*/
void b_free(Buffer *const pBD) {

	/* if pBD is not a null pointer it will free the pointer*/
	if (pBD != NULL) {
		free(pBD->cb_head);
		free(pBD);
	}
}
/*
Purpose: the purpose if this function is to check if buffer is full and return a value according to that
Author: Exequiel Repetto
History/Versions: 1.0
Called functions: none
Parameters: This function will take a pointer to BufferDescriptor structure
Return value: function will return an int 1 if buffer is full or 0 if buffer is not full
Algorithm:
*/
int b_isfull(Buffer * const pBD) {

	/* if pBD is a null pointer it will return -1*/
	if (!pBD)
		return RT_FAIL_1;

	/*if buffer is full it will return 1 else it will return 0*/
	if (pBD->addc_offset >= pBD->capacity)
		return RETN_1;
	return 0;
}
/*
Purpose: the purpose of this function is to return the limit of the character buffer (spaced being used)
Author: Exequiel Repetto
History/Versions: 1.0
Called functions: none
Parameters: This function will take a pointer to BufferDescriptor structure
Return value: function return a positive value of type short
Algorithm:
*/
short b_limit(Buffer * const pBD) {

	/* if pBD is a null pointer it will return -1 else it will return addc_offset*/
	if (!pBD)
		return RT_FAIL_1;
	else
		return pBD->addc_offset;
}
/*
Purpose: The purpose of the function is to return the current capacity of the buffer
Author: Exequiel Repetto
History/Versions:1.0
Called functions: none
Parameters: This function will take a pointer to BufferDescriptor structure
Return value: function returns a value of type char -1 if it fails
Algorithm:
*/
short b_capacity(Buffer * const pBD) {

	/* if pBD is not null it will return the current capacity of the buffer else it will return -1*/
	if (pBD != NULL)
		return pBD->capacity;
	else
		return RT_FAIL_1;
}
/*
Purpose: the purpose of the function is to set markc_offset to mark and return the set mark_offset
Author: Exequiel Repetto
History/Versions: 1.0
Called functions: none
Parameters: this functions takes 2 parameters: pointer to buffer descriptor and a short data type
Return value: function returns a value of type short -1 if function fails
Algorithm:
*/
short b_mark(pBuffer const pBD, short mark) {

	/* if pBD is not null it will return the markc_offcet else it will return -1*/
	if (pBD != NULL) {
		/* if mark is on the range of 0 and addc_offcet it will be assign to markc_offcet*/
		if (mark >= 0 && mark <= pBD->addc_offset) {
			pBD->markc_offset = mark;
			return pBD->markc_offset;
		}
	}
	return RT_FAIL_1;
}
/*
Purpose: the purpose of the function is to return the value of the mode
Author: Exequiel Repetto
History/Versions: 1.0
Called functions: none
Parameters: This function will take a pointer to BufferDescriptor structure
Return value: function returns a value of type int -1 if function fails
Algorithm:
*/
int b_mode(Buffer* const pBD) {

	/* if pBD is not a null pointer it will return the mode else it will return -1*/
	if (pBD != NULL)
		return pBD->mode;
	else
		return RT_FAIL_2;
}
/*
Purpose: the purpose of the function is to return the none negative value of inc_factor
Author: Exequiel Repetto
History/Versions: 1.0
Called functions: none
Parameters: This function will take a pointer to BufferDescriptor structure
Return value: the return type of the function is a unsigned char
Algorithm:
*/
size_t b_incfactor(Buffer * const pBD) {

	/*if pBD is not null it will return the inc_factor else it will return -1*/
	if (pBD != NULL) {
		return (unsigned char)pBD->inc_factor;
	}
	else
		return INC_FAIL;
}
/*
Purpose: The function loads (reads) an open input file specified by fi into a buffer specified by pBD.
Author: Exequiel Repetto
History/Versions: 1.0
Called functions: fgetc(), feof(), b_addc(), ungetc()
Parameters: this functions takes 2 parameters: pointer to buffer descriptor and a FILE pointer
Return value: function returns a value of type int -1 if function fails
Algorithm: function will check if both pointer that take as parameter are not null else it will return -1
-while loop it will get character from file and store it in buff variable, if end of file is reached it will break from the loop, it will add character to the buffer
if function cannot add character it will get the last character read from file and print it and return -2
-function returns the number of character added to the buffer if is successful
*/
int b_load(FILE * const fi, Buffer* const pBD) {

	char buff; /*Variable used to store one character at the time from file*/
	int t = RETN_1; /* use in a while loop as true*/

					/* if pBD is a null pointer or parameter fi is null it will return -1*/
	if (pBD == NULL || fi == NULL) {
		return RT_FAIL_1;
	}
	/* while loop will get character from file and add it to the buffer until end of file detected*/
	while (t) {
		buff = (char)fgetc(fi);
		/*if end of file detected it will break from loop*/
		if (feof(fi))
			break;
		/* if character can not be added to the buffer it will print last character read and return -2*/
		if (!b_addc(pBD, buff)) {
			ungetc(buff, fi);
			printf("The last character read from the file is: %c %d\n", buff, buff);
			return LOAD_FAIL;
		}
	}
	return pBD->addc_offset;
}
/*
Purpose: the purpose of this function is to check if the buffer is empty
Author: Exequiel Repetto
History/Versions: 1.0
Called functions: none
Parameters: This function will take a pointer to BufferDescriptor structure
Return value: function returns a variable of type int
Algorithm:
*/
int b_isempty(Buffer *const pBD) {

	/* if pBD is a null pointer it will return -1*/
	if (pBD != NULL) {
		/* if addc_offset is equal to 0 it will return 1 else it returns 0*/
		if (pBD->addc_offset == 0)
			return RETN_1;
		else
			return 0;
	}
	else
		return RT_FAIL_1;
}
/*
Purpose: the purpose of this function is to read the buffer and return its content character by character
Author: Exequiel Repetto
History/Versions: 1.0
Called functions: none
Parameters: This function will take a pointer to BufferDescriptor structure
Return value: function returns a value of type char, -2 if function fails
Algorithm:
*/
char b_getc(Buffer * const pBD) {

	/* if pBD is null it will return -2*/
	if (pBD != NULL) {
		/*if getc_offset and addc_offset are equal, eob flag is set to 1 and return 0*/
		if (pBD->getc_offset == pBD->addc_offset) {
			pBD->flags |= SET_EOB;
			return 0;
		}
		/* set eob flag to 0 increments getc_offset and return character located at getc_offset*/
		else
			pBD->flags &= RESET_EOB;
		return	pBD->cb_head[pBD->getc_offset++];
	}
	else
		return RT_FAIL_2;
}
/*
Purpose: the purpose of this function is to return the eob bit value
Author: Exequiel Repetto
History/Versions: 1.0
Called functions: none
Parameters: This function will take a pointer to BufferDescriptor structure
Return value: function returns a value of type int -1 if it fails
Algorithm:
*/
int b_eob(Buffer * const pBD) {

	/* if pBD is a null pointer it will return -1*/
	if (!pBD) {
		return RT_FAIL_1;
	}
	return pBD->flags & CHECK_EOB; /* it returns the eob flag value*/
}
/*
Purpose: the purpose of the function is to print character by character the contents of the character buffer
Author: Exequiel Repetto
History/Versions: 1.0
Called functions: b_getc(), b_eob()
Parameters: This function will take a pointer to BufferDescriptor structure
Return value: function returns a value of type int -1 if it fails
Algorithm: if pointer to buffer structure is null it will return -1
-if addc_offset is equal to 0 that means that the buffer is empty, it will print message and return -1
-else a do while loop with get character from buffer and stored it in temp variable, eob bit value is stored in eob variable, if value is 1 it will break the loop
print value in temp variable (character got from buffer), increment counter by one
-return counter (number of character printed)
*/
int b_print(Buffer * const pBD) {

	char temp; /*variable use to store one character at the time from buffer */
	int counter = 0; /*variable use to return the number of character printed*/
	int eob; /*variable use to store the current value of the eob flag*/
	int t = 1; /* variable use for a while loop as true*/

			   /* if pBD is a null pointer it will return -1*/
	if (!pBD)
		return RT_FAIL_1;

	/*check if buffer is empty and return -1*/
	if (pBD->addc_offset == 0) {
		printf("Empty buffer!\n");
		return NULL;
	}
	else {
		/*loop will get character by character from buffer and print it until eob flag is 1 (end of buffer)*/
		do {
			temp = b_getc(pBD);/* get character from buffer and assign it to the temp variable*/
			eob = b_eob(pBD); /*eob bit value is assign to the variable*/
							  /*if eob flag is equal to 1 it will break loop*/
			if (eob == RETN_1)
				break;
			printf("%c", temp);
			++counter;
		} while (t);
		printf("\n");
	}
	return counter; /*returns the number of character printed*/
}
/*
Purpose: the purpose of the function is to shrinks (or in some cases may expand) the buffer to a new capacity
Author: Exequiel Repetto
History/Versions: 1.0
Called functions: realloc()
Parameters: this functions takes 2 parameters: pointer to buffer descriptor and a char data type
Return value: returns a pointer to buffer descriptor
Algorithm: if pointer of buffer descriptor is null it will return null
-new capacity is addc_offset + 1
-reallocates new memory with new capacity
-if reallocation fails it returns null
-cb_head points to new memory and new_capcity is assign to capacity
-symbol is appended to cb_head
-addc_offset is incremented by one
-if cb_head doesn't point to the new memory location it will set r_flag bit value to 1
-function returns pointer to buffer descriptor
*/
Buffer * b_compact(Buffer * const pBD, char symbol) {

	/* if pBD is a null pointer it will return null */
	if (pBD == NULL)
		return NULL;

	short new_Capacity = (pBD->addc_offset + RETN_1) * sizeof(char); /* variable use to store the new capacity */
	char* temp = NULL; /* variable use to reallocate with the new capacity and the content already in cb_head*/

	temp = (char*)realloc(pBD->cb_head, sizeof(char*)* new_Capacity); /* reallocates new memory with new capacity*/

																	  /*if memory reallocation fails it will return null*/
	if (!temp) {
		return NULL;
	}

	pBD->cb_head = temp; /*cb_head points to new memory*/
	pBD->capacity = new_Capacity; /*new_capacity it assigns to the capacity */

	if (pBD != NULL) {
		pBD->cb_head[pBD->addc_offset] = symbol; /* symbol is added at the end of the character buffer*/
		++pBD->addc_offset; /* addc_offset is incremented by one*/
	}
	else
		return NULL;

	/*it sets the r_flag*/
	if (pBD->cb_head != temp)
		pBD->flags |= SET_R_FLAG;
	return pBD;
}
/*
Purpose: the purpose of the function is to return the r_flag bit value
Author: Exequiel Repetto
History/Versions: 1.0
Called functions: none
Parameters: This function will take a pointer to BufferDescriptor structure
Return value: function returns a value of type char
Algorithm:
*/
char b_rflag(Buffer *const pBD) {

	/* if pBD is a null pointer it will return -1*/
	if (pBD == NULL)
		return RT_FAIL_1;
	return (char)pBD->flags & CHECK_R_FLAG; /* it returns the R_flag value*/
}

/*
Purpose: the purpose of the function is to decrement getc_offset by one
Author: Exequiel Repetto
History/Versions: 1.0
Called functions: none
Parameters: This function will take a pointer to BufferDescriptor structure
Return value: function returns a value of type short
Algorithm:
*/
short b_retract(Buffer * const pBD) {

	/* if pBD is null it will return -1*/
	if (pBD != NULL) {

		/*it decrements getc_offset by one and if is not negative it will return the value*/
		if (--pBD->getc_offset < !0)
			return pBD->getc_offset;
	}

	return RT_FAIL_1;
}
/*
Purpose: the purpose of the function is to set getc_offset to the value of the current marc_offset
Author: Exequiel Repetto
History/Versions: 1.0
Called functions: none
Parameters: This function will take a pointer to BufferDescriptor structure
Return value: function returns a value of type short
Algorithm:
*/
short b_reset(Buffer * const pBD) {

	/* if pBD is null it will return -1*/
	if (pBD != NULL) {
		pBD->getc_offset = pBD->markc_offset; /* sets getc_offset to the current marc_offset*/
		return pBD->getc_offset; /* returns getc_offset*/
	}
	else
		return RT_FAIL_1;
}
/*
Purpose: the purpose of the function is to return getc_offset
Author: Exequiel Repetto
History/Versions: 1.0
Called functions: none
Parameters: This function will take a pointer to BufferDescriptor structure
Return value: function returns a value of type short
Algorithm:

*/
short b_getcoffset(Buffer* const pBD) {

	/* if pBD is a null pointer it will return -1 else getc_offset*/
	if (pBD != NULL) {
		return pBD->getc_offset;
	}
	else
		return RT_FAIL_1;
}
/*
Purpose: the purpose of the function is to set getc_offset and markc_offset to 0
Author: Exequiel Repetto
History/Versions: 1.0
Called functions: none
Parameters: This function will take a pointer to BufferDescriptor structure
Return value: function returns a value of type int
Algorithm:
*/
int b_rewind(Buffer * const pBD) {

	/* if pBD is a null pointer it will return -1 else it will assign 0 to getc_offset and markc_offset and return 0*/
	if (pBD != NULL) {
		pBD->getc_offset = 0;
		pBD->markc_offset = 0;
		return 0;
	}
	return RT_FAIL_1;
}
/*
Purpose: the purpose of the function is to returns a pointer to a location of the character buffer indicated by loc_offset parameter
Author: Exequiel Repetto
History/Versions: 1.0
Called functions: none
Parameters: this functions takes 2 parameters: pointer to buffer descriptor and a short data type
Return value: function returns a value of type char
Algorithm:
*/
char * b_location(Buffer * const pBD, short loc_offset) {

	/* if pBD is not a null pointer and loc_offset is not negative it returns the character in the buffer at loc_offset*/
	if (pBD != NULL) {
		return &pBD->cb_head[loc_offset];
	}
	else
		return NULL;
}

