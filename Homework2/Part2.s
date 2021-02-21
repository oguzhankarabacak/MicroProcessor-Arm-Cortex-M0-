;Oguzhan Karabacak	150170021

Index   EQU	 0x06							;Index value

ArraySize EQU 4*(Index+1)  						;ArraySize = (Index+1)*4 byte , each index is word
		AREA     My_Array, DATA, READWRITE		;Defined area will be placed to the data memory
		ALIGN	
array 	SPACE    ArraySize						;Allocate space from memory for array
array_end

		AREA factorial_f, code, readonly			;Defined area will be placed to the code memory
		ENTRY
		THUMB
		ALIGN


			
;factorial
factorial PROC					;factorial function
	PUSH	{r4,lr}				;take function arguments  ,r1 :array size, r0 : array
		MOVS	r2,#1			;r2 <- 1
		STR		r2,[r0,#0]		;r[0] <- 1 , array[0] <- 1
		MOVS	r2,#4 			;r2 = 4 , i which increase by 4 ,for word
		MOVS 	r3,#1			;r3 = 1 , i which increase by 1, index
Loop	CMP		r2,r1			;check word i < index+4
		BGE		stop			;if not finish loop
		SUBS	r4,r2,#4		;i=i-1 , r4 <- r2-4 for
		LDR		r4,[r0,r4]		;r4 = array[i-1]
		MULS	r4,r3,r4		;r4 = r3*r4, temp=i*array[i-1]
		STR 	r4,[r0,r2]		;array[i] = temp
		adds 	r2,r2,#4		;i=i+4 for word.
		adds	r3,r3,#1		;i=i+1 for index
		B		Loop			;End of the loop, jump start point
		ENDFUNC					;End function
		
__main	FUNCTION
		EXPORT __main
;main
		LDR 	r5,=array		;Load start address of the allocated space for array
		MOVS	r6,#ArraySize	;Load array size
		MOV r0,r5				;r0 <- r5 ,for arguments 
		MOV	r1,r6				;r1 <- r3
		BL	factorial			;call factorial
		
stop	B stop					;Infinite loop
		ALIGN
		ENDFUNC

		END