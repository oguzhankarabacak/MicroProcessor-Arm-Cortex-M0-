;Oguzhan Karabacak 150170021

Index   EQU	 0x06							;Index value

ArraySize EQU 4*(Index+1)                       ;ArraySize = (Index+1)*4 , each index is word 
		AREA     My_Array, DATA, READWRITE		;Defined area will be placed to the data memory
		ALIGN	
array 	SPACE    ArraySize						;Allocate space from memory for array
a_end

		AREA factorial_f, code, readonly			;Defined area will be placed to the code memory
		ENTRY
		THUMB
		ALIGN
			
;factorial
factorial PROC				;factorial function
		PUSH	{r4,lr}			;take argument, r0:index
		MOVS	r2,#2		;r2<-2 to compare
		CMP		r0,r2		;if if (r0 < r2)
		BGE		return_f	;else jumpy return_f
		MOVS 	r3,#1			;r3 <- 1
		POP 	{r4,pc}			;return r3

return_f 	SUBS	r0,r0,#1	;r0 <- r0-1 for new recursive function argument
		BL factorial			;call factorial , recursive , and return r3
		ADDS	r0,r0,#1		;to preserve old value of r0
		MULS	r3,r0,r3		;r3 <- r0 * r3 , n <- n * factorial (n-1)
		POP {r4,pc}				;return r0 value
		ENDFUNC					;end of function
		

__main	FUNCTION
		EXPORT __main
;main
		LDR 	r5,=array		;Load start address of the allocated space for array
		MOVS	r7,#ArraySize	;Load array size
		MOVS	r6,#0			;for index 4byte r6<-0
		MOVS	r0,#0			;r0 <- 0 , and r0 is argument of factorial function
Loop	CMP		r6,r7			;while(r6<=r7)
		BGE 	stop			;if r7>r6 jump stop
		BL	factorial			;call factorial function and return r3
		STR		r3,[r5,r6]		;r5[r6]<-r3
		ADDS 	r6,r6,#4		;r6<-r6+4  , i_4=i_4+4
		ADDS	r0,r0,#1		;r0<-r0+1  , i=i+1
		B Loop					;jump loop
		
		
stop	B stop					;Infinite loop
		ALIGN
		ENDFUNC

		END