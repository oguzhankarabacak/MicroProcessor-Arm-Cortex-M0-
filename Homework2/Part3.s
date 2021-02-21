;Oguzhan Karabacak 150170021

LIMIT   EQU	 0x78			;Limit value , 120

PrimeSize EQU 4*(LIMIT+1)	;PrimeSize = (Limit+1)*4
isPrimeSize	EQU (LIMIT+1)	;isPrimeSize = (Limit+1)
		AREA     My_Array, DATA, READWRITE		;Defined area will be placed to the data memory
		ALIGN	
primeNumbers	SPACE    PrimeSize		;Allocate space from memory for prime_numbers
prime_end	

		AREA     My_Array2, DATA, READWRITE		;Defined area will be placed to the data memory
		ALIGN	
isPrimeNumber	SPACE	isPrimeSize			;Allocate space from memory for is_prime values
isprime_end

		AREA factorial_f, code, readonly			;Defined area will be placed to the code memory
		ENTRY
		THUMB
		ALIGN
			


			
;SieveOfEratosthenes
sieveFunc PROC			;sievefunction
		PUSH	{r4,lr}		;take arguments r0<-limit
		MOVS	r1,#0 		;4 byte word index
		MOVS	r3,#0 		;1 byte index
loop1	CMP		r3,r0		; while (r1<limit+1)
		BGE		loop2b		;if is not jump loop2b
		MOVS	r2,#0		;r2<-0
		STR		r2,[r5,r1]	;primeNumbers[i_word]=0
		MOVS	r2,#1		;r2 <- 1
		STRB	r2,[r6,r3]	;isPrimeNumbers[i]=0
		ADDS	r3,r3,#1	;i = i+1
		ADDS	r1,r1,#4	;i = i+4 for word
		b loop1				;jump loop1
		
loop2b	MOVS 	r1,#8 	; i_word , 4 byte word index initially value is 8
		MOVS	r2,#2 		; i ,1 byte index	initially valaue is 2
loop2	MOVS	r3,r2		; r3 <- r2
		MULS	r3,r3,r3 	; i = i^2
		CMP		r3,r0		; while (i^2 < limit+1)
		BGE		loop3		;if it is not jump loop3
		LDRB	r4,[r6,r2]	;r4 <- isPrimeNumber[i]
		CMP		r4,#1		; if isPrimeNumber[i] == 1
		BNE		notE		;if is not jump notE
		MOVS	r4,#0		;r4 <- 0
		MOVS	r1,#0		; r1 <- 0
loop2c	MOVS	r3,r2		;r3 <- r2 for inside loop
		MULS	r3,r3,r3	; r3 = r3^2
		ADDS	r3,r3,r1	; r3 <- r3 + r1 , j=i*i+[r2]*i
		CMP		r3,r0		; while(i^2<limit+1)
		BGE		notE		;if is not jump notE
		MOVS	r1,#0		; r1 <- 0
		STRB	r1,[r6,r3]	;isPrimeNumber[j] <- 0
		ADDS	r4,r4,#1	;r4 <- r4+1
		MOV		r1,r4		; r1 <- r4
		MULS	r1,r2,r1	; r1 <- r2*r1
		B		loop2c		;jump loop2c
		
notE	ADDS	r2,r2,#1	;r2 <- r2+1, i=i+1
		B		loop2		;jump loop2
	
loop3	MOVS	r2,#0 		;r2 <- 0 , i  word index  
		MOVS 	r3,#2 		;i index	
loop3a	CMP		r3,r0		;while (i<limit+1)
		BGE		stop		;if is not go stop
		MOVS	r1,#1		;r1 <- 1
		LDRB	r4,[r6,r3]	;r4 <- isPrimeNumber[i] 
		CMP		r4,r1		; if isPrimeNumber[i] == 1
		BNE		notE2		;if is not jump notE2
		STR		r3,[r5,r2]	;primeNumbers[i_word] <- r3
		ADDS	r3,r3,#1	; r3 <- r3+1
		ADDS	r2,r2,#4	; r2 <- r2+4
		B		loop3a		;jump loop3a
		
notE2	ADDS	r3,r3,#1	;r3 <- r3+1
		B		loop3a		;jump loop3a
		ENDFUNC
		
__main	FUNCTION
		EXPORT __main
;main
		LDR 	r5,=primeNumbers	;Load start address of the allocated space for prime_array
		LDR		r6,=isPrimeNumber		;Load start address of the allocated space for is_array
		MOVS	r7,#isPrimeSize		;Load array isPrimesize
		MOV		r0,r7				;r0 <- r7 , For function argument
		BL	sieveFunc	
		
stop	B stop									;Infinite loop
		ALIGN
		ENDFUNC

		END