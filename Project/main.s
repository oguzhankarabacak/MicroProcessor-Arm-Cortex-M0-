;***************************
;@file				 Main.s
;@project		     Microprocessor Systems Term Project
;@date				 25.01.2021
;				

;828 * 10^-6 = (1 + Reload Value)/8 * 10^6
;reload value = 6623

;@PROJECT GROUP
;@groupno			15
;@member1			Tevfik Ozgu 150180082
;@member2			Soner Ozturk 150170005
;@member3			Ceyhun Ugur 150170017
;@member4			Muhammet Akcan 150180066
;@member5			Oguzhan Karabacak 150170021
;***************************
;***************************
;@section 		INPUT_DATASET
;***************************

;@brief 	This data will be used for insertion and deletion operation.
;@note		The input dataset will be changed at the grading. 
;			Therefore, you shouldn't use the constant number size for this dataset in your code. 
				AREA     IN_DATA_AREA, DATA, READONLY
IN_DATA			DCD		0x10, 0x20, 0x15, 0x65, 0x25, 0x01, 0x01, 0x12, 0x65, 0x25, 0x85, 0x46, 0x10, 0x00
END_IN_DATA

;@brief 	This data contains operation flags of input dataset. 
;@note		0 -> Deletion operation, 1 -> Insertion 
				AREA     IN_DATA_FLAG_AREA, DATA, READONLY
IN_DATA_FLAG	DCD		0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x01, 0x01, 0x00, 0x02
END_IN_DATA_FLAG


;***************************
;@endsection 	INPUT_DATASET
;***************************

;***************************
;@section 		DATA_DECLARATION
;***************************

;@brief 	This part will be used for constant numbers definition.
NUMBER_OF_AT	EQU		20									; Number of Allocation Table
AT_SIZE			EQU		NUMBER_OF_AT*4						; Allocation Table Size


DATA_AREA_SIZE	EQU		AT_SIZE*32*2						; Allocable data area
															; Each allocation table has 32 Cell
															; Each Cell Has 2 word (Value + Address)
															; Each word has 4 byte
ARRAY_SIZE		EQU		AT_SIZE*32							; Allocable data area
															; Each allocation table has 32 Cell
															; Each Cell Has 1 word (Value)
															; Each word has 4 byte
LOG_ARRAY_SIZE	EQU     AT_SIZE*32*3						; Log Array Size
															; Each log contains 3 word
															; 16 bit for index
															; 8 bit for error_code
															; 8 bit for operation
															; 32 bit for data
															; 32 bit for timestamp in us

;//-------- <<< USER CODE BEGIN Constant Numbers Definitions >>> ----------------------															
							


;//-------- <<< USER CODE END Constant Numbers Definitions >>> ------------------------	

;***************************
;@brief 	This area will be used for global variables.
				AREA     GLOBAL_VARIABLES, DATA, READWRITE		
				ALIGN	
TICK_COUNT		SPACE	 4									; Allocate #4 byte area to store tick count of the system tick timer.
FIRST_ELEMENT  	SPACE    4									; Allocate #4 byte area to store the first element pointer of the linked list.
INDEX_INPUT_DS  SPACE    4									; Allocate #4 byte area to store the index of input dataset.
INDEX_ERROR_LOG SPACE	 4									; Allocate #4 byte aret to store the index of the error log array.
PROGRAM_STATUS  SPACE    4									; Allocate #4 byte to store program status.
															; 0-> Program started, 1->Timer started, 2-> All data operation finished.
;//-------- <<< USER CODE BEGIN Global Variables >>> ----------------------															
							


;//-------- <<< USER CODE END Global Variables >>> ------------------------															

;***************************

;@brief 	This area will be used for the allocation table
				AREA     ALLOCATION_TABLE, DATA, READWRITE		
				ALIGN	
__AT_Start
AT_MEM       	SPACE    AT_SIZE							; Allocate #AT_SIZE byte area from memory.
__AT_END

;@brief 	This area will be used for the linked list.
				AREA     DATA_AREA, DATA, READWRITE		
				ALIGN	
__DATA_Start
DATA_MEM        SPACE    DATA_AREA_SIZE						; Allocate #DATA_AREA_SIZE byte area from memory.
__DATA_END

;@brief 	This area will be used for the array. 
;			Array will be used at the end of the program to transform linked list to array.
				AREA     ARRAY_AREA, DATA, READWRITE		
				ALIGN	
__ARRAY_Start
ARRAY_MEM       SPACE    ARRAY_SIZE						; Allocate #ARRAY_SIZE byte area from memory.
__ARRAY_END

;@brief 	This area will be used for the error log array. 
				AREA     ARRAY_AREA, DATA, READWRITE		
				ALIGN	
__LOG_Start
LOG_MEM       	SPACE    LOG_ARRAY_SIZE						; Allocate #DATA_AREA_SIZE byte area from memory.
__LOG_END

;//-------- <<< USER CODE BEGIN Data Allocation >>> ----------------------															
							


;//-------- <<< USER CODE END Data Allocation >>> ------------------------															

;***************************
;@endsection 	DATA_DECLARATION
;***************************

;***************************
;@section 		MAIN_FUNCTION
;***************************

			
;@brief 	This area contains project codes. 
;@note		You shouldn't change the main function. 				
				AREA MAINFUNCTION, CODE, READONLY
				ENTRY
				THUMB
				ALIGN 
__main			FUNCTION
				EXPORT __main
				BL	Clear_Alloc					; Call Clear Allocation Function.
				BL  Clear_ErrorLogs				; Call Clear ErrorLogs Function.
				BL	Init_GlobVars				; Call Initiate Global Variable Function.
				BL	SysTick_Init				; Call Initialize System Tick Timer Function.
				LDR R0, =PROGRAM_STATUS			; Load Program Status Variable Addresses.
LOOP			LDR R1, [R0]					; Load Program Status Variable.
				CMP	R1, #2						; Check If Program finished.
				BNE LOOP						; Go to loop If program do not finish.
STOP			B	STOP						; Infinite loop.
				
				ENDFUNC

			
;***************************
;@endsection 		MAIN_FUNCTION
;***************************				

;***************************
;@section 			USER_FUNCTIONS
;***************************

;@brief 	This function will be used for System Tick Handler
				ALIGN
SysTick_Handler	FUNCTION	
				
;//-------- <<< USER CODE BEGIN System Tick Handler >>> ----------------------															
				EXPORT SysTick_Handler
				PUSH {LR}
				LDR R3, =IN_DATA_FLAG	;Load Input Data Flag Address to R3.
				LDR	R0, =IN_DATA		;Load Input Data Address to R0
				LDR R1, =TICK_COUNT		;Load TICK_COUNT Address
				LDR R4, [R1]			;R4 <- Index
				LSLS R4, #2				;R4 <- R4*4
				LDR R2,[R3,R4]			;R2 <- IN_DATA_FLAG[Index]
				CMP	R2,#0				;If Operation == Delete
				BNE	Add_Number			;If Operation == Delete
				LDR	R0,[R0,R4]			;Load data to R0
				BL	Remove				;Call Remove
				B	END_OF_TICK			;If Removed, jump to END_OF_TICK
Add_Number		CMP	R2,#1				;If Operation == Add
				BNE	Finish_Program		;If Operation != Add
				LDR	R0,[R0,R4]			;Load data to R0
				BL	Insert				;Call Insert
				B 	END_OF_TICK			;If Inserted, jump to END_OF_TICK
Finish_Program	CMP	R2,#2				;If Operation == Finish
				BNE	NO_OP_FOUND			;If Operation != Finish
				BL 	LinkedList2Arr		;Write values to linked list array
				BL 	SysTick_Stop		;Call Stop Timer
				B	END_OF_TICK			;finish handler
NO_OP_FOUND
				MOVS R0,#6				;If Operation is not found load 6 to R0		
END_OF_TICK		
				CMP R0,#0				;If there is no error
				BEQ	FINISH_INTERRUPT	;then branch to FINISH_INTERRUPT
				MOV	R1,R0				;R1 = Error Code
				MOV	R0,R4				;R0 = Index of Input Data
				LDR	R3, =IN_DATA		;R3 = Address of IN_DATA
				LDR	R3,[R3,R4]			;R3 = Data
				BL	WriteErrorLog		;Branch to Error_Log
				
FINISH_INTERRUPT				
				LSRS R4, #2				;R4 <- R4/4 since it was shifted 2 bit before
				ADDS R4,R4,#1;			;Index+=1
				LDR R1, =TICK_COUNT		;Load TICK_COUNT Address
				STR	R4,[R1]				;TICK_COUNT += 1
				LDR R1, =INDEX_INPUT_DS	;Load INDEX_INPUT_DS Address
				STR	R4,[R1]				;INDEX_INPUT_DS += 1
				POP	{PC}				;Return to Main
;//-------- <<< USER CODE END System Tick Handler >>> ------------------------		
				ENDFUNC

;***************************				

;@brief 	This function will be used to initiate System Tick Handler
				ALIGN
SysTick_Init	FUNCTION
				
;//-------- <<< USER CODE BEGIN System Tick Timer Initialize >>> ----------------------															
				LDR R2,=0XE000E010	;Load SysTick Control and Status Register Address
				LDR R3,=6623		;Reload Value
				STR R3, [R2,#4];	;Store Reload Value to Value Reg.
				MOVS R3,#0;			;Clear R2
				STR	R3,[R2,#8];		;Clear Current Value Register
				MOVS R3, #7;		;Set Enable, Clock and Interrupt Flags
				STR R3, [R2]		;Store R1 To SysTickCSR register
				MOVS R3,#1;			;Store Timer Started Value
				LDR R0, =PROGRAM_STATUS	; Load Program Status Variable Addresses.
				STR	R3, [R0]		;Store Program Status to Program Status Register
				MOVS R4, #0;		;Index value
				BX LR
;//-------- <<< USER CODE END System Tick Timer Initialize >>> ------------------------				
				ENDFUNC

;***************************				

;@brief 	This function will be used to stop the System Tick Timer
				ALIGN
SysTick_Stop	FUNCTION			
;//-------- <<< USER CODE BEGIN System Tick Timer Stop >>> ----------------------
				PUSH{r0,r1,r2,r3,lr}
				LDR R2,=0XE000E010	;Load SysTick Control and Status Register Address
				
				MOVS R3, #0;		;Set Enable, Clock and Interrupt Flags to 0
				STR R3, [R2]		;Store R3 To SysTickCSR register
				MOVS R3,#2;			;MOVS All data operations finished value.
				LDR R0, =PROGRAM_STATUS			; Load Program Status Variable Addresses.
				STR	R3, [R0]		;Store All data operations finished flag to Program Status Register
				POP{r0,r1,r2,r3,pc}
;//-------- <<< USER CODE END System Tick Timer Stop >>> ------------------------				
				ENDFUNC

;***************************				

;@brief 	This function will be used to clear allocation table
				ALIGN
Clear_Alloc		FUNCTION			
;//-------- <<< USER CODE BEGIN Clear Allocation Table Function >>> ----------------------	
				PUSH{R2,R3,R4,R5,LR}
				LDR R2, =AT_MEM		; Load Allocation Table Addresses.
				MOVS R3, #0x00		;Set Allocation Table to 0
				MOVS R4, #0			;counter
				LDR R5, =AT_SIZE	;Load Allocation Table Size
loopclear		CMP R4, R5			;if(r4 == r5)	
				BEQ endclear		;	end
				STR R3, [R2,R4]		;Store R3 To Allocation Table
				ADDS R4, #4			;counter++
				B loopclear   		;back to loop
endclear		POP{R2,R3,R4,R5,PC}	
;//-------- <<< USER CODE END Clear Allocation Table Function >>> ------------------------				
				ENDFUNC
				
;***************************		

;@brief 	This function will be used to clear error log array
				ALIGN
Clear_ErrorLogs	FUNCTION			
;//-------- <<< USER CODE BEGIN Clear Error Logs Function >>> ----------------------															
				PUSH{R2,R3,R4,R5,LR}
				LDR R2, =LOG_MEM	; Load Allocation Table Addresses.
				MOVS R3, #0x00		;Set Allocation Table to 0
				MOVS R4, #0			;counter
				LDR R5, =LOG_ARRAY_SIZE	;Load Allocation Table Size
loopclear2		CMP R4, R5			;if(r4 == r5)	
				BEQ endclear2		;end
				STR R3, [R2,R4]		;Store R3 To Allocation Table
				ADDS R4, #4			;counter++
				B loopclear2		;back to loop
endclear2		POP{R2,R3,R4,R5,PC}	
;//-------- <<< USER CODE END Clear Error Logs Function >>> ------------------------				
				ENDFUNC
				
;***************************

;@brief 	This function will be used to initialize global variables
				ALIGN
Init_GlobVars	FUNCTION	
	
;//-------- <<< USER CODE BEGIN Initialize Global Variables >>> ----------------------															
				MOVS R2, #0			;Set R3 to 0
				LDR R3, =TICK_COUNT	;Load TICK_COUNT address to R2
				STR R2, [R3]		;Store R3 To Error Log Array
				LDR R3, =FIRST_ELEMENT	;Load FIRST_ELEMENT address to R2
				STR R2, [R3]		;Store R3 To Error Log Array
				LDR R3, =INDEX_INPUT_DS	;Load INDEX_INPUT_DS address to R2
				STR R2, [R3]		;Store R3 To Error Log Array
				LDR R3, =INDEX_ERROR_LOG	;Load INDEX_ERROR_LOG address to R2
				STR R2, [R3]		;Store R3 To Error Log Array
				LDR R3, =PROGRAM_STATUS	;Load PROGRAM_STATUS address to R2
				STR R2, [R3]		;Store R3 To Error Log Array
				BX LR
;//-------- <<< USER CODE END Initialize Global Variables >>> ------------------------				
				ENDFUNC
				
				
;***************************	

;@brief 	This function will be used to allocate the new cell 
;			from the memory using the allocation table.
;@return 	R0 <- The allocated area address
				ALIGN
Malloc			FUNCTION			
;//-------- <<< USER CODE BEGIN Malloc Function >>> ----------------------	
				PUSH{r1,r2,r3,r4,r5,r6,r7,lr}
				LDR r3, =AT_MEM				;load allocation table array addresses
				LDR r6, =AT_SIZE			;load allocation table size 
				MOVS r4, #0					;index for looping in at_mem array
				MOVS r1, #0					;counter1 for number of bytes controlled in at_mem array
LOOP1			CMP r6, r1					;if number of bytes controlled = at_size
				BEQ end_of_array			;	end of at_mem array is reached, which means linkedlist is full
				LDRB r5, [r3, r4] 			;load AT_MEM[r4] byte by byte
				ADDS r4, r4, #1				;index++
				MOVS r2, #0					;counter2 for number of bits controlled in 1 byte
				ADDS r1, r1, #1				;counter1++
LOOP2			CMP r2, #8					;if number of bits controlled = 8			
				BEQ LOOP1					;	all bits controlled in loaded 1 byte, branch to loop1 to load next bye
				MOVS r7, #0xFE				;1111 1110, 2nd operand of OR
				ORRS r7, r5, r7				;R7 = R7 OR R5
											;except LSB all bits in loaded byte will be 1, and LSB dont change 
				LSRS r5, r5, #1				;shift right R5 to load the bit, on left side of the controlled bit, to LSB
				ADDS r2, r2, #1				;counter2++
				CMP r7, #0xFF				;compare r7 and FF, r7 = 1111 111x where x is the LSB of R5 
				BEQ	LOOP2					;if x=1 then branch to loop2 again to check remained bits
											;else(if x=0) that means we found an empty place
				
				;calculations for finding the index number of the first 0 bit in allocation table with using counter1 and counter2
				SUBS r1, r1, #1				;counter1-- since it was increased one more time in the last loop1
				MOVS r7, r1					;in order not to lose the value of r1, r7=r1 is done, r7 will be used later
				MOVS r6, #8					;r6 = 8
				MULS r1, r6, r1				;r1 = r1*8, now r1 holds the num of bits controlled except the last controlled byte
				SUBS r2, r2, #1				;r2-- since we want to start indexing bits from 0, not 1
				ADDS r1, r1, r2				;r2 hold the bits controlled in the last taken byte
											;add it to the r2 to calculate all num of bits controlled
				;if the rightmost bit of allocation table is index0, r1 holds now first 0 bit from rightside

				;calculations for finding the corresponding address in DATA_MEM array
				MULS r1, r6, r1				;r1=r1*8 since each bit in allocation table corresponds 8 byte in DATA_MEM array
				LDR r0, =DATA_MEM			;load address of DATA_MEM
				ADDS r0, r1, r0				;r0=r0+r1 in order to find the corresponding addres in DATA_MEM array
				
				;in the allocation table, found bit which holds 0, should be changed to 1 since it willnot be empty afterwards
				MOVS r6, #0x01				;r6 = 0000 0001
				LSLS r6, r6, r2				;shift left r6, r2 times(r2 holds num of bits controlled in the last byte)
change_0_to_1	LDRB r5, [r3,r7]			;load the value of corresponding address in allocation table
				ADDS r5, r5, r6				;change 0 to 1;for example, if taken byte is 1100 1111 then 0001 0000 is added to it
				STRB r5, [r3,r7]			;store the changed value to corresponding address	
				POP{r1,r2,r3,r4,r5,r6,r7,pc}	;return
				
end_of_array	MOVS r0,#0					;r0 = 0, no empty place, linked list full
				POP{r1,r2,r3,r4,r5,r6,r7,pc}	;return

;//-------- <<< USER CODE END Malloc Function >>> ------------------------				
				ENDFUNC
				
;***************************				

;@brief 	This function will be used for deallocate the existing area
;@param		R0 <- Address to deallocate
				ALIGN
Free 			FUNCTION 
;//-------- <<< USER CODE BEGIN Free Function >>> ----------------------
				;r0,r1,r2,r3,r4,r5 were used
				PUSH {R0,R1,R2,R3,R4,R5,r6,LR} 
				LDR r1,=AT_MEM;r1 <- AT_MEM
				MOVS r6,#0 ;r6 <- 0
				LDR r2,=DATA_MEM;r2<-DATA_MEM
				STR	r6,[r0] ;store  to r0
				STR	r6,[r0,#4];store 
				SUBS r0,r0,r2 ;calculate the distance to the begining of the data mem array
				LSRS r0,r0,#3 ;divide to find index
				LSRS r3,r0,#3 ;find appropiate byte
				LSLS r4,r3,#3 ;temp to subtract 
				SUBS r0,r0,r4 ;find which bit will be freed
				LDRB r4,[r1,r3]  ;take at mem register to change
				MOVS r5,#1;r5<-1
				LSLS r5,r5,r0;find which bit will be change
				EORS r4,r4,r5;change the bit 
				STRB r4,[r1,r3];store back to at mem 
				POP {R0,R1,R2,R3,R4,R5,r6,PC};return 
;//-------- <<< USER CODE END Free Function >>> ------------------------ 
				ENDFUNC

;***************************				

;@brief 	This function will be used to insert data to the linked list
;@param		R0 <- The data to insert
;@return    R0 <- Error Code
				ALIGN
Insert			FUNCTION			
;//-------- <<< USER CODE BEGIN Insert Function >>> ----------------------		
				PUSH {r1,r2,r3,r4,LR}   ;r0 is parameter for new value
				MOV	r1,r0    			;first r1<-r0 because r0 will be used in malloc return
				LDR	r2,=FIRST_ELEMENT  	;Load FIRST_ELEMENT to r2
				LDR	r3,=DATA_MEM 		;Load DATA_MEM start address to r3
				LDR	r4,[r2]  			;Load FIRST_ELEMENT address to r4 
				CMP	r4,#0				;First check linkedlist is empty, head == NULL
				BNE	head_not_null 		;if head != NULL jump head_not_null
				BL Malloc				;if null call Malloc function, and Malloc return free address with r0
				CMP r0,#0				;compare r0 is equal 0
				BEQ list_full			;if Malloc return 0, it is mean that list is full jump list_full
				STR	r1,[r0]	  			;if Malloc does not return 1,list has area so value can be stored in Malloc return address
				STR	r0,[r2]  			;The return address be stored in FIRST_ELEMENT 
				MOVS r5,#0				;r5 <- 0 , to head point address is 0
				STR	r5,[r0,#4] 			;head->next=0 , 
				B not_error				;then jump not_error
				
				
head_not_null	LDR r4,[r4]			;if head is not null r4 <- head_value
				CMP	r1,r4			;Compare new_value and head_value
				BGE	is_equal_head	;if new_value is not smaller than head value jump is_equal_head
				BL Malloc			;if smaller,malloc return free address with r0
				CMP r0,#0			;compare r0 is equal 0
				BEQ list_full		;if Malloc return 0, it is mean that list is full jump list_full
				STR	r1,[r0] 		;if Malloc does not return 1,list has area so value can be stored in Malloc return address
				ADDS r0,r0,#4 		; r0 <- r0 + 4 to access node address
				LDR r4,[r2]			;Take FIRST_ELEMENT address, head address
				STR	r4,[r0] 		;Address of old header is pointed by new node
				SUBS r0,r0,#4 		;r0 <- r0 - 4 to access node address
				STR	r0,[r2]			;store r0 value to FIRST_ELEMENT because it is new head
				B not_error			;then jump not_error

is_equal_head	CMP r1,r4			;compare head_value and new_value is equal
				BNE  bigger_head	;if is not equal jump bigger_head
				MOVS  r0,#2			;if is equal this is error so r0 <- 2 
				B return			;then jump return

bigger_head 	LDR r5,[r2]			;r5 = iter = head to traverse in linkedlist
loop			ADDS r5,r5,#4 		;r5 <- r5+4 to access address
				LDR r4,[r5]			;take next node address
				CMP r4,#0			;compare address and 0 , because if it is zero , the iter is in the end of linkedlist
				BEQ not_equal		;if it is equal, jump not_equal
				LDR r6,[r4]			;if it is not equal,load r4 value(next node value) to r6
				CMP r6,r1			;compare new value and node value
				BGE end_of 			;if new value is smaller jump end_of to check is equal
				MOV r5,r4			;if is not smaller new temp is current node iter = iter->next
				B loop				;then jump loop
	
end_of	   		LDR	r6,[r5]		;load iter address  to r6 register
				LDR r6,[r6]		;and take current_node value
				CMP r6,r1		;check iter->value == new_value
				BNE not_equal	;if current_node is not equal to new_value  jump not equal
				MOVS r0,#2		;if is equal this is error so r0 <- 2 
				B return		;jump return

not_equal		LDR r6,[r5]		;load iter address to r6 register r6 <- iter
				BL Malloc  		;call malloc 
				CMP r0,#0		;compare malloc return value with 0,
				BEQ list_full	;if malloc return 0 , list is full so jump list_full
				STR r1,[r0] 	;if malloc return address, new value is stored in address
				ADDS r0,r0,#4 	;r0 <- r0+4 to access node point address
				STR r6,[r0] 	;new_node->address = iter->address
				SUBS r0,r0,#4 	;r0 <- r0-+ to access node address
				STR	r0,[r5] 	;iter->address = new_node 
				B not_error		;then jump not_error
							
list_full		MOVS r0,#1     ;if list_full this error code is 1
				B return		;jump return
					
not_error		MOVS r0,#0 		;if not error error code is 0
return			POP {r1,r2,r3,r4,PC}  ;return r0
;//-------- <<< USER CODE END Insert Function >>> ------------------------				
				ENDFUNC
				
;***************************				

;@brief 	This function will be used to remove data from the linked list
;@param		R0 <- the data to delete
;@return    R0 <- Error Code
				ALIGN
Remove			FUNCTION			
;//-------- <<< USER CODE BEGIN Remove Function >>> ----------------------
				PUSH {r1,r2,r3,r4,LR}					;data which will be removed
				LDR		r1, =FIRST_ELEMENT		;determine head of the linked list
				LDR		r2,[r1]					;load value of the FIRST_ELEMENT address to the r2(iter)
				LDR		r3, [r2]				;load value of the r2 address to the r3
				CMP		r2, #0					;control linked list is empty or not?
				BEQ		go_to_error_1			;if r3 == NULL, then go to the go_to_error_2 label
				CMP		r0, r3					;control (param ?= iter->data)-----> BASTAN SIL
				BNE		while					;if not equal, go to while label
				LDR		r3, [r2, #4]			;load the next smallest element to the r3
				MOV		r0, r2					;move r2 to the r0 to send the parameter to the free function
				STR		r3, [r1]				;determine the new head element
				BL		Free					;call free function
				MOVS	r0, #0					;determine the success code as error code
				POP {r1,r2,r3,r4,PC}					;return
				
while			
				;LDR		r1, [r1]				;load value of the FIRST_ELEMENT address to the r1(sondan bir ?nceki eleman)				
				LDR		r3, [r2, #4]			;r3 = r2->next
				CMP		r3, #0					;control r4 is NULL or not
				BEQ		end_while				;if r4 is NULL, then go to the end_while label
				LDR		r4, [r3]				;load the value of the address to the r4(iter->next->data)
				CMP		r4, r0					;control (param =? iter->next->data)-----> ORTADAN SIL
				BNE		second_if				;if r4 and r0 is different, go to the second_if label
				LDR		r4, [r3, #4]			;r4 = iter->next->next
				MOV		r0, r3					;r0 = iter->next
				STR		r4, [r2, #4]			;iter->next = r4
				BL		Free					;call free function
				MOVS	r0, #0					;determine the success code as error code
				POP {r1,r2,r3,r4,PC}					;return
second_if
				LDR		r3, [r3, #4]			;r3 = iter->next->next
				CMP		r3, #0					;control (iter-next->next =? NULL)
				BNE		end_second_if			;if not, go to the end_second_if
				MOV		r1, r2					;r1(keep) = iter
end_second_if
				LDR		r2, [r2, #4]			;iter = iter->next
				B		while					;go to the while label
end_while
				LDR		r3, [r2]				;r3 = iter->data
				CMP		r0, r3					;control (param =? iter->data)-----> SONDAN SIL
				BNE		go_to_error_2			;if not, go to the go_to_error label
				MOV		r0, r2					;r0 = iter
				MOVS	r4, #0					;load 0 to r4 register
				STR		r4, [r1, #4]			;r4 = NULL
				BL		Free					;call free function
				MOVS	r0, #0					;determine the success code as error code
				POP {r1,r2,r3,r4,PC}					;return
go_to_error_1
				MOVS	r0, #3					;ERROR (LINKED LIST IS EMPTY)
				POP {r1,r2,r3,r4,PC}
go_to_error_2
				MOVS	r0, #4					;ERROR (ELEMENT IS NOT IN THE LINKED LIST)
				POP {r1,r2,r3,r4,PC}
;//-------- <<< USER CODE END Remove Function >>> ------------------------				
				ENDFUNC
				
;***************************				

;@brief 	This function will be used to clear the array and copy the linked list to the array
;@return	R0 <- Error Code
				ALIGN
LinkedList2Arr	FUNCTION			
;//-------- <<< USER CODE BEGIN Linked List To Array >>> ----------------------		
				PUSH{r1,r2,r3,r4,lr}
				LDR 	r1,=FIRST_ELEMENT		;r1 = head
				LDR 	r2,=ARRAY_MEM			;r2 = array
				LDR 	r1,[r1]				;r1 = linked list element value
				MOVS 	r3,#0					;index
				MOVS	r4,#5					;error code
loopa			CMP		r1,#0					;compare r1 with 0
				BEQ		finish 				;if equal
				MOVS	r4,#0					;error flag
				LDR		r0,[r1]					;r0 is data of the node
				LDR		r1,[r1,#4]				;r1 is the next node's address
				STR		r0,[r2,r3]				;store the value to the array
				ADDS	r3,#4					;r3 = r3 + 4
				BL		Remove				;call remove function
				B		loopa				;jump to loopa
finish			MOVS 	r0,r4					;r0 = r4
				;return r0					
				POP{r1,r2,r3,r4,PC}
;//-------- <<< USER CODE END Linked List To Array >>> ------------------------				
				ENDFUNC
				
;***************************				

;@brief 	This function will be used to write errors to the error log array.
;@param		R0 -> Index of Input Dataset Array
;@param     R1 -> Error Code 
;@param     R2 -> Operation (Insertion / Deletion / LinkedList2Array)
;@param     R3 -> Data
				ALIGN
WriteErrorLog	FUNCTION			
;//-------- <<< USER CODE BEGIN Write Error Log >>> ----------------------			
				PUSH{r0,r1,r2,r3,r4,r5,r6,r7,lr}
				LDR r5,=LOG_MEM 			;load log_mem address to r5 register				
				
				LDR r6,=INDEX_ERROR_LOG 		;r6 = where the new error will be stored
				LDR 	r7,[r6]				;take the value
				
				MOVS	r4,#12				;r4 <- 12
				MULS 	r7,r4,r7			;r7 <-  r4*r7 
				ADDS	r5,r7,r5			;r5 <- r7 + r5 find the start of error code 
				
				STRH	r0,[r5]				;write r0 as index of input dataset
				MOVS 	r0, #0x02
				ADDS	r5,r0				;increment the cursor
				
				STRB	r1,[r5]				;write r1 as error code 
				ADDS	r5,#1				;increment cursor 
				STRB	r2,[r5]				;write r2 as operation
				ADDS	r5,#1				;increment cursor
				STR 	r3,[r5]				;write r3 as data 
				ADDS	r5,#4				;increment cursor
				BL	GetNow				;get timestamp
				STR	r0,[r5]				;write r0 as timstamp
				
				LDR 	r7,[r6]				;take global index
				ADDS	r7,#1				;increment global index
				STR		r7,[r6]			;store global index
				POP{r0,r1,r2,r3,r4,r5,r6,r7,pc}
					
;//-------- <<< USER CODE END Write Error Log >>> ------------------------
				ENDFUNC
;@brief 	This function will be used to get working time of the System Tick timer
;@return	R0 <- Working time of the System Tick Timer (in us).	
				ALIGN
GetNow			FUNCTION			
;//-------- <<< USER CODE BEGIN Get Now >>> ----------------------															
				PUSH{R1,R2,R3,LR}		;Push Registers and LR to SP
				LDR R1, =TICK_COUNT		;Load TICK_COUNT Address
				LDR R2, [R1]			;R2 <- Index
				LDR R3, =828			;R3<-Period
				MULS R2, R3, R2			;R2 <- Period * Index
				LDR R1,=0XE000E010		;Load SysTick Control and Status Register Address
				LDR R3,[R1,#4]			;Load Reload Value to R3.
				LDR	R0,[R1,#8]			;Load  Current Value Register to R0
				SUBS R0, R3, R0			;R0 <- Reload Value Register - Current Value Register
				ADDS R0, R0, #1			;R0 <- R0 - 1
				LSRS R0, #3				;R0 <- R0/8
				ADDS R0, R0, R2			;R0 <- R0 + Period * Index
				POP{R1,R2,R3,PC}		;Return By LR
;//-------- <<< USER CODE END Get Now >>> ------------------------
				ENDFUNC
				
;***************************	

;//-------- <<< USER CODE BEGIN Functions >>> ----------------------															


;//-------- <<< USER CODE END Functions >>> ------------------------

;***************************
;@endsection 		USER_FUNCTIONS
;***************************
				ALIGN
				END		; Finish the assembly file
				
;***************************
;@endfile 			main.s
;***************************
