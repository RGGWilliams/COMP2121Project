;
; Lab05.asm
;
; Created: 18/05/2017 7:21:00 PM
; Author : Comp2121
;


.include "m2560def.inc"

.def address = r4
.def temp = r16
.def temp1 = r20
.def lcd = r21
.def temp2 = r17
.def spin_counter = r18
.def speed = r19
.def digit = r22
.def digitCount = r23

.macro do_lcd_command                  ; LCD Command
	ldi temp2, @0                    ; Saves to r16
	rcall lcd_command              ; Goes to LCD Command macro
	rcall lcd_wait                 ; Wait b/c LCD is slow
.endmacro

.macro do_lcd_data                     ; do LCD Data
	ldi temp2, @0                    ; Saves to r16
	rcall lcd_data                 ; Goes to the LCD Data macro
	rcall lcd_wait                 ; Wait b/c LCD is slow
.endmacro

.macro do_lcd_command_reg
	mov temp2, @0
	rcall lcd_command
	rcall lcd_wait
.endmacro

.macro do_lcd_data_reg
	mov temp2, @0
	rcall lcd_data
	rcall lcd_wait
.endmacro  

.macro do_lcd_rdata
	mov lcd, @0
	subi lcd, -'0'
	rcall lcd_data
	rcall lcd_wait
.endmacro
.macro do_lcd_digits
	clr digit
	clr digitCount
	mov temp, @0			; temp is given number
	rcall convert_digits	; call a function
.endmacro  

.macro lcd_set                          ; LCD set
	sbi PORTA, @0                   ; Set Bit in I/O Register
.endmacro

.macro lcd_clr                          ; LCD clear
	cbi PORTA, @0                   ; Clear Bit in I/O Register
.endmacro         

                                        ;   As plugged into our board:
.equ LCD_RS = 7                         ; Register Select
.equ LCD_E = 6                          ; Enable pin - Operation start signal for data read/write.
.equ LCD_RW = 5                         ; Signal to select Read or Write     [�0�: Write, �1�: Read]
.equ LCD_BE = 4                         ; Not used in this code ?? not sure what for

                                        ; More on Register Select
                                        ; �0�: Instruction register (Write)
                                        ; : Busy flag; Address counter (Read)
                                        ; �1�: Data register (Write, Read)

.macro clear 
	ldi YL, low(@0)  ; loads the memory address to Y register
	ldi YH, high(@0)
	clr temp
	st Y+, temp		  ; clear the two bytes at @0 in SRAm
	st Y, temp
.endmacro

.dseg 
	TempCounter:	; Temporary counter. Used to determine if one second has passed
	.byte 2
	digits:
	.byte 4
	;TODO VoltageFlag:
	;TODO .byte 1

.cseg
.org 0x0000
	jmp RESET
	jmp DEFAULT
	jmp DEFAULT
.org INT2addr
	jmp EXT_INT2
.org OVF0addr
	jmp Timer0OVF	;jump to the interrupt handler for
					; Timer0 overflow

	;...
	jmp DEFAULT		; jmp  default at all other instructions
	
DEFAULT:
	reti
	

RESET:
	ldi temp, high(RAMEND) ; initialize stack
	out SPH, temp
	ldi temp, low(RAMEND)
	out SPL, temp
	
	ser temp2
	out DDRF, temp2                  ; LCD Data - Port F - output
	out DDRA, temp2                 ; LCD Control  - Port A - output

	clr temp2
	out PORTF, temp2                 ; remove all instructions that were sent to the LCD
	out PORTA, temp2

	do_lcd_command 0b00111000 ; 2x5x7          ; (Sets the interface data length to 8 bits and selects
                                                   ; 2-line display and 5 x 7-dot character font.)
	rcall sleep_5ms                            ; suggested long wait during setup
	do_lcd_command 0b00111000 ; 2x5x7
	rcall sleep_1ms
	do_lcd_command 0b00111000 ; 2x5x7          ; Repeated step - uses some set up time
	do_lcd_command 0b00111000 ; 2x5x7
	do_lcd_command 0b00001000 ; display off?
	do_lcd_command 0b00000001 ; clear display - cursor in initial position
	do_lcd_command 0b00000110 ; increment, no display shift
                              ; ( Sets mode to increment address by one and to shift the cursor
                              ; to the right at the time of write to internal RAM)
	do_lcd_command 0b00001110 ; Cursor on, bar, no blink (Turns on display and cursor.)


	do_lcd_data 'S'
	do_lcd_data 'P'
	do_lcd_data 'E'
	do_lcd_data 'E'
	do_lcd_data 'D'
	do_lcd_data ':'


	;clr temp
	;sts digits, temp
	;sts digits+1, temp
	;sts digits+2, temp
	;sts digits+3, temp

	ldi temp, (2<<ISC20)	; set INT2 as falling-edge
	sts EICRA, temp			;edge triggered interrupt
	in temp, EIMSK			; enable InT2
	ori temp, (1<<INT2)
	out EIMSK, temp 
	rjmp main
	
main:

	 clear TempCounter	; initializing tempCounter
	 ldi temp, 0b00000000
	 out TCCR0A, temp
	 ldi temp, 0b00000010
	 out TCCR0B, temp	; prescaling value of 8
	 ldi temp, 1<<TOIE0  
	 sts TIMSK0, temp	; T/C0 interrupt enable
	 sei

loop:
	rjmp loop

TIMER0OVF:
	in temp, SREG
	push temp	
	push YH
	push YL
	push r25
	push r24

	lds r24, TempCounter
	lds r25, TempCounter+1
	adiw r25:r24, 1
	cpi r24, low(3906)	;checking if 500ms has passed
	ldi temp, high(3906)
	cpc r25, temp
	brne NotSecond		;that is 'not half second'
	clear TempCounter

	;Speed = spin counter/2

	;clr r14

	mov speed, spin_counter
	lsr speed					;Divide spin counter by 2
	
	do_lcd_command 0b00000001	;clear display
	do_lcd_command 0b00000110	;increament, no display shift
	do_lcd_command 0b00001110	;Cursor on, bar, no blink

	do_lcd_data 'S';
	do_lcd_data 'P';
	do_lcd_data 'E';
	do_lcd_data 'E';
	do_lcd_data 'D';
	do_lcd_data ':';
	do_lcd_data ' ';

	do_lcd_digits speed
	clr speed
	clr spin_counter
	rjmp EndIF

NotSecond:
	sts TempCounter, r24
	sts TempCounter+1, r25

EndIF:
	pop r24
	pop r25
	pop YL
	pop YH
	pop temp
	out SREG, temp
	reti


EXT_INT2:
	push temp
	in temp, SREG
	push temp

	inc spin_counter

	pop temp
	out SREG, temp
	pop temp
	reti




;
; Send a command to the LCD (r16)
;

lcd_command:
	out PORTF, temp2             ; PORT F is LCD data, r16 sent to LCD
	rcall sleep_1ms
	lcd_set LCD_E              ; Enable bit set, so it does the command
	rcall sleep_1ms
	lcd_clr LCD_E              ; Enable bit cleared
	rcall sleep_1ms
	ret

lcd_data:
	out PORTF, temp2     ; temp2 contains the data being outputted
	lcd_set LCD_RS     ; Set top bit (bit 7) of LCD Control (PORT A), this means write/read to the LCD
	rcall sleep_1ms    ; Sleeps here b/c LCD is slower than AVR
	lcd_set LCD_E      ; Set bit 6
	rcall sleep_1ms
	lcd_clr LCD_E
	rcall sleep_1ms
	lcd_clr LCD_RS
	ret

lcd_wait:
	push temp2
	clr temp2
	out DDRF, temp2
	out PORTF, temp2
	lcd_set LCD_RW

lcd_wait_loop:
	rcall sleep_1ms
	lcd_set LCD_E
	rcall sleep_1ms
	in temp2, PINF
	lcd_clr LCD_E
	sbrc temp2, 7
	rjmp lcd_wait_loop
	lcd_clr LCD_RW
	ser temp2
	out DDRF, temp2
	pop temp2
	ret

.equ F_CPU = 16000000
.equ DELAY_1MS = F_CPU / 4 / 1000 - 4
; 4 cycles per iteration - setup/call-return overhead

sleep_1ms:
	push r24
	push r25
	ldi r25, high(DELAY_1MS)
	ldi r24, low(DELAY_1MS)

delayloop_1ms:
	sbiw r25:r24, 1
	brne delayloop_1ms
	pop r25
	pop r24
	ret

sleep_5ms:
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	ret


; function: displaying given number by digit in ASCII using stack
convert_digits:
	push digit
	;push temp
	;push temp1
	;push temp2
	checkHundreds:
		cpi temp, 100			; is the number still > 100?
		brsh hundredsDigit		; if YES - increase hundreds digit
		cpi digit, 0			
		brne pushHundredsDigit	; If digit ! 0 => this digit goes into stack
		
	checkTensInit:
		clr digit
	checkTens:
		ldi temp1, 10
		cp temp, temp1			; is the number still > 10? 
		brsh tensDigit			; if YES - increase tens digit
		cpi digitCount, 1		; were there hundred digits?
		breq pushTensDigit		; if YES i.e. digitCount==1 -> push the tens digit even if 0
								; otherwise: no hundreds are present
		cpi digit, 0			; is tens digit = 0?
		brne pushTensDigit		; if digit != 0 push it to the stack			 

	saveOnes:
		clr digit				; ones are always saved in stack
		mov digit, temp			; whatever is left in temp is the ones digit
		push digit				
		inc digitCount
	; now all digit temp data is in the stack
	; unload data into temp2, temp1, temp
	; and the do_lcd_rdata in reverse order
	; this will display the currentNumber value to LCD
	; it's not an elegant solution but will do for now
	cpi digitCount, 3
	breq dispThreeDigits
	cpi digitCount, 2
	breq dispTwoDigits
	cpi digitCount, 1
	breq dispOneDigit

	endDisplayDigits:
	;pop temp2
	;pop temp1
	;pop temp
	pop digit
	ret

; hundreds digit
hundredsDigit:
	inc digit				; if YES increase the digit count
	subi temp, 100			; and subtract a 100 from the number
	rjmp checkHundreds		; check hundreds again

; tens digit
tensDigit:
	inc digit				; if YES increase the digit count
	subi temp, 10			; and subtract a 10 from the number
	rjmp checkTens			; check tens again

pushHundredsDigit:
	push digit
	inc digitCount
	rjmp checkTensInit

pushTensDigit:
	push digit
	inc digitCount
	rjmp saveOnes

dispThreeDigits:
	pop temp2
	pop temp1
	pop temp
	do_lcd_rdata temp
	do_lcd_rdata temp1
	do_lcd_rdata temp2
	rjmp endDisplayDigits

dispTwoDigits:
	pop temp2
	pop temp1
	do_lcd_rdata temp1
	do_lcd_rdata temp2
	rjmp endDisplayDigits

dispOneDigit:
	pop temp
	do_lcd_rdata temp
	rjmp endDisplayDigits