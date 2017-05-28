;
; Project.asm
;
; Created: 24/05/2017 10:55:30 AM
; Author : Rhys
;

.include "m2560def.inc"

;General - notify other member if change any of these
.def temp1 = r16
.def temp2 = r17
.def address = r18
.def digit = r21
.def digit_count = r22
.def current_screen	=r23	;to keep track of what screen we are currently on where 0: start screen, 1: Select Screen, 2: Empty Screen, 3: Coin Screen, 4: Deliver Screen, 5: Admin Screen
.def row = r24
.def col = r25
.def rmask = r26
.def cmask = r27
.def debounceFlag0 = r28
.def debounceFlag1 = r29



;Constants----------------------------

;Delay Constants
.equ F_CPU = 16000000                               ; 16MHz
.equ DELAY_1MS = F_CPU / 4 / 1000 - 4 				; 4 cycles per iteration - setup/call-return overhead

;Keypad Constants
.equ PORTLDIR = 0xF0								;PD7-4: output, PD3-0, input
.equ INITCOLMASK = 0xEF								; scan from the rightmost column,
.equ INITROWMASK = 0x01								; scan from the top row
.equ ROWMASK = 0x0F									; for obtaining input from Port D

; LCD Instructions
;As plugged into our board
.equ LCD_RS = 7										; Register Select				
.equ LCD_E = 6										;Enable pin - Operation start signal for data read/write
.equ LCD_RW = 5										;Signal to select Read or Write  ["0": Write; "1": Read]
.equ LCD_BE = 4

.set LCD_DISP_ON = 0b00001110
.set LCD_DISP_OFF = 0b00001000
.set LCD_DISP_CLR = 0b00000001
.set LCD_FUNC_SET = 0b00111000 						; 2 lines, 5 by 7 characters
.set LCD_ENTR_SET = 0b00000110 						; increment, no display shift
.set LCD_HOME_LINE = 0b10000000 					; goes to 1st line (address 0)
.set LCD_SEC_LINE = 0b10101000 						; goes to 2nd line (address 40)

;Macros-------------------------------------------------------------------

.macro clear
	ldi YL, low(@0)     ; load the memory address to Y
    ldi YH, high(@0)
    clr temp 
    st Y+, temp         ; clear the two bytes at @0 in SRAM
    st Y, temp
.endmacro

; LCD Macros-----------------

.macro do_lcd_command
	ldi temp1, @0										;saves to temp1
	rcall lcd_command								;goes to LCD Command macro 
	rcall lcd_wait									; wait... LCD is slow
.endmacro

.macro do_lcd_command_reg
	mov temp1, @0										; saves t0 temp1
	rcall lcd_command								; goes to LCD data macro
	rcall lcd_wait									; wait ... LCD is slow
.endmacro

.macro do_lcd_data
	ldi temp1, @0
	rcall lcd_data
	rcall lcd_wait
.endmacro

.macro do_lcd_data_reg
	mov temp1, @0
	rcall lcd_data
	rcall lcd_wait
.endmacro

.macro lcd_set
	sbi PORTA, @0									;set bit in I/O register
.endmacro

.macro lcd_clr
	cbi PORTA, @0									; clear bit in I?O register
.endmacro

;DSEG and CSEG ---------------------------------------------------------
.dseg
	SecondCounter: .byte 2
	TempCounter: .byte 2
	DebounceCounter: .byte 2	;Used to determine if 50ms has passed for push button pressing

.cseg
.org 0x0000
	jmp RESET
	jmp EXT_INT0		;Handling for IRQ0 (button pushed)
	jmp EXT_INT1	;Handling for IRQ1 (button pushed)
	jmp DEFAULT		;Handling for IRQ2
.org OVF0addr
	jmp Timer0OVF	;Handling for Timer 0 overflow
jmp DEFAULT			;Handling for all other interrupts

;DEFAULT AND RESET --------------------------------------------
DEFAULT: reti

RESET:
	;Stack Pointer initialisation
	ldi temp1, low(RAMEND)
	out SPL, temp1
	ldi temp1, high(RAMEND)
	out SPH, temp1

	;Keypad
	ldi temp1, PORTLDIR ; PL7:4/PL3:0, out/in
	sts DDRL, temp1	

	;LEDs
	ser temp1	;set PORTC (LEDS) as output
	out DDRC, temp1

	;Button interrupt
	ldi temp1, (2<<ISC00)	;set INT0 as falling-edge interrupt
	sts EICRA, temp1
	in temp1, EIMSK	;enable INT0
	ori temp1, (1<<INT0)
	out EIMSK, temp1

	ldi temp1, (2<<ISC10)	;set INT1 as falling-edge interrupt
	sts EICRA, temp1
	in temp1, EIMSK			;enable INT1
	ori temp1, (1<<INT1)
	out EIMSK, temp1

	;Timer Interrupt
	clear TempCounter
	clear SecondCounter
	
	ldi temp1, 0b00000000
	out TCCR0A, temp1
	ldi temp1, 0b00000010	;set prescaler to 8 = 128 ms
	out TCCR0B, temp1
	ldi temp1, (1<<TOIE0)
	sts TIMSK0, temp1
	
	sei 

	;LCD
	ser temp1
	out DDRF, temp1			;set PORTF(LCD Data) and PORTA(LCD Control) to output
	out DDRA, temp1
	clr temp1				;clear PORTF and PORTA registers
	out PORTF, temp1
	out PORTA, temp1

	do_lcd_command LCD_FUNC_SET 					; initialise LCD
	rcall sleep_5ms
	do_lcd_command LCD_FUNC_SET
	rcall sleep_1ms

	do_lcd_command LCD_FUNC_SET
	do_lcd_command LCD_FUNC_SET
	do_lcd_command LCD_DISP_OFF
	do_lcd_command LCD_DISP_CLR
	do_lcd_command LCD_ENTR_SET
	do_lcd_command LCD_DISP_ON  

	ldi temp1, LCD_HOME_LINE					; initialise variables
	mov address, temp1

	;TODO other parts

	rjmp main

;INTERRUPTS-----------------------------------------------------
;Button Interrupts-----------
EXT_INT0:
	;TODO Handling for EXT_INT0

EXT_INT1:
	;TODO Handling for EXT_INT1


;Timer Interrupts--------------
Timer0OVF:
	in temp1, SREG ;Save status register
	push temp1
	push YH
	push YL
	push r25
	push r24

	cpi debounceFlag0, 1 ;if either flags have been set run debounce timer
	breq new_bounce_timer
	cpi debounceFlag1, 1
	breq new_bounce_timer
	rjmp continue_timer ;else continue with the second timer

	new_bounce_timer:
		lds r24, DebounceCounter
		lds r25, DebounceCounter+1
		adiw r24:r25, 1

		cpi r24, low(780) ;for 50 ms say
		ldi temp, high(780)
		cpc r25, temp
		brne NotFif
		clr debounceFlag0	;if 50ms has passed clear the flags and counter
		clr debounceFlag1
		clear DebounceCounter
		clr r24
		clr r25
		rjmp continue_timer

	NotFif:
		sts DebounceCounter, r24
		sts DebounceCounter+1, r25

	continue_timer:
		lds r24, TempCounter
		lds r25, TempCounter+1
		adiw r25:r24, 1

		cpi r24, low(7812) 
		ldi temp, high(7812)
		cpc r25, temp
		brne NotSecond
		;TODO What to do after second has passed
		clear TempCounter

		lds r24, SecondCounter
		lds r25, SecoundCounter+1
		adiw r25:r24, 1		;Increase second counter by 1
		sts SecondCounter, r24
		sts SecondCounter+1, r25
		rjmp EndIF

NotSecond:
	sts TempCounter, r24	;Store the new value of the temporary counter
	sts TempCounter+1, r25
	rjmp EndIF

EndIF:
	pop r24
	pop r25
	pop YL
	pop YH
	pop temp1
	out SREG, temp1
	reti


main:
	;Keypad
	ldi cmask, INITCOLMASK		;initial column mask
	clr col						;initial column

colloop:
	col_prologue:
		ldi cmask, INITCOLMASK
		clr col
	cpi col, 4
	breq col_prologue ; If all keys are scanned, repeat.
	sts PORTL, cmask ; Otherwise, scan a column.

	ldi temp1, 0xFF ; Slow down the scan operation.

delay: 
	dec temp1
	brne delay ;until temp 1 is zero we delay

	lds temp1, PINL ; Read PORTA
	andi temp1, ROWMASK ; Get the keypad output value
	cpi temp1, 0xF ; Check if any row is low
	breq nextcol

	; If yes, find which row is low
	ldi rmask, INITROWMASK ; Initialize for row check
	clr row ; 

rowloop:
	cpi row, 4
	breq nextcol ; the row scan is over.

	mov temp2, temp1
	and temp2, rmask ; check un-masked bit
	breq convert ; if bit is clear, the key is pressed

	inc row ; else move to the next row
	lsl rmask
	
	jmp rowloop

nextcol: ; if row scan is over
	lsl cmask
	inc col ; increase column value
	jmp colloop ; go to the next column

convert:
	cpi col, 3 ; If the pressed key is in col.3
	breq letters ; we have a letter
					; If the key is not in col.3 and
	cpi row, 3 ; If the key is in row3,
	breq symbols ; we have a symbol or 0

	numbers: ;else its a number (NOT ZERO)
		mov temp1, row ; Otherwise we have a number in 1-9
		lsl temp1
		add temp1, row
		add temp1, col ; temp1 = row*3 + col
		;temp1 now has the value of the button pushed

	jmp convert_end

letters:
	//TODO letters handling for each screen
	;Should be in the form if current_screen is this do this
	;if current_screen is this then do this
	jmp main

symbols:
	cpi col, 1 ; or if we have zero
	breq zero
	
	;TODO symbol handling for each screen

zero:
	ldi temp1, '0' ; Set to zero
	;TODO zero handling for each screen
	jmp convert_end

convert_end:
	jmp colloop ; Restart main loop

;LCD Commands-----------------------------------------------------------------------------------
lcd_command:
	out PORTF, temp1					;Port F is LCD data, temp1 sent to LCD
	rcall sleep_1ms
	lcd_set LCD_E					;Enable bit set so it does the command
	rcall sleep_1ms
	lcd_clr LCD_E					;Enable bit cleared
	rcall sleep_1ms
	ret

lcd_data:
	out PORTF, temp1					;temp1 contains the data being outputted
	lcd_set LCD_RS					; Set top bit (bit 7) of LCD Control (Port A) 
									; this determines read/ write to the LCD
	rcall sleep_1ms					;wait ... LCD is slow
	lcd_set LCD_E					;Set bit 6
	rcall sleep_1ms
	lcd_clr LCD_E
	rcall sleep_1ms
	lcd_clr LCD_RS
	ret

lcd_wait:
	push temp1
	clr temp1
	out DDRF, temp1
	out PORTF, temp1
	lcd_set LCD_RW

lcd_wait_loop:
	rcall sleep_1ms
	lcd_set LCD_E
	rcall sleep_1ms
	in temp1, PINF
	lcd_clr LCD_E
	sbrc temp1, 7
	rjmp lcd_wait_loop
	lcd_clr LCD_RW
	ser temp1
	out DDRF, temp1
	pop temp1
	ret

; Delay commands

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


sleep_20ms:
	rcall sleep_5ms
	rcall sleep_5ms
	rcall sleep_5ms
	rcall sleep_5ms
	rcall sleep_5ms
	ret


;SCREENS--------------------------------------------------------------------------------------------

;Start Screen------------------
;should show 2121 17s1 (dunno what our group name is) then next line Vending Machine
;Then 3 seconds later, or until any keypad button is pressed should go to select screen
start_screen:
	ldi current_screen, 0
	;TODO


;Select Screen-----------------
;Pressing 1-9 should try to retrieve the corresponding item, if in inventory -> coin screen else -> empty screen
select_screen:
	ldi current_screen, 1
	;TODO


;Empty Screen-----------------
;display Out of Stock with number of item trying to be retrieved on the second line
;Stay in state for 3 seconds before returning to select screen
;All 10 LEDS should be on for the first half of the 3 seconds then off for the second half
empty_screen:
	ldi current_screen, 2
	;TODO

;Coin Screen----------------
coin_screen:
	ldi current_screen, 3
	;TODO

;Deliver Screen
deliver_screen:
	ldi current_screen, 4
	;TODO

;Admin Screen
admin_screen:
	ldi current_screen, 5
	;TODO




	
 


convert_digits: ;given number stored in temp1 
	;TODO 

