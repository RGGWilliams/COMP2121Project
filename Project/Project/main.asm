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
.def star_pressed = r12
.def second_counter = r10
.def current_item = r11
.def new_screen_flag = r23
.def current_screen	=r22	;to keep track of what screen we are currently on where 0: start screen, 1: Select Screen, 2: Empty Screen, 3: Coin Screen, 4: Deliver Screen, 5: Admin Screen
.def row = r24
.def col = r25
.def rmask = r18
.def cmask = r19
.def debounceFlag0 = r20
.def debounceFlag1 = r21
.def tempcount = r13
.def tempcost = r14
.def address = r15
.def coins_needed = r9
.def new_star_flag = r8
.def interrupt_counter = r7
.def adc_L = r4
.def adc_H = r5


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
	push temp1
	ldi YL, low(@0)     ; load the memory address to Y
    ldi YH, high(@0)
    clr temp1 
    st Y+, temp1        ; clear the two bytes at @0 in SRAM
    st Y, temp1
	pop temp1
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
	TempCounter: .byte 2
	TempCounter1: .byte 2
	DebounceCounter: .byte 2	;Used to determine if 50ms has passed for push button pressing
	Item1: .byte 2				;Item stores where first byte is the inventory count and the second byte is the coin cost 
	Item2: .byte 2				;Coin cost accessed by eg Item1+1
	Item3: .byte 2
	Item4: .byte 2
	Item5: .byte 2
	Item6: .byte 2
	Item7: .byte 2
	Item8: .byte 2
	Item9: .byte 2
	currPress: .byte 1
	wasPress: .byte 1

.cseg
.org 0x0000
	jmp RESET
.org INT0addr
	jmp EXT_INT0		;Handling for IRQ0 (button pushed)
	jmp EXT_INT1	;Handling for IRQ1 (button pushed)

	jmp DEFAULT
	jmp DEFAULT
	jmp DEFAULT    ;Handling for Potentiometer
	jmp DEFAULT		;Handling for IRQ2
.org OVF0addr
	jmp Timer0OVF	;Handling for Timer 0 overflow
.org OVF1addr
	jmp Timer1OVF	;Handling for Timer 1 overflow
.org ADCCADDR
	jmp ADC_ISR
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

	clr temp1
	sts currPress, temp1
	sts wasPress, temp1

	;LEDs
	ser temp1	;set PORTC (LEDs) as output
	out DDRC, temp1
	ser temp1	;set PG2 and PG3 as output (the other LEDs)
	out DDRG, temp1

	;Button interrupt
	clear DebounceCounter
	clr DebounceFlag0
	clr DebounceFlag1
	ldi temp1, 1
	mov current_item, temp1

	sei


	ldi temp1, (2<<ISC00)	;set INT0 as falling-edge interrupt
	sts EICRA, temp1
	in temp1, EIMSK	;enable INT0
	ori temp1, (1<<INT0)
	out EIMSK, temp1

	sei

	ldi temp1, (2<<ISC10)	;set INT1 as falling-edge interrupt
	sts EICRA, temp1
	in temp1, EIMSK			;enable INT1
	ori temp1, (1<<INT1)
	out EIMSK, temp1
	
	sei

	;Timer Interrupt
	clear TempCounter
	clear TempCounter1
	
	ldi temp1, 0b00000000
	out TCCR0A, temp1
	ldi temp1, (2<<CS00)	;set prescaler to 8 = 128 ms
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

	;Item counts
	clear Item1
	clear Item2
	clear Item3
	clear Item4
	clear Item5
	clear Item6
	clear Item7
	clear Item8
	clear Item9

	;Keypad
	;Potentiometer - connected to PK2
	sei
	;initialise to read 
	ldi temp1, (3<<REFS0)|(0<<ADLAR)|(0<<MUX0)
	sts ADMUX, temp1
	ldi temp1, (1<<MUX5)
	sts ADCSRB, temp1
	ldi temp1, (1<<ADEN)|(1<<ADSC)|(1<<ADIE)|(5<<ADPS0) 
	sts ADCSRA, temp1
	
	clr coins_needed
	clr interrupt_counter

	;motor
	sei 
	ser temp1
	out DDRE, temp1

	ldi temp1, 0b00000000
	sts TCCR1A, temp1
	ldi temp1, 0b00000010	;set prescaler to 8 = 128 ms
	sts TCCR1B, temp1

	;speaker 
	ldi temp1, 0b00000001
	out DDRB, temp1

	rjmp start_screen

	loop:
		rjmp loop


;INTERRUPTS-----------------------------------------------------
;Button Interrupts-----------
EXT_INT0:						;Right button
	push temp1
	push temp2
	in temp1, SREG
	push temp1

	cpi debounceFlag0, 1		;If flag is set end
	breq END_INT

	ldi debounceFlag0, 1		;set debounce flag

	cpi current_screen, 5	;Check for admin screen
	breq admin_right
	rjmp END_INT

	admin_right:
		call get_item
		ldi temp1, 10			;check to see if inventory count is already at 10
		cp tempcount, temp1
		breq END_INT			;if so don't increment
		mov temp1, tempcount
		inc temp1
		mov tempcount, temp1
		call set_item
		rjmp END_INT



EXT_INT1:		
	push temp1
	push temp2
	in temp1, SREG
	push temp1

	cpi debounceFlag1, 1		;If flag is set end
	breq END_INT

	ldi debounceFlag1, 1		;set debounce flag

	cpi current_screen, 5	;Check for admin screen
	breq admin_left
	rjmp END_INT

	admin_left:
		call get_item
		ldi temp1, 0
		cp tempcount, temp1
		breq END_INT
		mov temp1, tempcount
		dec temp1
		mov tempcount, temp1
		call set_item
		rjmp END_INT

END_INT:
	pop temp1
	out SREG, temp1
	pop temp2
	pop temp1
	cpi current_screen, 5		;If in admin screen need to update the screen
	breq update_admin_hop		
	reti

update_admin_hop:
	call update_admin_screen

;Timer Interrupts--------------
Timer0OVF:
	push temp1
	push temp2
	in temp1, SREG ;Save status register
	push temp1
	push r25
	push r24
	push YH
	push YL

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
		ldi temp1, high(780)
		cpc r25, temp1
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

		cpi current_screen, 0
		breq start_timer

		cpi current_screen, 1
		breq select_timer

		cpi current_screen, 2
		breq empty_timer_hop

		rjmp ENDIF

	empty_timer_hop:
	jmp empty_timer

	start_timer:
		cpi new_screen_flag, 1	;if screen has just been changed to start screen then start a new timer
		breq new_start_timer

		cpi r24, low(23436)		;check to see if 3 seconds has passed
		ldi temp1, high(23436)
		cpc r25, temp1
		brne NotThreeSeconds
		pop YL
		pop YH
		pop r24
		pop r25
		pop temp1
		out SREG, temp1
		pop temp2
		pop temp1
		rjmp select_screen		;else if 3 seconds has passed jump to select screen

		new_start_timer:
			ldi new_screen_flag, 0	;not a new screen anymore
			clear TempCounter	;start 'new' timer
			rjmp EndIF

		NotThreeSeconds:
			sts TempCounter, r24	;Store the new value of the temporary counter
			sts TempCounter+1, r25
			rjmp EndIF	

	select_timer:
		ldi temp1, 1
		cp star_pressed, temp1		;if star has been pressed then start a new timer
		brne ENDIF_hop

		ldi temp1, 1
		cp new_star_flag, temp1
		breq new_select_timer

		cpi r24, low(7812)			;check if one second has passed
		ldi temp1, high(7812)
		cpc r25, temp1
		brne NotSecond				;if not jump to not second and store tempcounter
		inc second_counter			;if so increment the number of seconds had and clear TempCounter to count the next second
		clear TempCounter
		ldi temp1, 5
		cp second_counter, temp1	;if this is now 5 seconds then go to admin screen else
		brne EndIF_hop
		pop YL
		pop YH
		pop r24
		pop r25
		pop temp1
		out SREG, temp1
		pop temp2
		pop temp1
		rjmp admin_screen

		new_select_timer:
			clear TempCounter	;start 'new' timer
			clr second_counter
			ldi temp1, 0
			mov new_star_flag, temp1
			rjmp EndIF

		NotSecond:
			sts TempCounter, r24	;Store the new value of the temporary counter
			sts TempCounter+1, r25
			rjmp EndIF	

	ENDIF_hop:
		jmp ENDIF
	
	empty_timer:
		cpi new_screen_flag, 1	;if screen has just been changed to start screen then start a new timer
		breq new_empty_timer

		cpi r24, low(11718)		;check to see if 1.5 seconds has passed
		ldi temp1, high(11718)
		cpc r25, temp1
		brne NotHalfThreeSeconds
		clr temp1				;turn off all LEDs after 1.5 seconds
		out PORTC, temp1
		out PORTG, temp1
		cpi r24, low(23436)	
		ldi r25, high(23436)
		cpc r25, temp1			;check to see if 3 seconds has passed
		brne NotThreeSecondsEmpty
		pop YL
		pop YH
		pop r24
		pop r25
		pop temp1
		out SREG, temp1
		pop temp2
		pop temp1
		jmp select_screen

		new_empty_timer:
			ldi new_screen_flag, 0	;not a new screen anymore
			clear TempCounter	;start 'new' timer
			ser temp1
			out PORTC, temp1		;turn on all port C LEDs
			out PORTG, temp1		;turn on the 2 port G LEDs
			rjmp EndIF

		NotHalfThreeSeconds:
			sts TempCounter, r24	;Store the new value of the temporary counter
			sts TempCounter+1, r25
			rjmp EndIF
			
		NotThreeSecondsEmpty:
			sts TempCounter, r24
			sts TempCounter+1, r25
			rjmp EndIF


EndIF:
    pop YL
	pop YH
	pop r24
	pop r25
	pop temp1
	out SREG, temp1
	pop temp2
	pop temp1
	reti

keypad_prologue:
	ldi cmask, INITCOLMASK
	clr col
	jmp colloop

keysScanned:
	ldi temp1, 0 									; set currPress = 0
	sts currPress, temp1
	mov star_pressed, temp1
	rjmp keypad_prologue

colloop:
	cpi col, 4 										; compare current column # to total # columns
	breq keysScanned								; if all keys are scanned, repeat
	sts PORTL, cmask								; otherwise, scan a column

	ldi temp1, 0xFF									; slow down the scan operation to debounce button press
	delay:
	dec temp1
	brne delay
	rcall sleep_20ms

	lds temp1, currPress 							; if currPress = 0, set wasPress = 0
	cpi temp1, 1
	brne notPressed
	ldi temp1, 1									; set wasPress = 1
	sts wasPress, temp1
	rjmp scan
	notPressed:
		ldi temp1, 0 								; set wasPress = 0
		rcall sleep_5ms
		sts wasPress, temp1

	scan:
	lds temp1, PINL									; read PORTL
	andi temp1, ROWMASK								; get the keypad output value
	cpi temp1, 0xF0 								; check if any row is low (0)
	breq rowloop									; if yes, find which row is low
	ldi rmask, INITROWMASK							; initialize rmask with 0000 0001 for row check
	clr row

rowloop:
	cpi row, 4 										; compare current value of row with total number of rows (4)
	breq nextcol									; if theyre equal, the row scan is over.
	mov temp2, temp1 								; temp1 is 0xF
	and temp2, rmask 								; check un-masked bit
	breq convert 									; if bit is clear, the key is pressed
	inc row 										; else move to the next row
	lsl rmask 										; shift row mask left by one
	jmp rowloop

nextcol:											; if row scan is over
	lsl cmask 										; shift column mask left by one
	inc col 										; increase column value
	jmp colloop

convert:
	ldi temp1, 1
	sts currPress, temp1		;if wasPress = 1 ignore the press
	lds temp1, wasPress
	cpi temp1, 1
	breq keypad_prologue

	cpi current_screen, 0	;if the current screen is the start screen a button pressed means it should go to the next screen
	breq button_pressed

	cpi col, 3 ; If the pressed key is in col.3
	breq letters ; we have a letter
					; If the key is not in col.3 and
	cpi row, 3 ; If the key is in row3,
	breq symbols_hop ; we have a symbol or 0

	rjmp numbers		;else we have a number

symbols_hop:
	jmp symbols

button_pressed:
	jmp select_screen

numbers: ;else its a number (NOT ZERO)
	mov temp1, row ; Otherwise we have a number in 1-9
	lsl temp1
	add temp1, row
	add temp1, col ; temp1 = row*3 + col
	inc temp1
	;temp1 now has the value of the button pushed
	numbers_computed:
		cpi current_screen, 1	;check if the current screen is the select screen
		breq numbers_select

		cpi current_screen, 5	;check if current screen is admin
		breq numbers_admin

	jmp keypad_prologue

	numbers_select:
		mov current_item, temp1
		call get_item
		ldi temp2, 0
		cp tempcount, temp2
		breq empty_hop
		jmp coin_screen
	
	numbers_admin:
		mov current_item, temp1
		rjmp update_admin_screen

empty_hop:
	call empty_screen		

letters:
	cpi current_screen, 5	;check if current screen is admin
	breq letters_admin
	jmp keypad_prologue

	letters_admin:
		cpi row, 0
		breq letters_admin_A

		cpi row, 1
		breq letters_admin_B
		
		cpi row, 3			;If C do nothing
		breq update_admin_hop2

		call get_item
		ldi temp1, 0
		mov tempcount, temp1
		call set_item
		jmp update_admin_screen

		letters_admin_A:
			call get_item
			ldi temp1, 3
			cp tempcost, temp1
			breq update_admin_hop2
			mov temp1, tempcost
			inc temp1
			mov tempcost, temp1
			call set_item
			jmp update_admin_screen

		letters_admin_B:
			call get_item
			ldi temp1, 0
			cp tempcost, temp1
			breq update_admin_hop2
			mov temp1, tempcost
			dec temp1
			mov tempcost, temp1
			call set_item
			jmp update_admin_screen

update_admin_hop2:
	call update_admin_screen

symbols:
	cpi col, 1 ; or if we have zero
	breq zero

	cpi current_screen, 1 ;check for select screen
	breq symbols_select

	cpi current_screen, 3 ;check for coin screen
	breq symbols_coin

	cpi current_screen, 5 ;check for admin screen
	breq symbols_admin

	jmp keypad_prologue

	symbols_select:
		cpi col, 0
		breq star_pushed
		jmp keypad_prologue

	star_pushed:
		ldi temp1, 0
		cp star_pressed, temp1			;if star pressed is 0 then must've been pushed for first time
		brne keypad_prologue_hop
		ldi temp1, 1
		mov new_star_flag, temp1			;star pushed for first time
		ldi temp1, 1
		mov star_pressed, temp1			;set star pressed to 1
		jmp keypad_prologue	

	symbols_coin:
		cpi col, 2	;check to see if # pressed
		brne keypad_prologue_hop
		jmp CoinRet
		;breq call_CoinRet
		;jmp call_acd

	symbols_admin:
		cpi col, 2	;check to see if # pressed
		brne keypad_prologue_hop
		clr temp1
		out PORTC, temp1
		out PORTG, temp1
		rjmp select_screen	;if pressed go to select_screen

keypad_prologue_hop:
	jmp keypad_prologue

zero:
	;ldi temp1, 0 ; Set to zero
	;rjmp numbers_computed
	;TODO zero handling for each screen
	jmp keypad_prologue

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


get_item:
	push temp1
	ldi YH, high(Item1)
	ldi YL, low(Item1)
	mov temp1, current_item
	get_item_loop:
		cpi temp1, 0
		breq end_item_count
		ld tempcount, Y+	
		ld tempcost, Y+
		dec temp1
		rjmp get_item_loop
	end_item_count:
		pop temp1
		ret

set_item:
	push temp1
	push temp2

	ldi YH, high(Item1)
	ldi YL, low(Item1)
	mov temp1, current_item
	set_item_loop:
		cpi temp1, 1
		breq end_item_set
		ld temp2, Y+
		ld temp2, Y+
		dec temp1
		rjmp set_item_loop
	end_item_set:
		st Y+, tempcount
		st Y+, tempcost
		pop temp2
		pop temp1
		ret





	


;SCREENS--------------------------------------------------------------------------------------------

;Start Screen------------------
;should show 2121 17s1 (dunno what our group name is) then next line Vending Machine
;Then 3 seconds later, or until any keypad button is pressed should go to select screen
start_screen:
	ldi current_screen, 0
	ldi new_screen_flag, 1
	do_lcd_data '2'
	do_lcd_data '1'
	do_lcd_data '2'
	do_lcd_data '1'
	do_lcd_data ' '
	do_lcd_data '1'
	do_lcd_data '7'
	do_lcd_data 's'
	do_lcd_data '1'
	do_lcd_data ' '
	do_lcd_data ' '
	do_lcd_data ' '
	do_lcd_data ' '
	do_lcd_data 'M'
	do_lcd_data '2'
	do_lcd_command LCD_SEC_LINE
	do_lcd_data 'V'
	do_lcd_data 'e'
	do_lcd_data 'n'
	do_lcd_data 'd'
	do_lcd_data 'i'
	do_lcd_data 'n'
	do_lcd_data 'g'
	do_lcd_data ' '
	do_lcd_data 'M'
	do_lcd_data 'a'
	do_lcd_data 'c'
	do_lcd_data 'h'
	do_lcd_data 'i'
	do_lcd_data 'n'
	do_lcd_data 'e'
	rcall starting_inventory
	jmp keypad_prologue

starting_inventory:
	ldi temp1, 1
	starting_inventory_loop:		;loop over each item setting inventory as number of items
		cpi temp1, 10
		breq starting_cost
		mov current_item, temp1
		call get_item				;tempcount tempcost now have old values of current_item (which is temp1)
		mov tempcount, temp1
		rcall set_item				;sets tempcount to temp1 of current_item
		inc temp1
		rjmp starting_inventory_loop

starting_cost:
	ldi temp1, 1
	starting_cost_odd_loop:		;loop over each odd numbered item setting cost to 1 coin. Temp1 will be current item to be looked at
		cpi temp1, 11
		breq starting_cost_even
		mov current_item, temp1
		call get_item			;saves old values of count and cost into tempcount tempcost
		ldi temp2, 1
		mov tempcost, temp2
		call set_item
		inc temp1
		inc temp1
		rjmp starting_cost_odd_loop
starting_cost_even:
	ldi temp1, 2
	starting_cost_even_loop:		;loop over each even numbered item setting cost to 2 coins
		cpi temp1, 10
		breq starting_done
		mov current_item, temp1
		call get_item			;saves old values of count and cost into tempcount tempcost
		ldi temp2, 2
		mov tempcost, temp2
		call set_item
		inc temp1
		inc temp1
		rjmp starting_cost_even_loop
starting_done:
	ldi temp1, 9
	mov current_item, temp1
	call get_item
	ldi temp1, 0
	mov tempcount, temp1
	call set_item
	ret

;halt:
;	rjmp halt

;Select Screen-----------------
;Pressing 1-9 should try to retrieve the corresponding item, if in inventory -> coin screen else -> empty screen
select_screen:
	ldi current_screen, 1
	do_lcd_command LCD_DISP_CLR
	do_lcd_command LCD_HOME_LINE
	do_lcd_data 'S'
	do_lcd_data 'e'
	do_lcd_data 'l'
	do_lcd_data 'e'
	do_lcd_data 'c'
	do_lcd_data 't'
	do_lcd_data ' '
	do_lcd_data 'i'
	do_lcd_data 't'
	do_lcd_data 'e'
	do_lcd_data 'm'

	jmp keypad_prologue
	;all handling in button areas


;Empty Screen-----------------
;display Out of Stock with number of item trying to be retrieved on the second line
;Stay in state for 3 seconds before returning to select screen
;All 10 LEDS should be on for the first half of the 3 seconds then off for the second half
empty_screen:
	ldi current_screen, 2
	ldi new_screen_flag, 1
	do_lcd_command LCD_DISP_CLR
	do_lcd_command LCD_HOME_LINE
	do_lcd_data 'O'
	do_lcd_data 'u'
	do_lcd_data 't'
	do_lcd_data ' '
	do_lcd_data 'o'
	do_lcd_data 'f'
	do_lcd_data ' '
	do_lcd_data 's'
	do_lcd_data 't'
	do_lcd_data 'o'
	do_lcd_data 'c'
	do_lcd_data 'k'
	do_lcd_command LCD_SEC_LINE

	mov temp1, current_item
	rcall write_digits		;write out current_item
	empty_loop:
		rcall empty_loop		;loop until timers send back to select_screen

;Coin Screen----------------
coin_screen:
	ldi current_screen, 3
	call get_item
	mov coins_needed, tempcost

	coin_screen_update:

	do_lcd_command LCD_DISP_CLR
		do_lcd_command LCD_HOME_LINE
		do_lcd_data 'I'
		do_lcd_data 'n'
		do_lcd_data 's'
		do_lcd_data 'e'
		do_lcd_data 'r'
		do_lcd_data 't'
		do_lcd_data ' '
		do_lcd_data 'c'
		do_lcd_data 'o'
		do_lcd_data 'i'
		do_lcd_data 'n'
		do_lcd_data 's'
		do_lcd_command LCD_SEC_LINE
	
		mov temp1, coins_needed
		rcall write_digits    		;write  tmp cost
		jmp keypad_prologue			;check for # to be pressed


    ADC_ISR:
    	push temp1
		in temp1, SREG
		push temp1
		push adc_L
		push adc_H
		push interrupt_counter

		lds adc_L, ADCL		
		lds adc_H, ADCH
		

		ldi temp1, 1
		cp interrupt_counter, temp1
		breq check_max

		ldi temp1, low(0)
		cp adc_L, temp1		; check whether interrupt reads 0
		ldi temp1, high(0)
		cpc adc_H, temp1
		brne adc_end
		inc interrupt_counter
		rjmp check_coin		

		check_max:
			ldi temp1, low(0x3FF)				; check for max value 
			cp adc_L, temp1	
			ldi temp1, high(0x3FF)
			cpc adc_H, temp1
			brne adc_end
			inc interrupt_counter
			rjmp adc_end
    
	out PORTC,interrupt_counter

	    check_coin:			; if interrupt_counter=3 then coin has been inserted 
			ldi temp1, 3
			cp interrupt_counter, temp1
			;brne adc_end
			breq coin_loop
	    
	 adc_end:
	    pop interrupt_counter
	    pop adc_H
	    pop adc_L
		pop temp1
		out SREG, temp1
		pop temp1
		reti

     
		coin_loop:										;dealing with the potentiometer
		    push temp1
			push interrupt_counter
			push coins_needed

			clr interrupt_counter
		    dec coins_needed
			ldi temp1, 0
		    cp coins_needed, temp1
		    breq go_deliver_screen

			pop coins_needed
			pop interrupt_counter
			pop temp1
			rjmp coin_screen_update

			go_deliver_screen:
			    pop coins_needed
				pop interrupt_counter
				pop temp1
				jmp deliver_screen
		
		CoinRet:
		    push temp1
		    push coins_needed

			CoinRetLoop:
		    inc coins_needed
			rcall startMotor
			cp coins_needed, tempcost
			breq EndCoinRet
			rjmp CoinRetLoop
			
		    	EndCoinRet:
		    	    pop coins_needed 
		    	    pop temp1
					jmp select_screen


;Deliver Screen
deliver_screen:
	ldi current_screen, 4

	do_lcd_command LCD_DISP_CLR
		do_lcd_command LCD_HOME_LINE
		do_lcd_data 'D'
		do_lcd_data 'e'
		do_lcd_data 'l'
		do_lcd_data 'i'
		do_lcd_data 'v'
		do_lcd_data 'e'
		do_lcd_data 'r'
		do_lcd_data 'i'
		do_lcd_data 'n'
		do_lcd_data 'g'
		do_lcd_data ' '
		do_lcd_data 'I'
		do_lcd_data 't'
		do_lcd_data 'e'
		do_lcd_data 'm'
		
		rjmp startMotor
		clr temp1
		sts TempCounter, temp1
		rjmp Timer1OVF

;Motor Control -------------------------------------------------


startMotor:
	push temp1
	

	ldi temp1, (1 << TOIE1) 							; enable timer
	sts TIMSK0, temp1
	ldi temp1, low(0xFF)						; start motor
	sts OCR3AH, temp1
	ldi temp1, high(0xFF)
	sts OCR3AL, temp1

	ldi new_screen_flag, 1
	clear TempCounter1
	
	pop temp1
	ret

stopMotor:
	push temp1
	
	ldi temp1, (0 << TOIE1) 							; disable timer
	sts TIMSK0, temp1
	clr temp1										; stop motor
	sts OCR3AH, temp1
	sts OCR3AL, temp1
	
	pop temp1
	ret

Timer1OVF:						; interrupt subroutine to Timer1
	push temp1
	in temp1, SREG
	push temp1 					; save conflict registers
	push r25
	push r24

	lds r24, TempCounter1 		; load value of temporary counter
	lds r25, TempCounter1 + 1
	adiw r25:r24, 1 			; increase temporary counter by 1
	
	
	cpi current_screen, 4
		breq deliver_timer

	cpi current_screen, 3
		breq coin_timer
	
	
	deliver_timer:
		cpi new_screen_flag, 1	;if screen has just been changed to start screen then start a new timer
		breq new_deliver_timer

		cpi r24, low(11718)		;check to see if 1.5 seconds has passed
		ldi temp1, high(11718)
		cpc r25, temp1
		brne NotHalfThree
		
		cpi r24, low(23436)	
		ldi r25, high(23436)
		cpc r25, temp1			;check to see if 3 seconds has passed
		brne NotThree
		rcall stopMotor
		rjmp END

		new_deliver_timer:
			ldi new_screen_flag, 0	;not a new screen anymore
			clear TempCounter1	;start 'new' timer
			rjmp END

		NotHalfThree:
			ser temp1
			out PORTC, temp1		;turn on all port C LEDs
			ldi temp1, 0b00110000
			out PORTG, temp1		;turn on the 2 port G LEDs
			sts TempCounter1, r24	;Store the new value of the temporary counter
			sts TempCounter1+1, r25
			rjmp END
			
		NotThree:
			clr temp1				;if 3 seconds has passed turn off leds and go back to select screen
			out PORTC, temp1
			out PORTG, temp1
			sts TempCounter1, r24
			sts TempCounter1+1, r25
			rjmp END
	
	  coin_timer:
		cpi new_screen_flag, 1	;if screen has just been changed to start screen then start a new timer
		breq new_coin_timer

		cpi r24, low(1953)		;check to see if 0.25 seconds has passed
		ldi temp1, high(1953)
		cpc r25, temp1
		brne NotQuarterSecond
		;0.25 seconds has passed
		rcall stopMotor
		
		cpi r24, low(3902)	
		ldi r25, high(3902)
		cpc r25, temp1			;check to see if 0.5 seconds has passed
		brne NotHalfSecond
		rjmp END
		

		new_coin_timer:
			ldi new_screen_flag, 0	;not a new screen anymore
			clear TempCounter1	;start 'new' timer
			rjmp END

		NotQuarterSecond:
			sts TempCounter1, r24	;Store the new value of the temporary counter
			sts TempCounter1+1, r25
			rjmp END
			
		NotHalfSecond:
			sts TempCounter1, r24
			sts TempCounter1+1, r25
			rjmp END
	
	END:
		pop r24
		pop r25
		pop temp1
		out SREG, temp1
		pop temp1
		reti 	


	


;Admin Screen
admin_screen:
	ldi current_screen, 5
	ldi temp1, 1
	mov current_item, temp1		;default item selected is 1. temp1 will hold item selected
	update_admin_screen:
		do_lcd_command LCD_DISP_CLR
		do_lcd_command LCD_HOME_LINE
		do_lcd_data 'A'
		do_lcd_data 'd'
		do_lcd_data 'm'
		do_lcd_data 'i'
		do_lcd_data 'n'
		do_lcd_data ' '
		do_lcd_data 'm'
		do_lcd_data 'o'
		do_lcd_data 'd'
		do_lcd_data 'e'
		do_lcd_data ' '
		mov temp1, current_item
		rcall write_digits	;prints item number stored in temp1
		do_lcd_command LCD_SEC_LINE
		call get_item
		mov temp1, tempcount
		call write_digits
		rcall show_inventory
		do_lcd_data ' '
		do_lcd_data ' '
		do_lcd_data ' '
		do_lcd_data ' '
		do_lcd_data ' '
		do_lcd_data ' '
		do_lcd_data ' '
		do_lcd_data ' '			;TODO chech how long lcd line is
		do_lcd_data '$'
		mov temp1, tempcost
		call write_digits
		jmp keypad_prologue		;check for button pushing

		show_inventory:		;Function to show inventory on leds
			clr temp2			;temp2 will be what to be shown on leds
			mov temp1, tempcount	
			cpi temp1, 9		;check to see if need to show on PORTG LEDs as well
			brlo show_inventory_C_loop
			cpi temp1, 9
			breq show_9
			ldi temp1, 8		;else must be 10 and have 8 remaining leds to show on PORTC
			ser temp2	;show both PORTG LEDs
			out PORTG, temp2	
			clr temp2
			rjmp show_inventory_C_loop
			show_9:
				ldi temp1, 8
				ldi temp2, 0b00010000	;show just one PORTG LED TODO not sure if this is right one
				out PORTG, temp2
				clr temp2
			show_inventory_C_loop:
				cpi temp1, 0
				breq show_inventory_done
				lsl temp2
				ori temp2, 0b00000001
				dec temp1
				rjmp show_inventory_C_loop

			show_inventory_done:
				out PORTC, temp2
				ret
				

		

write_digits:	
	push temp1									; writes value of temp1
	cpi temp1, 10
	brne write_ones

	do_lcd_data '1'
	subi temp1, 10

	write_ones:
		subi temp1, -'0'
		do_lcd_data_reg temp1
		pop temp1
		ret

