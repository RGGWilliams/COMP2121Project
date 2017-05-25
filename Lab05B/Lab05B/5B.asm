;
; Lab05B.asm
;
; Created: 21/05/2017 11:25:34 PM
; Author : Rhys
;


; Replace with your application code
.include "m2560def.inc"

.equ max_brightness = 0b1111111111111111
.def temp = r16
.def brightness_l = r17
.def brightness_h = r18

.macro clear
	ldi YL, low(@0)
	ldi YH, high(@0)
	clr temp
	st Y+, temp
	st Y, temp
.endmacro

.dseg
UpdateCounter: .byte 2	;Used to count how many times we've updated the brightness
TempCounter: .byte 2	;Count number of timer interrupts

.cseg
.org 0x0000
   jmp RESET
   jmp DEFAULT          ; No handling for IRQ0.
   jmp DEFAULT          ; No handling for IRQ1.
.org OVF3addr
   jmp Timer3OVF        ; Jump to the interrupt handler for
                        ; Timer3 overflow.
   jmp DEFAULT          ; default service for all other interrupts.
DEFAULT:  reti          ; no service

RESET:
	ldi temp, high(RAMEND)	;initialise stack pointer
	out SPH, temp
	ldi temp, low(RAMEND)
	out SPL, temp
	ser temp
	out DDRC, temp	;set port C as output (the LED)
	rjmp main

Timer3OVF:	;interrupt handler for Timer0
	in temp, SREG
	push temp
	push YH
	push YL
	push r25
	push r24

	lds r24, TempCounter
	lds r25, TempCounter+1
	adiw r25:r24, 1

	cpi r24, low(976)		;Check to see if one 16th of a second has passed (TODO not sure if value is correct b/c no prescaling now)
	cpi temp, high(976)		;as will update brightness that often because OCR is a 16bit register
	cpc r25, temp
	brne NotSixteenth

	clear TempCounter

	;change brightness by shifting max brightness pattern right and adding zeroes at front
	lsr brightness_h
	ror brightness_l

	;set brightness
	sts OCR3BL, brightness_l
	sts OCR3BH, brightness_h

	lds r24, UpdateCounter
	lds r25, UpdateCounter+1
	adiw r25:r24, 1

	sts UpdateCounter, r24
	sts UpdateCounter+1, r25

	ldi r19, high(16)		;check if one second has passed to reset
	cpi r24, low(16)
	cpc r25, r19
	brne EndIF
	rjmp reload_pattern		;if one second has passed reload the max brightness

	reload_pattern:
		ldi brightness_h, high(max_brightness)
		ldi brightness_l, low(max_brightness)
		clear UpdateCounter
		rjmp EndIF

	NotSixteenth:
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

main:

	ldi brightness_h, high(max_brightness)
	ldi brightness_l, low(max_brightness)

	clear TempCounter	;intitialise temp counter
	clear UpdateCounter		;intialise second counter

	ldi temp, (1<<WGM30)|(1<<COM3B1)	;Set timer 3 to phase correct PWM mode
	sts TCCR3A, temp
	ldi temp, (1 << CS30)	;no prescaling
	sts TCCR3B, temp		
	ldi temp, (1<<TOIE3)	;T/C3 interrupt enabled
	sts TIMSK3, temp	
	sei	

	;PWM set up
	ldi temp, 0b00001000
	sts DDRL, temp			;Bit 3 will function as OC3B

	;ldi temp, 0xFF			;Unsure what the difference is between OCR3A and OCR3B
	;sts OCR3AL, temp
	;clr temp
	;sts OCR5AH, temp

	;Set bit PE2 as output
	ser temp
	out DDRE, temp	;so PE2 can function as OC3B (as output)
	ldi temp, 0xFF	;the value controls the PWM duty cycle start at highest (highest brightness)
	sts OCR3BL, temp
	clr temp
	sts OCR3BH, temp

done_loop:
	rjmp done_loop
		


	