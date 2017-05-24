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
SecondCounter: .byte 2
TempCounter: .byte 2

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
	out DDRC, temp	;set port C as output
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

	cpi r24, low(7812)	;Check to see if one second has passed
	cpi temp, high(7812)
	cpc r25, temp
	brne NotSecond

	clear TempCounter

	;set brightness
	sts OCR3BL, brightness_h
	;change brightness
	lsr brightness_h
	ror brightness_l

	lds r24, SecondCounter
	lds r25, SecondCounter+1
	adiw r25:r24, 1

	sts SecondCounter, r24
	sts SecondCounter+1, r25

	ldi r19, high(16)		;check if one second has passed to reset
	cpi r24, low(16)
	cpc r25, r19
	brne EndIF
	rjmp reload_pattern

	rjmp EndIF

	reload_pattern:
		ldi brightness_h, high(max_brightness)
		ldi brightness_l, low(max_brightness)
		clear SecondCounter
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

main:

	ser temp
	out DDRC, brightness_h	;set port C for output
	ldi brightness_h, high(max_brightness)
	ldi brightness_l, low(max_brightness)

	clear TempCounter	;intitialise temp counter
	clear SecondCounter		;intialise second counter

	ldi temp, (1<<WGM30)|(1<<COM3B1)	;Set timer 3 to phase correct PWM mode
	sts TCCR3A, temp
	ldi temp, (1 << CS00)	;no prescaling
	sts TCCR3B, temp		
	ldi temp, 1<<TOIE3	
	sts TIMSK3, temp		;T/C0 interrupt enabled

	;PWM set up
	ldi temp, 0b00001000
	sts DDRL, temp			;Bit 3 will function as OC3B

	ldi temp, 0x4A			;the value controls the PWM duty cycle
	sts OCR3AL, temp
	clr temp
	sts OCR5AH, temp

	;Set bit PE2 as output
	ser temp
	out DDRE, temp	;so PE2 can function as OC3B
	ldi temp, 0xFF
	sts OCR3BL, temp
	clr temp
	sts OCR3BH, temp

	sei
		


	