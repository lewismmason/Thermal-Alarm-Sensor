$MODLP51

; Reset vector
org 0x0000
    ljmp mainprogram

; External interrupt 0 vector (not used in this code)
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR

; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	reti


CLK  EQU 22118400
BAUD equ 115200
BRG_VAL equ (0x100-(CLK/(16*BAUD)))
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))

$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$include(math32.inc) ; A library of 32bit math functions and utility macros
;$MODLP51
$LIST

TIMER0_RELOAD_H DATA 0xf4
TIMER0_RELOAD_L DATA 0xf2


; These ’EQU’ must match the wiring between the microcontroller and ADC
CE_ADC  EQU P2.0	;SS'
MY_MOSI EQU P2.1	;MOSI
MY_MISO EQU P2.2	;MISO
MY_SCLK EQU P2.3 	;CLK
USER_B  EQU P4.5	;Button to control average tempurature toggle
; These ’EQU’ must match the wiring between the microcontroller and LCD
LCD_RS equ P1.1
LCD_RW equ P1.2
LCD_E  equ P1.3
LCD_D4 equ P3.2
LCD_D5 equ P3.3
LCD_D6 equ P3.4
LCD_D7 equ P3.5	
SS equ P2.5
Sound_Out equ P3.7
	
;--------
;definitions
;--------


DSEG at 30H
buffer			: ds 30
Result			: ds 2
x			: ds 4
y			: ds 4
bcd			: ds 5	;necessary variables for math32.inc
prevtemp		: ds 1	; the previous tempurature
MMflag			: ds 1	; used for determining if the user wants to track average tempurature
xrunning		: ds 1	;the sum of all values used for calculating the average tempurature
xnumber			: ds 4	;the number of tempuratures used for the running value
risktemp_bcd		: ds 1	;the risk-temp
mintemp_bcd		: ds 1	;min tempurature
maxtemp_bcd		: ds 1	;max tempurature
risktemp_flag		: ds 1	;flag indicating if the tempurature is too high
alarmflag		: ds 1	;determines if the flag should be set
ssflag			: ds 1	;determines if the flag should be set

BSEG
mf			: dbit 1

CSEG


clearhalf	: db  '        ', 0		;used to clear the LCD
clear		: db  '               ', 0	;used to clear the LCD
Constant	: db  'Const', 0
Increasing	: db  'Inc', 0
Decreasing	: db  'Dec', 0
Warning1	: db  '-Warning-', 0
Warning2	: db  '-TOO HOT-', 0
MaxMin		: db 'Max=   Min=', 0
MaxMinOn	: db 'On', 0
MaxMinOFF	: db '  ', 0

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Set autoreload value
	mov TIMER0_RELOAD_H, #high(TIMER0_RELOAD)
	mov TIMER0_RELOAD_L, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    	setb ET0  ; Enable timer 0 interrupt
    	setb TR0  ; Start timer 0
	ret
	
;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P3.7 ;
;---------------------------------;
Timer0_ISR:
	push acc
	push PSW
	
	mov  a, alarmflag
	cjne a, #0x1, no_alarm_sound
	cpl SOUND_OUT ; Connect speaker to P3.7!
	ljmp Done_Timer0_ISR
no_alarm_sound:
	clr SOUND_OUT
Done_Timer0_ISR:
	pop PSW
	pop acc
	reti

GeString:
	mov R0, #buffer
GSLoop:
	lcall getchar
	push acc
	clr c
	subb a, #10H
	pop acc
	jc GSDone
	MOV @R0, A
	inc R0
	SJMP GSLoop
GSDone:
	clr a
	mov @R0, a
	ret
	

; Configure the serial port and baud rate
InitSerialPort:
    ; Since the reset button bounces, we need to wait a bit before
    ; sending messages, otherwise we risk displaying gibberish!
    mov R1, #222
    mov R0, #166
    djnz R0, $   ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, $-4 ; 22.51519us*222=4.998ms
    ; Now we can proceed with the configuration
	orl	PCON,#0x80
	mov	SCON,#0x52
	mov	BDRCON,#0x00
	mov	BRL,#BRG_VAL
	mov	BDRCON,#0x1E ; BDRCON=BRR|TBCK|RBCK|SPD;
    ret

INIT_SPI:
	 setb MY_MISO ; Make MISO an input pin
	 clr MY_SCLK ; For mode (0,0) SCLK is zero
	 ret

DO_SPI_G:
	 push acc
	 mov R1, #0 ; Received byte stored in R1
	 mov R2, #8 ; Loop counter (8-bits)
DO_SPI_G_LOOP:
	 mov a, R0 ; Byte to write is in R0
	 rlc a ; Carry flag has bit to write
	 mov R0, a
	 mov MY_MOSI, c
	 setb MY_SCLK ; Transmit
	 mov c, MY_MISO ; Read received bit
	 mov a, R1 ; Save received bit in R1
	 rlc a
	 mov R1, a
	 clr MY_SCLK
	 djnz R2, DO_SPI_G_LOOP
	 pop acc
	 ret

; Send a character using the serial port
putchar:
    jnb TI, putchar
    clr TI
    mov SBUF, a
    ret

getchar:
	jnb RI, getchar
	clr RI
	mov a, SBUF
	ret

; Send a constant-zero-terminated string using the serial port
SendString:
    	clr A
    	movc A, @A+DPTR
    	jz SendStringDone
    	lcall putchar
    	inc DPTR
    	sjmp SendString
SendStringDone:
    	ret
 
;----------------;
;Delay 	 
;----------------; 
;Delays for one half second
Delay: 
	push acc
	mov a, ssflag
	cjne a, #0x00, SSnot0
	Wait_Milli_Seconds(#250)
	ljmp doneDelay

SSnot0:
	
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	ljmp doneDelay
	
	
doneDelay:
	pop acc
	ret
	
;----------------;
;Get_ADC_Value 	 
;----------------;  
Get_ADC_Value:	;get the current values from SPI, store 10-bit value in Result/Result+1
	;Read_ADC_Channel(0)
	clr CE_ADC
	mov R0, #00000001B ; Start bit:1
	lcall DO_SPI_G
	mov R0, #10000000B ; Single ended, read channel 0
	lcall DO_SPI_G
	mov a, R1 ; R1 contains bits 8 and 9
	anl a, #00000011B ; We need only the two least significant bits
	mov Result+1, a ; Save result high.
	mov R0, #55H ; It doesn't matter what we transmit...
	lcall DO_SPI_G
	mov Result, R1 ; R1 contains bits 0 to 7. Save result low.
	setb CE_ADC
	lcall Execute_Result
	lcall Delay	
	ret
	
;------------------------------------;
;Display_10_digit_BCD
;------------------------------------;
Display_10_digit_BCD:
	Set_Cursor(2, 12)
	Display_BCD(bcd+4)
	Display_BCD(bcd+3)
	Display_BCD(bcd+2)
	Display_BCD(bcd+1)
	Display_BCD(bcd+0)
	ret
	
;----------------;
;Volt_to_Temp	 
;----------------;
Volt_to_Temp:	;change the voltage measurement from ADC to a tempurature using math funcs
	mov x+0, Result + 0
	mov x+1, Result + 1
	mov x+2, #0x00
	mov x+3, #0x00
	
	Load_Y(410)
	lcall mul32 
	Load_Y(1023)
	lcall div32			;x now contains T
	
	Load_Y(273)
	lcall sub32
	
	lcall Inc_Dec_Temp		;will print to the LCD if the tempurature is increasing or decreasing
	lcall Max_Min_Temp		;can print the average tempurature
	lcall Check_Temp_Hazard ;checks if the value is greater than risk-temp
	lcall hex2bcd
	ret

newline:
    DB  ' ', '\r', '\n', 0
    
;----------------;
;Max_Min_Temp	 
;----------------;
Max_Min_Temp:				;if user enables, displays the max/ min temp till disabled

	push acc
	mov a, x				;x is a hex value, turn it into a bcd value and compare with max/mintemp
	push acc						
	jb USER_B, Skip_Toggle_Max_Min_Temp  	; if the 'USER_B' button is not pressed skip
	Wait_Milli_Seconds(#50)				 ; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb USER_B, Skip_Toggle_Max_Min_Temp  	; if the 'USER_B' button is not pressed skip
	jnb USER_B, $
	;button has been pressed, toggle temp setting
	
	mov a, MMflag				;determining what the previous setting was
	cjne a, #0x00, Max_Min_Temp_Flag_One	;if a is not 0 it is 1, then branch
	mov MMflag, #0x01	;set flag 1
	set_Cursor(2,15)
	Send_Constant_String(#MaxMinOn)
	
	
	ljmp Skip_Toggle_Max_Min_Temp
	
Max_Min_Temp_Flag_One:
	mov MMflag, #0x00	;set flag 0
	mov maxtemp_bcd, #0x00
	mov mintemp_bcd, #0x44
	set_Cursor(2,15)
	Send_Constant_String(#MaxMinOff)
	
	ljmp Skip_Toggle_Max_Min_Temp
	
Skip_Toggle_Max_Min_Temp:

	mov a, #0x01
	cjne a, MMflag, Not_Max_Min_Temp
	;now we need to compare the current values and place values in variables accordingly
	lcall hex2bcd	;bcd now contains the bcd number for x
	mov a, bcd		
	
	;now compare a with max and min temps to see if the temp needs to replace one
	cjne a, maxtemp_bcd, Check_Carry_T_1
Check_Carry_T_1:
	jc Not_New_Max_Temp	;if a is less than maxtemp, don't replace so jump check min
	mov maxtemp_bcd, a	;if maxtemp_bcd now contains new maxtemp
	
	
Not_New_Max_Temp:
	cjne a, mintemp_bcd, Check_Carry_T_2
Check_Carry_T_2:
	jc New_Min_Temp	;if a is less than mintemp, new mintemp
	ljmp Display_Max_Min
	
New_Min_Temp:	
	mov mintemp_bcd, a	;if maxtemp_bcd now contains new min temp
	
Display_Max_Min:
	set_Cursor(2,1)
	Send_Constant_String(#MaxMin)
	
	set_Cursor(2,5)
	Display_BCD(maxtemp_bcd)
	set_Cursor(2,12)
	Display_BCD(mintemp_bcd)
	
Not_Max_Min_Temp:
	pop acc
	mov x, a
	pop acc	
	ret
	
;---------------------;
;Check_Temp_Hazard	 
;---------------------;
Check_Temp_Hazard:			;checks if the tempurature is greater than the risk-temp, if so, enable the alarm
	push acc
	mov a, x
	push acc
	
	mov a, x
	lcall hex2bcd
	mov a, bcd
	
	cjne a, risktemp_bcd, RiskTemp_NOT_CurrTemp	;compare risktemp and x
	set_Cursor(1,9)
	Send_Constant_String(#Clearhalf)
	set_Cursor(1,8)
	Send_Constant_String(#Warning1)
	mov risktemp_flag, #0x01		;set flag to 1 (current temp warning)
	mov alarmflag, #0x00
	ljmp Done_Temp_Hazard
	
RiskTemp_NOT_CurrTemp:	
	jc Not_Too_Hot		;if carry set, that means a is less than risktemp, jump 
	;if didnt't jump, temp is too hot, display this on screen and allow the alarm
	set_Cursor(1,9)
	Send_Constant_String(#Clearhalf)
	set_Cursor(1,8)
	Send_Constant_String(#Warning2)
	
	lcall Sound_Alarm
	ljmp Done_Temp_Hazard
	
Not_Too_Hot:
	;so check if the previous value of the flag was on, if so, clear the screen and 
	;reset the flag
	mov a, risktemp_flag
	cjne a, #0x00, Need_To_Reset_Flag	;if the previous flag was not zero, reset
	mov alarmflag, #0x00
	ljmp Done_Temp_Hazard
	
Need_To_Reset_Flag:
	mov risktemp_flag, #0x00
	set_Cursor(1,9)
	Send_Constant_String(#Clearhalf)
	mov alarmflag, #0x00
	ljmp Done_Temp_Hazard
	
Done_Temp_Hazard:	
	pop acc
	mov x, a
	pop acc
	ret

;---------------------;
;Sount_Alarm	 
;---------------------;
Sound_Alarm:			;enables the alarm flag to be used by interupts
	mov alarmflag, #0x01
	ret
	
    
;----------------;
;Execute_Result	 
;----------------;
Execute_Result:
	lcall Volt_to_Temp	;change the result into a BCD number
	
	Send_BCD(bcd)
	
	mov DPTR, #newline
	lcall sendstring

	ret
	
;----------------;
;Inc_Dec_Temp	 
;----------------;
Inc_Dec_Temp:		;prints on the LCD if the tempurature is increasing or decreasing or staying constant
	push acc
	mov a, prevtemp
	cjne a, x, New_Temp_Not_Equal 	;compare prevtemp with new value
	set_Cursor(1,1)
    Send_Constant_String(#clear)
	set_Cursor(1,1)
    Send_Constant_String(#Constant)
    
	ljmp Done_INC_Dec_Temp
	
New_Temp_Not_Equal:
	;if not equal we need to check for if x is higher or lower than previous temp
	;note: for cjne if 'a' is less than 'x' then carry bit is set
	
	jc  Temp_Flag_Set	;jump if set, ie if a is less than x (so tempurature is increasing)
	;otherwise Temp is decreasing
	set_Cursor(1,1)
    Send_Constant_String(#clear)
	set_Cursor(1,1)
    Send_Constant_String(#Decreasing)
    
	ljmp Done_INC_Dec_Temp
	
Temp_Flag_set:
	set_Cursor(1,1)
    Send_Constant_String(#clear)
	set_Cursor(1,1)
    Send_Constant_String(#Increasing)
	ljmp Done_INC_Dec_Temp
	
Done_INC_Dec_Temp:
	mov a, x
	mov prevtemp, a	; place the current value in prevtemp to be used next cycle
	pop acc
	ret

;----------------;
;main program	 
;----------------;
MainProgram:
    mov SP, #7FH ; Set the stack pointer to the begining of idata
    ;enable interupts
    lcall Timer0_Init
    mov P0M0, #0
    mov P0M1, #0
    setb EA		;enable global interupts.
    
    lcall InitSerialPort
    lcall INIT_SPI
    lcall LCD_4BIT
    set_Cursor(1,1)
    Send_Constant_String(#clear);
    set_Cursor(1,1)
    
    mov a, #0x40
    da a
    mov risktemp_bcd, a
    mov a, #0x44
    da a
    mov mintemp_bcd, a	;placing a large starting mintemp
    mov a, #0x00
    da a
	mov maxtemp_bcd, a	;placing a small starting maxtemp
	mov MMflag, #0x00

    
    
Fetch_Every_Sec:			;prints the Temp from channel 0 into PuTTY every second
	lcall Get_ADC_Value

	
	;check if the user wants to change the sampling speed
	jb SS, skipsschangetp  ; if the 'USER_B' button is not pressed skip
	Wait_Milli_Seconds(#50)				 ; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb SS, skipsschangetp  ; if the 'USER_B' button is not pressed skip
	jnb SS, $
	ljmp sschange
skipsschangetp:
	ljmp skipsschange
sschange:
	mov a, ssflag
	cjne a, #0x00, SSnot0swap
	mov ssflag, #0x01
	ljmp skipsschange
SSnot0swap:
	mov ssflag, #0x00
skipsschange:

		ljmp Fetch_Every_Sec
END