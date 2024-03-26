
;*******************************************************************************
;
;	Acc5
;	
;	This is a new version based on Acc4 so that 8 individual outputs can be 
;	controlled as two blocks of 4 at separate addresses. This allows either
;	the MERG encoder or other commercial controllers which expect outputs to
;	only work in pairs to work individual outputs. In this mode, an activate to
;	the first of a pair turns that output on. An activate to the other in a pair
;	turns the first output off. Main purpose is to allow 8 signals / lights from
;	one decoder. Also will operate 8 points via two  MERG Servo4 boards.
;
;	There are two completely separate blocks of CVs so it is effectively two 
;	decoders in one. This dual mode is set by bit 7 of CV545 (CV33)
;
;	If this bit is 0, the decoder operates as a standard Acc4.
;	Tested with MERG encoder in mode 1. 11/05/05. Seems OK
;	Not tested with a Lenz system.
;	Acc5 modified for Lenz system  30/05/05 to reverse the output pairs
;
;	Note:  Default setting is dual mode.
 
;	Accessory decoder based on original loco decoder program by D Probst 
;	to whom full acknowledgement is made.
;
;	First modified for accessory control by Mike Bolton  2nd Dec 1998
;	Revised 25/5/02 for 16F628 PIC
;
;	Program is intended to be used with the accessory decoder designs    	
;	DCCACC2B and DCCACC3. The decoder is opto isolated . No power booster is required 
;	but the signals can be taken from the track if desired. 
;	A separate 20v AC power supply is needed.
;	Decoder includes circuitry to drive solenoid
;	point motors using a capacitor discharge technique. 
;
;	
;
;	Schematic, PCB layout and source listing (+ HEX file) available. Contact
;	Mike Bolton at mbolton@fs1.with.man.ac.uk.
;
;	Details of a matching accessory encoders are also available.
;
;	
;This is code for a PIC16F628A . This program will read three, four,
;and five byte packets and supports paged, direct CV or register mode 
;access of CV data. Direct read uses bit mode. Direct write uses both
;byte write or bit manipulation.
;
;
;The decoder follows the NMRA standard for an Accessory Decoder. It controls
;four pairs of outputs with on times set by the CVs in increments of 
;10 millisecs. Range is 10 ms to 2.54 sec.or continuous.
;
;Supports 9 bit addressing. (3 byte packets) at present.

;The processor is configured to operate at 8MHz. 
;When programming,the fuses should be set with HS clock, WDT On.
;

;The following NMRA defined CV's and ranges are supported...

;NMRA  alt
;CV513 (1)  Lower 6 address bits    00h - 3Fh  
;CV514 (2) Auxiliary activation    00h - FFh  Bits 0 to 7.Each bit sets an output.
;					   1 is active, 0 is inactive   
;CV515 F1 function 'on' time   00h - FFh  Time in 10ms steps. 0 is continuous.
;CV516 F2  
;CV517 F3                                        
;CV518 F4
;CV519 Version Number     	00h -FFh   Cannot be changed (same as CV8)
;CV520 Manufacturers ID        00h -FFh   Cannot be changed (same as CV9)
;                
;CV521 Upper 3 bits of		00h -07h
;      	    address     
;CV541 Configuration		Bit 7 = 1  (same as CV29)
;CV545 Mode			Bits 0 to 3 set the toggle mode on output pairs (Lenz compatibility)
;				Bit 7 sets dual mode

; The following alternative CV's and register addresses are supported.
; NMRA CV  Alt CV   Register
; CV513    CV1      R1 (Address)
; CV514    CV2         (not available in register mode)
; CV515    CV3      R2 (F1 on time)
; CV516    CV4      R3 (F2 on time)
; CV517    CV5      R4 (F3 on time)
; CV518    CV6      R5 (F4 on time)
;                   R6 (Page register)
; CV519    CV7      R7 (version)
; CV520    CV8      R8 (manufacturer)
; CV521    CV9         (not available in register mode)
; CV541    CV29        (not available in register mode)
; CV545	   CV33		(sets toggle function for Lenz compatibility)
; 
;	No other CVs are supported.

        processor       16F628A
	include		"p16f628A.inc"
	__CONFIG	h'3F66' ; PUTimer on, WDT on, HS clock, BOD on

				; 8 Mhz Crystal

;Variable definition - I/O Port assignments

;F1a  	equ    	PORTA,0		;pin 17
;F1b	equ		PORTA,1		;pin 18   
;F2a  	equ     PORTA,2		;pin 1
;F2b  	equ    	PORTA,3		;pin 2
;F3a	equ		PORTB,0		;pin 6
;F3b  	equ     PORTB,1		;pin 7
;F4a  	equ		PORTB,2    	;pin 8
;F4b	equ     PORTB,3		;pin 9              
;ackout	equ		PORTB,6		;pin 12 (acknowledge)
;dat	equ		PORTB,7		;DCC data in














;bit definitions


primary equ    	0     	;primary address match
reset 	equ     1     	;reset packet
brdcst	equ		2		;accessory broadcast
service equ    	3
altadr	equ		4		;alternative primary address (mode 1)
value   equ    	0
byte4   equ    	1     	
byte5   equ    	2 	;not used yet


;registers

	CBLOCK	0x0020

	addcfg    	;address configuration
	confg	
	count   	;counts which output
	CV513a  	;holds CV513 low address bits
	CV521a  	;holds CV521 high address bits
	CV514a		;which outputs to activate
	CV515a		;times for fast access
	CV516a
	CV517a
	CV518a
	CV541a   	;config CV
	CV545a		;toggle CV
 				;alternate decoder CVs
	CV513_2a  	;holds CV513 low address bits
	CV521_2a  	;holds CV521 high address bits
	CV514_2a	;which outputs to activate
	CV515_2a	;times for fast access
	CV516_2a
	CV517_2a
	CV518_2a
	CV541_2a   	;config CV
	
	toggle		;register for Lenz toggle state
	mode		;for single / dual mode 
	endval  
	lastd1  
	lastd2  
	lastd3  
	lastd4  
	action		;4 bits of accessory command
	tick1		;counts 10 millisec intervals
	time1		;on time for output 1 etc.
	time2	
	time3	
	time4	
	time5	
	time6	
	time7	
	time8	

	pagereg 
	m       	;as in Dean Probst's decode
	n       
	state   
	temp    
	arg3	
	arg4	
  
	ENDC

	CBLOCK	0x0071	;for access in any page

	data1   	;first packet byte
	data2   	;second packet byte
	data3   
	data4   
	data5
   	
	ENDC

;Init sets PORTA as all outputs, pin b7 as input and the rest of PORTB as
;outputs and sets the TMR0 counter to divide by 32 (16us at 8MHz clock).
	org	0000
	nop
	goto	Init

Init  bsf	STATUS,RP0	;set to page 1
	movlw	B'00000000'	;port a outputs 
	movwf	TRISA
	movlw	B'10000000'	;port b 7 is data in
	movwf	TRISB
	movlw	B'10000100'	;set OPTION reg RTCC /32
	movwf	OPTION_REG
	bcf		STATUS,RP0
	movlw	B'00000111'	;
	movwf	CMCON		; turn comparators OFF
	clrf    PORTA
    clrf    PORTB
    clrf    INTCON
        
        
    clrf    state
    clrf    m
    clrf    n
    clrf    confg
    clrf	action
	clrf	toggle
	call	refresh		;refresh the CVs

			
	

	



;The following routine loops between Starthi and Startlo in a precisely 
;timed sequence that samples track voltage at 22us intervals, decodes the
;incoming packet, and updates the decoder outputs. When a change in 
;voltage is detected, a half-bit is defined and the subroutine 
;Getstat is called to check the value of the half-bit and decode the 
;sequence. Subsequent delays between samplings call the update subroutine 
;to update the outputs and timing registers of the microcontoller. At 22us, 
;the values of the half-bits are determined as follows:
;
;       Delays          Possible Duration
;       1               1us  - 43us
;       2               23us - 65us
;       3               45us - 87us
;       4               67us - up
;
;With these values, one delay will be 43us or less and will reset 
;the packet decode sequence. Two or three delays will be 23us to 87us and 
;will be recognized as a hi (1) half-bit. This completely encompasses the 
;duration required by the NMRA DCC standards of 52us to 64us. Four or more 
;delays will be 67us or greater and are recognized as a low (0) half-bit.
;Again, this meets the NMRA DCC requirements of 90us to 10000us. Durations of
;~18ms in length, such as DC or when the signal is lost, will cause the
;watch dog timer to reset, reseting the decoder. 

;                               timing

Starthi call    Getval          ;3
Conthi  btfss   PORTB,7       	;44/0/88
        goto    Startlo         ;45/1
        btfss   m,7             ;46
        incf    m,F             ;47        
        call    Update          ;48
        goto    Conthi          ;86

Startlo call    Getval          ;3
Contlo  btfsc   PORTB,7       	;44/0/88
        goto    Starthi         ;45/1
        btfss   m,7             ;46
        incf    m,F             ;47       
        call    Update          ;48
        goto    Contlo          ;86

;Getval checks m for a value of 0 (reset), 1 or 2 (hi) or greater (low).
;It then checks state for a value less than or equal to 24 (18h) and then
;adds state to pc register (program counter) to offset execution to a 
;specific subroutine.

Getval  incf    n,F             ;5
        movf    m,W             ;6
        btfsc   STATUS,Z	;7
        goto    Resetv          ;8
        clrwdt                  ;9
        clrf    m               ;10
        bcf     confg,value    ;11
        addlw   0xFD            ;12
        btfss   STATUS,C    	;13
        bsf     confg,value    ;14
        btfsc   STATUS,C    	;15
        clrf    n               ;16
        movlw   0xE7            ;17
        addwf   state,W         ;18
        btfsc   STATUS,C    	;19
        clrf    state           ;20
        movf    state,W         ;21
        addwf   PCL,F           ;22
                                              ;state
        goto    Waitn           ;24             ;0     
        goto    Waitlo                          ;1
        goto    Testlo                          ;2

        goto    Bitset                          ;3
        goto    Lastv                           ;4
        goto    Bitset                          ;5
        goto    Lastv                           ;6
        goto    Bitset                          ;7
        goto    Lastv                           ;8
        goto    Bitset                          ;9
        goto    Lastv                           ;10
        goto    Bitset                          ;11
        goto    Lastv                           ;12
        goto    Bitset                          ;13
        goto    Lastv                           ;14
        goto    Bitset                          ;15
        goto    Lastv                           ;16
        goto    Bitset                          ;17
        goto    Endx                            ;18

        goto    End1                            ;19
        goto    End2                            ;20
        goto    End3                            ;21
        goto    End4                            ;22
        goto    End5                            ;23
        
;This is the last state of the packet decode sequence. It first checks for
;a high half-bit. It then XORs the five data bytes to check packet parity. 
;It then checks addcfg register flags to determine instruction address 
;and branches to CVserv or Decode.

        clrf    state           ;24        state = 24
        btfss   confg,value    ;25
ret16   goto    ret14           ;26
        movf    addcfg,W        ;27
        btfsc   STATUS,Z        ;28
        goto    ret11           ;29
        movf    data1,W         ;30
        xorwf   data2,W         ;31
        xorwf   data3,W         ;32
        xorwf   data4,W         ;33
        xorwf   data5,W         ;34
        btfss   STATUS,Z        ;35
        goto    ret4            ;36
        btfsc   addcfg,service  ;37
        goto    CVserv          ;38
        clrf    n               ;39     
        bcf     confg,service	;40
        	

Decode  ;btfsc	addcfg,primary
	;nop
	movf    data2,W         ;Dcontrl	action
	andlw	0xE0		;000
	btfsc	STATUS,Z 	;reset packet
	goto	Dcontrl 
	btfss	data2,7		;accessory instruction?
	goto	noacc
	movf	data2,W		;recover byte		
        andlw   B'00001111'     ;mask
	movwf	action		;decoder instruction (four bits)
	bsf	action,7	;set new action
noacc	return  

Nextbyt	return
       

Resetv  clrf    state           ;11
        clrf    n               ;12
        goto    ret27           ;13

;Counter jumps to alternating output update routines. It is called from the
;Update routine.

Counter incf    count,F         ;55
        movlw   B'00000111'     ;56
        andwf  	count,W         ;57
        addwf   PCL,F           ;58
        goto	count1		;60
        goto    count2  
        goto    count3
        goto    count4
		goto	count5
        goto	count6
		goto	count7
		goto	count8

;CVpage is for three byte service mode addressing (page addressing). It 
;uses the page register and the offset in data1 to calculate the CV number. 
;The data in data2 is moved to data3 and the calculated CV is stored in
;data2.

; makes the distinction between register mode and page mode

CVpage  movf    pagereg,W
	iorlw	0
	btfsc	STATUS,Z	;Is the page register 0.
	goto	Regmode
                
	movf    data2,W		; Page mode
        movwf   data3
        movlw   0x07
        andwf   data1,W
        addwf   PCL,F

        goto    CVfind		;RRR = 0 , Data Register 0
        goto    CVfind		;RRR = 1 , Data Register 1
        goto    CVfind		;RRR = 2 , Data Register 2
        goto    CVfind		;RRR = 3 , Data Register 3
        goto    CV541s		;RRR = 4 , Basic configuration register
        goto    CVpgreg		;RRR = 5 , Paging register
        return			;RRR = 6 , Not used
        return			;RRR = 7 , Not used                 
        
Regmode	movf    data2,W		; Register mode
        movwf   data3
        movlw   0x07
        andwf   data1,W
        addwf   PCL,F

        goto    Creg1s		;RRR = 0 , Config Register 1
        goto    Creg2s		;RRR = 1 , Config Register 2
        goto    Creg3s		;RRR = 2 , Config Register 3
        goto    Creg4s		;RRR = 3 , Config Register 4
        goto    Creg5s		;RRR = 4 , Config Register 5
        goto    CVpgreg		;RRR = 5 , Paging register
        goto	Creg7s		;RRR = 6 , Config Register 6
        goto	Creg8s		;RRR = 7 , Config Register 7                 
        
;

;Waitn waits for 20 hi half-bits (ten hi bits of the pre-amble) to start
;the decode sequence. 

Waitn   movlw   0xEC            ;26
        addwf   n,W             ;27
        btfss   STATUS,C    	;28
        goto    ret11           ;29
        incf    state,F         ;30
        clrf    data3           ;31
        clrf    data4           ;32
        clrf    addcfg          ;33
        clrf    endval          ;34
		nop           			;35     
        nop				        ;36
		nop						;37
        nop				        ;38
		nop						;39
		nop						;40
        nop						;41
       	nop						;42
		return     				;44
        

;Waitlo waits for a low bit to signal the end of the pre-amble. 

Waitlo  btfss   confg,value    ;26
        incf    state,F         ;27
        nop			            ;28  
		nop						;29
        nop			            ;30
        nop				        ;31
        nop				        ;32
		nop						;33
        nop						;34     
        nop					    ;35     
        goto	ret4			;35
        

;Testlo must have a low half-bit. 

Testlo  incf    state,F         ;26
        btfsc   confg,value    ;27
        clrf    state           ;28
        nop            			;29	
		nop              		;30
        nop           			;31                  
        nop       				;32
        nop        				;33
		nop						;34
        nop          			;35	
       	nop						;36
        nop        				;37
       	nop        				;38
        nop        				;39	
       	nop						;40     
        nop     				;41                  
        nop    					;42
        
        return                  ;43
        
;Bitset reads the value of the just finished half-bit and rotates it
;into the LSB of data5. Eight cycles through Bitset will read a 
;complete byte of data. 
        
Bitset  incf    state,F         ;26
        bcf     STATUS,C    	;27
        btfsc   confg,value    ;28
        bsf     STATUS,C    	;29
        rlf     data5,F         ;30
        goto	ret9			;31
ret2    goto    ret0            

;Lastv checks for the current half-bit to be the same as the previous
;half-bit. 

Lastv   incf    state,F         ;26
        btfss   confg,value    ;27
        goto    lastv1          ;28
        btfss   data5,0         ;29
        clrf    state           ;30
		goto	ret9
lastv1  btfsc   data5,0         ;30
        clrf    state           ;31
ret10	goto	ret8		;32
       

;Endx compares the present half-bit to the previous half-bit (same as
;lastv) and then determines the offset for the intermediate bits.

Endx    btfss   confg,value    ;26
        goto    endx1           ;27
        btfss   data5,0         ;28
        comf    state,F         ;29           
        goto    endx2           ;30
endx1   btfsc   data5,0         ;29
        comf    state,F         ;30
        nop                     ;31
endx2   incf    endval,F        ;32
        movf    endval,W        ;33
        addwf   state,F         ;34
        goto    ret5            ;35

;Intermediate bit after first byte - must be low. End1 also checks for
;broadcast and service mode address.

End1    movlw   0x02            ;26
        movwf   state           ;27
        movf    data5,W         ;28
        movwf   data1           ;29
        btfsc   confg,value    ;30
        clrf    state           ;31
        movf    data1,W         ;32	get first byte
        btfsc	STATUS,Z     	;33
        bsf	addcfg,reset   		;34	yes so set bit 
        xorlw	B'10111111'     ;35	accessory broadcast low bits?
        btfsc	STATUS,Z     	;36
        bsf	addcfg,brdcst   	;37	set broadcast bit
        movf	data1,W         ;38 
        andlw	B'01111111'		;39	;acc address has top bit set
        addlw	0x90            ;40
        btfsc   STATUS,C		;41
        bsf     addcfg,service  ;42	service so set bit
       	return          		;43

;Intermediate bit after second byte - must be low. End2 also checks for
;primary address (9 bits)and rest of broadcast.

End2    movlw   0x02            ;26
        movwf   state           ;27
        movf    data5,W         ;28
        movwf   data2           ;29
        btfsc   confg,value    ;30
        clrf    state           ;31
        movf    data2,W         ;32	2nd byte
        andlw	B'01110000'     ;33	mask except hi address bits
	btfss   STATUS,Z     	;34	all zeros?
        bcf     addcfg,brdcst   ;35	not broadcast	
        xorwf	CV521a,W	;36	hi bits match?
        btfsc	STATUS,Z     	;37
        bsf	addcfg,primary  ;38	yes
        movf	data1,W         ;39	get first byte
        xorwf   CV513a,W        ;40	match?
        btfss   STATUS,Z     	;41
     
        bcf     addcfg,primary  ;42	no
        return                  ;43

;Intermediate bit after third byte - hi signals end of packet.

End3    btfsc   confg,value    ;26
        goto    end3hi          ;27
        movlw   0x02            ;28
        movwf   state           ;29
        movf    data5,W         ;30
        movwf   data3           ;31
        goto    ret8            ;32

end3hi  movlw   0x18            ;29 check for alternate address
        movwf   state           ;30
        bcf     confg,byte4    ;31
        bcf     confg,byte5    ;32
		movf    data2,W         ;33	2nd byte
        andlw	B'01110000'     ;34	mask except hi address bits
		xorwf	CV521_2a,W		;35	hi bits match?
        btfsc	STATUS,Z     	;36
        bsf		addcfg,altadr  	;37	yes
        movf	data1,W         ;38	get first byte
        xorwf   CV513_2a,W      ;39	match?
        btfss   STATUS,Z     	;40
     	bcf     addcfg,altadr  	;41	no
		nop						;42
		return					;43

;Intermediate bit after fourth byte - hi signals end of packet.

End4    btfsc   confg,value    ;26
        goto    end4hi          ;27
        movlw   0x02            ;28
        movwf   state           ;29
        movf    data5,W         ;30
        movwf   data4           ;31
        goto    ret8            ;32

end4hi  movlw   0x18            ;29
        movwf   state           ;30
        bsf     confg,byte4    ;31
        bcf     confg,byte5    ;32
        goto    ret7            ;33

;End bit after fifth byte - must be hi.

End5    incf    state,F         ;26
        btfss   confg,value    ;27
        clrf    state           ;28
        bsf     confg,byte4    ;29
        bsf     confg,byte5    ;30
        goto    ret9            ;31

;This subroutine is used to update the outputs of the microcontroller. 
;The TMR0 is compared with the value 63. As TMR0 increments every 16
;microsecs, count will match at about 1 millisec.A match condition (=>)
;will increment tick1 and reset the TMR0. Tick1 is compared with 10 and
;if => then all channel timers, (time1 to time8) are incremented.This
;sets the fundamental time interval at 10 millisec.
;Between tick1 updates, routine switches to 'Counter' which accesses each
;output routine in turn to see if the outputs should be on or off.

Update  movlw   .63             ;50		one millisec approx
        subwf   TMR0,W          ;51
        btfss   STATUS,C    	;52
        goto    Counter         ;53
        incf	tick1,F         ;54
        clrf	TMR0 		;55		reset TMR0
        nop           		;56
        movlw   .10         	;57
        subwf	tick1,W 	;58             
        btfss	STATUS,C 	;59             10ms
ret22   goto	ret20		;60
        clrf	tick1      	;61             
        incf    time1,F       	;62 		increment channel timers           
        incf    time2,F    	;63             
        incf    time3,F       	;64             
        incf    time4,F       	;65
        incf    time5,F       	;66
        incf    time6,F       	;67
        incf    time7,F       	;68
        incf    time8,F       	;69                     
        goto	ret12       	;70               
                   
        		       
                       


;Count1 controls output1 (or op5 in alt mode)

count1 	btfss   action,7        ;62	new action
        goto	t1a             ;63	no
        movf	action,W      	;64
        andlw	B'00000111'     ;65	mask
        xorlw	B'00000000'     ;66	is it output 0?
        btfss   STATUS,Z     	;67	yes
        goto    t1b          	;68 	no
        btfsc	mode,0			;69
		goto	alt1			;70
		btfsc	CV545a,0		;71	Lenz mode?
		goto	lenz1			;72
	
on1        btfss   CV514a,0       	;73	is this bit enabled?
        goto    t1c             ;74	no
        btfss	action,3        ;75	on or off
        goto	off1            ;76
        bsf	PORTA,0        		;77	F1a on
		clrf	tick1			;78
		clrf	time1      		;79  start timer   
        bcf	action,7      		;80	action done
        goto	ret1			;81
off1	bcf	PORTA,0				;76	F1a off
		bcf	action,7       		;77
		nop						;78
		goto	ret5			;79
t1a	nop			;65
	nop			;66
	nop			;67
	nop			;68
	nop			;69
t1b	nop			;70
	nop			;71
	nop			;72	
	nop			;73
t1c	nop			;74
	nop			;75
t1f	nop			;76
	nop			;77
t1e	movf	CV515a,W	;78 	get CV515 time on
	btfsc	STATUS,Z		;79	if zero then always on
	goto	ret2		;80
	subwf	time1,W		;81	compare time
	btfsc	STATUS,C	;82
	bcf		PORTA,0		;83	time out so put off 
	return              ;84 

lenz1	btfsc	toggle,0	;74 	is it first time?
	goto	t1d		;75     no so just check time
	bsf	toggle,0	;76     yes so set toggle
	bcf	toggle,1	;77     clear other toggle
	bcf	PORTA,1		;78     clear other side
	bsf	PORTA,0		;79     set new side
	clrf	tick1		;80
	clrf	time1      	;81  	start timer   
    bcf		action,7      	;82	action done 
	goto	ret1		;83

t1d	bcf	action,7	;75  	clear action anyway
	goto	t1e		;76

alt1	btfss	action,3		;72
	goto	t1f		;73	do nothing	
	
	btfss	addcfg,primary		;74
	goto	alt1a			;75
	
	bcf	PORTA,0				;76  set op1 off
		bcf	action,7			;77
		goto	ret6			;78
 


alt1a	bcf	PORTB,0			;77  set 0P5 off
		bcf	action,7			;78 
		goto	ret5			;79	


ret13	goto	ret11
ret11	goto	ret9
ret9	goto	ret7
ret5    goto	ret3
ret3	goto	ret1	
                        
ret1    nop                     ;83
ret0    return                  ;84



;Count2 controls output 2  (Op1 or op5 in alt mode) 

count2 	btfss   action,7        ;62	new action
        goto	t2a             ;63	no
        movf	action,W      	;64
        andlw	B'00000111'     ;65	mask
        xorlw	B'00000001'     ;66	is it output 1?
        btfss   STATUS,Z     	;67	yes
        goto    t2b          	;68 
        btfsc	mode,0			;69
		goto	alt2			;70
		btfsc	CV545a,0		;71	Lenz mode?
		goto	lenz2			;72
	
        btfss   CV514a,1       	;73	is this bit enabled?
        goto    t2c             ;74	no
        btfss	action,3        ;75	on or off
        goto	off2            ;76
        bsf	PORTA,1        	;77	F1b on
	clrf	tick1		;78
	clrf	time2      	;79  	start timer   
        bcf	action,7      	;80	action done
        goto	ret3		;81
off2	bcf	PORTA,1		;78	F1b off
	bcf	action,7       	;80
	goto	ret3
t2a	nop			;65
	nop			;66
	nop			;67
	nop			;68
	nop			;69
t2b	nop			;70
	nop			;71
	nop			;72	
	nop			;73
t2c	nop			;74
	nop			;75
t2f	nop			;76
	nop			;77
t2e	movf	CV515a,W	;78 	get CV515 time on
	btfsc	STATUS,Z	;79	if zero then always on
	goto	ret3		;80
	subwf	time2,W		;81	compare time
	btfsc	STATUS,C	;82
	bcf	PORTA,1		;83	time out so put off 
	return                  ;84 

lenz2	btfsc	toggle,1	;72 	is it first time?
	goto	t2d		;73     no so just check time
	bsf	toggle,1	;74     yes so set toggle
	bcf	toggle,0	;75     clear other toggle
	bcf	PORTA,0		;76     clear other side
	bsf	PORTA,1		;77     set new side
	clrf	tick1		;78
	clrf	time2      	;79  	start timer   
        bcf	action,7      	;80	action done 
	goto	ret1		;81

t2d	bcf	action,7	;75  	clear action anyway
	goto	t2e		;76

alt2	btfss	action,3		;72	activate?
		goto	t2f				;73 ignore
		btfss	addcfg,primary	;74
		goto	alt2a			;75


       	bsf	PORTA,0        		;76	F1a on
		clrf	tick1			;77
		clrf	time1      		;78 start timer   
        bcf	action,7      		;79	action done
		goto	ret4			;80

	
alt2a	bsf	PORTB,0		;77	 set 0P5 on
	clrf	tick1		;78
	clrf	time5		;79
	bcf	action,7		;80
	goto	ret3		;81

	
		
			
	


 


;Count3 controls output 3  (op2 or op6 in alt mode

count3 	btfss   action,7        ;62	new action
        goto	t3a             ;63	no
        movf	action,W      	;64
        andlw	B'00000111'     ;65	mask
        xorlw	B'00000010'     ;66	is it output 2?
        btfss   STATUS,Z     	;67	yes
        goto    t3b          	;68 
        btfsc	mode,0			;69
		goto	alt3			;70
	btfsc	CV545a,1	;71	Lenz mode?
	goto	lenz3		;72
	
        btfss   CV514a,1       	;71	is this bit enabled?
        goto    t3c             ;72	no
        btfss	action,3        ;73	on or off
        goto	off3            ;74
        bsf	PORTA,2        	;75	F2a on
	clrf	tick1		;76
	clrf	time3      	;77  	start timer   
        bcf	action,7      	;78	action done
        goto	ret1		;79
off3	bcf	PORTA,2		;76	F2a off
	bcf	action,7       	;77
	nop			;78
	goto	ret5		;79
t3a	nop			;65
	nop			;66
	nop			;67
	nop			;68
	nop			;69
t3b	nop			;70
	nop			;71
	nop			;72	
	nop			;73
t3c	nop			;74
	nop			;75
t3f	nop			;76
	nop			;77
t3e	movf	CV516a,W	;78 	get CV516 time on
	btfsc	STATUS,Z	;79	if zero then always on
	goto	ret3		;80
	subwf	time3,W		;81	compare time
	btfsc	STATUS,C	;82
	bcf	PORTA,2		;83	time out so put off 
	return                  ;84 

lenz3	btfsc	toggle,2	;74 	is it first time?
	goto	t3d		;75     no so just check time
	bsf	toggle,2	;76     yes so set toggle
	bcf	toggle,3	;77     clear other toggle
	bcf	PORTA,3		;78     clear other side
	bsf	PORTA,2		;79     set new side
	clrf	tick1		;80
	clrf	time3      	;81  	start timer   
    bcf	action,7      	;82	action done 
	goto	ret1		;83

t3d	bcf	action,7	;75  	clear action anyway
	goto	t3e		;76

alt3	btfss	action,3		;72
	goto	t3f		;73	do nothing	
	
	btfss	addcfg,primary		;74
	goto	alt3a			;75
	bcf	PORTA,1			;76  set op2 off
	bcf	action,7		;77
	goto	ret6			;78
      

alt3a	bcf	PORTB,1			;77  set 0P6 off
		bcf	action,7			;78 
		goto	ret5			;79







;Count4 controls F2b  (Op2 or Op 6 mode 1)

count4 	btfss   action,7        ;62	new action
        goto	t4a             ;63	no
        movf	action,W      	;64
        andlw	B'00000111'     ;65	mask
        xorlw	B'00000011'     ;66	is it output 3?
        btfss   STATUS,Z     	;67	yes
        goto    t4b          	;68 
        btfsc	mode,0			;69
		goto	alt4			;70
	btfsc	CV545a,1	;71	Lenz mode?
	goto	lenz4		;72

        btfss   CV514a,3       	;73	is this bit enabled?
        goto    t4c             ;74	no
        btfss	action,3        ;75	on or off
        goto	off4            ;76
        bsf	PORTA,3        	;77	F2b on
	clrf	tick1		;78
	clrf	time4      	;79  	start timer   
        bcf	action,7      	;80	action done
        goto	ret3		;81
off4	bcf	PORTA,3		;78	F2b off
	bcf	action,7       	;79
	nop			;80
	goto	ret3		;81
t4a	nop			;65
	nop			;66
	nop			;67
	nop			;68
	nop			;69
t4b	nop			;70
	nop			;71
	nop			;72	
	nop			;73
t4c	nop			;74
	nop			;75
t4f	nop			;76
	nop			;77
t4e	movf	CV516a,W	;78 	get CV516 time on
	btfsc	STATUS,Z	;79	if zero then always on
	goto	ret3		;80
	subwf	time4,W		;81	compare time
	btfsc	STATUS,C	;82
	bcf	PORTA,3		;83	time out so put off 
	return                  ;84 

lenz4	btfsc	toggle,3	;72 	is it first time?
	goto	t4d		;73     no so just check time
	bsf	toggle,3	;74     yes so set toggle
	bcf	toggle,2	;75     clear other toggle
	bcf	PORTA,2		;76     clear other side
	bsf	PORTA,3		;77     set new side
	clrf	tick1		;78
	clrf	time4      	;79  	start timer   
        bcf	action,7      	;80	action done 
	goto	ret3		;81

t4d	bcf	action,7	;75  	clear action anyway
	goto	t4e		;76


alt4	btfss	action,3		;72	activate?
		goto	t4f			;73 ignore
		btfss	addcfg,primary	;74
		goto	alt4a			;75

		bsf	PORTA,1       		;76	OP2 on
		clrf	tick1		;77
		clrf	time2      		;78 start timer   
        bcf	action,7      		;79	action done
		goto	ret4				;80

	
alt4a	bsf	PORTB,1		;77	 set 0P6 on
	clrf	tick1		;78
	clrf	time6		;79
	bcf	action,7		;80
	goto	ret3		;81

		





;Count5 controls output 5 (Op 3 or Op7 mode 1)

count5 	btfss   action,7        ;62	new action
        goto	t5a             ;63	no
        movf	action,W      	;64
        andlw	B'00000111'     ;65	mask
        xorlw	B'00000100'     ;66	is it output 4?
        btfss   STATUS,Z     	;67	yes
        goto    t5b          	;68 
        btfsc	mode,0			;69
		goto	alt5			;70
	btfsc	CV545a,2	;71	Lenz mode?
	goto	lenz5		;72
	
        btfss   CV514a,4       	;73	is this bit enabled?
        goto    t5c             ;74	no
        btfss	action,3        ;75	on or off
        goto	off5            ;76
        bsf	PORTB,0        	;77	F3a on
	clrf	tick1		;78
	clrf	time5      	;79  	start timer   
        bcf	action,7      	;80	action done
        goto	ret3		;81
off5	bcf	PORTB,0		;76	F3a off
	bcf	action,7       	;77
	nop			;78
	goto	ret5		;79
t5a	nop			;65
	nop			;66
	nop			;67
	nop			;68
	nop			;69
t5b	nop			;70
	nop			;71
	nop			;72	
	nop			;73
t5c	nop			;74
	nop			;75
t5f	nop			;76
	nop			;77
t5e	movf	CV517a,W	;78 	get CV517 time on
	btfsc	STATUS,Z	;79	if zero then always on
	goto	ret3		;80
	subwf	time5,W		;81	compare time
	btfsc	STATUS,C	;82
	bcf	PORTB,0		;83	time out so put off 
	return                  ;84 

lenz5	btfsc	toggle,4	;74	is it first time?
	goto	t5d		;75     no so just check time
	bsf	toggle,4	;76     yes so set toggle
	bcf	toggle,5	;77     clear other toggle
	bcf	PORTB,1		;78     clear other side
	bsf	PORTB,0		;79     set new side
	clrf	tick1		;80
	clrf	time5      	;81  	start timer   
        bcf	action,7      	;82	action done 
	goto	ret1		;83

t5d	bcf	action,7	;75  	clear action anyway
	goto	t5e		;76

alt5	btfss	action,3		;72
	goto	t5f		;73	do nothing	
	
	btfss	addcfg,primary		;74
	goto	alt5a			;75
	bcf	PORTA,2			;76  set op3 off
	bcf	action,7		;77
	goto	ret6			;78
      

alt5a	bcf	PORTB,2			;77  set 0P7 off
		bcf	action,7			;78 
		goto	ret5			;79



;Count6	controls the output 6 (Op 3 or Op7 mode 1) 

count6 	btfss   action,7        ;62	new action
        goto	t6a             ;63	no
        movf	action,W      	;64
        andlw	B'00000111'     ;65	mask
        xorlw	B'00000101'     ;66	is it output 5?
        btfss   STATUS,Z     	;67	yes
        goto    t6b          	;68 
        btfsc	mode,0			;69
		goto	alt6			;70
	btfsc	CV545a,2	;71	Lenz mode?
	goto	lenz6		;72
	
        btfss   CV514a,5       	;73	is this bit enabled?
        goto    t6c             ;74	no
        btfss	action,3        ;75	on or off
        goto	off6            ;76
        bsf	PORTB,1        	;77	F3b on
	clrf	tick1		;78
	clrf	time6      	;79  	start timer   
        bcf	action,7      	;80	action done
        goto	ret3		;81
off6	bcf	PORTB,1		;76	F3b off
	bcf	action,7       	;77
	nop			;78
	goto	ret5		;79
t6a	nop			;65
	nop			;66
	nop			;67
	nop			;68
	nop			;69
t6b	nop			;70
	nop			;71
	nop			;72	
	nop			;73
t6c	nop			;74
	nop			;75
t6f	nop			;76
	nop			;77
t6e	movf	CV517a,W	;78 	get CV517 time on
	btfsc	STATUS,Z	;79	if zero then always on
	goto	ret3		;80
	subwf	time6,W		;81	compare time
	btfsc	STATUS,C	;82
	bcf		PORTB,1		;83	time out so put off 
	return                  ;84 

lenz6	btfsc	toggle,5	;72 	is it first time?
	goto	t6d		;73     no so just check time
	bsf	toggle,5	;74     yes so set toggle
	bcf	toggle,4	;75     clear other toggle
	bcf	PORTB,0		;76     clear other side
	bsf	PORTB,1		;77     set new side
	clrf	tick1		;78
	clrf	time6      	;79  	start timer   
        bcf	action,7      	;80	action done 
	goto	ret1		;81

t6d	bcf	action,7	;75  	clear action anyway
	goto	t6e		;76

alt6	btfss	action,3		;72	activate?
		goto	t6f				;73 ignore
		btfss	addcfg,primary	;74
		goto	alt6a			;75

		bsf	PORTA,2       		;76	OP3 on
		clrf	tick1			;77
		clrf	time3      		;78 start timer   
        bcf	action,7      		;79	action done
		goto	ret4			;80
	
alt6a	bsf	PORTB,2			;77	; set 0P6 on
		clrf	tick1		;78
		clrf	time7		;79
		bcf	action,7		;80
		goto	ret3		;81

	




;Count7	controls output 7 (Op 4 and Op8 mode 1) 

count7 	btfss   action,7        ;62	new action
        goto	t7a             ;63	no
        movf	action,W      	;64
        andlw	B'00000111'     ;65	mask
        xorlw	B'00000110'     ;66	is it output 6?
        btfss   STATUS,Z     	;67	yes
        goto    t7b          	;68 
        btfsc	mode,0			;69
		goto	alt7			;70
	btfsc	CV545a,3	;71	Lenz mode?
	goto	lenz7		;72
	
        btfss   CV514a,6       	;73	is this bit enabled?
        goto    t7c             ;74	no
        btfss	action,3        ;75	on or off
        goto	off7            ;76
        bsf	PORTB,2        	;77	F4a on
	clrf	tick1		;78
	clrf	time7      	;78  	start timer   
        bcf	action,7      	;80	action done
        goto	ret3		;81
off7	bcf	PORTB,2		;78	F4a off
	bcf	action,7       	;78
	nop			;80
	goto	ret3		;81
t7a	nop			;65
	nop			;66
	nop			;67
	nop			;68
	nop			;69
t7b	nop			;70
	nop			;71
	nop			;72	
	nop			;73
t7c	nop			;74
	nop			;75
t7f	nop			;76
	nop			;77
t7e	movf	CV518a,W	;78 	get CV518 time on
	btfsc	STATUS,Z	;79	if zero then always on
	goto	ret3		;80
	subwf	time7,W		;81	compare time
	btfsc	STATUS,C	;82
	bcf	PORTB,2		;83	time out so put off 
	return                  ;84 

lenz7	btfsc	toggle,6	;74 	is it first time?
	goto	t7d		;75     no so just check time
	bsf	toggle,6	;76     yes so set toggle
	bcf	toggle,7	;77     clear other toggle
	bcf	PORTB,3		;78     clear other side
	bsf	PORTB,2		;79     set new side
	clrf	tick1		;80
	clrf	time7      	;81  	start timer   
        bcf	action,7      	;82	action done 
	goto	ret1		;83

t7d	bcf	action,7	;75  	clear action anyway
	goto	t7e		;76

alt7	btfss	action,3		;72
	goto	t7f		;73	do nothing	
	
	btfss	addcfg,primary		;74
	goto	alt7a			;75
	bcf	PORTA,3			;76  set op4 off
	bcf	action,7		;77
	goto	ret6			;78
        

alt7a	bcf	PORTB,3			;77  set 0P8 off
		bcf	action,7			;78 
		goto	ret5			;79




;Count8	controls output 8 (Op 4 or Op8 in mode 1) 

count8 	btfss   action,7        ;62	new action
        goto	t8a             ;63	no
        movf	action,W      	;64
        andlw	B'00000111'     ;65	mask
        xorlw	B'00000111'     ;66	is it output 7?
        btfss   STATUS,Z     	;67	yes
        goto    t8b          	;68 
        btfsc	mode,0			;69
		goto	alt8			;70
	btfsc	CV545a,3	;71	Lenz mode?
	goto	lenz8		;72
	
        btfss   CV514a,7       	;73	is this bit enabled?
        goto    t8c             ;74	no
        btfss	action,3        ;75	on or off
        goto	off8            ;76
        bsf	PORTB,3        	;77	F4b on
	clrf	tick1		;78
	clrf	time8      	;79  	start timer   
        bcf	action,7      	;80	action done
        goto	ret3		;81
off8	bcf	PORTB,3		;78	F4b off
	bcf	action,7       	;79
	nop			;80
	goto	ret3		;81
t8a	nop			;65
	nop			;66
	nop			;67
	nop			;68
	nop			;69
t8b	nop			;70
	nop			;71
	nop			;72	
	nop			;73
t8c	nop			;74
	nop			;75
t8f	nop			;76
	nop			;77
t8e	movf	CV518a,W	;78 	get CV518 time on
	btfsc	STATUS,Z	;79	if zero then always on
	goto	ret3		;80
	subwf	time8,W		;81	compare time
	btfsc	STATUS,C	;82
	bcf		PORTB,3		;83	time out so put off 
	return                  ;84 

lenz8	btfsc	toggle,7	;72 	is it first time?
	goto	t8d		;73     no so just check time
	bsf	toggle,7	;74     yes so set toggle
	bcf	toggle,6	;75     clear other toggle
	bcf	PORTB,2		;76     clear other side
	bsf	PORTB,3		;77     set new side
	clrf	tick1		;78
	clrf	time8      	;79  	start timer   
        bcf	action,7      	;80	action done 
	goto	ret3		;81

t8d	bcf	action,7	;75  	clear action anyway
	goto	t8e		;76


alt8	btfss	action,3		;72	activate?
		goto	t8f			;73 ignore
		btfss	addcfg,primary	;74
		goto	alt8a			;75
		bsf	PORTA,3       		;76	OP4 on
		clrf	tick1		;77
		clrf	time4      		;78 start timer   
        bcf	action,7      		;79	action done
		goto	ret4			;80

		
alt8a	bsf	PORTB,3		;77	 set 0P8 on
	clrf	tick1		;78
	clrf	time8		;79
	bcf	action,7		;80
	goto	ret3		;81


	




    
ret7    goto    ret5            ;77        
ret6    goto    ret4            ;78

;Additional goto sequences.

ret27   goto    ret25           ;15 
ret25   goto    ret23           ;17
ret24   goto    ret22  
ret23   goto    ret21           ;19
			        ;20 62
ret21   goto    ret19           ;21 63
ret20   goto    ret18           ;22 64
ret19   goto    ret17           ;23 65
ret18   goto    ret16           ;24 66
ret17   goto    ret15           ;25 67
ret15   goto    ret13           ;27 69
ret14   goto    ret12           ;28 70
ret12   goto    ret10           ;30 72
ret8    goto    ret6            ;34 76
ret4	goto	ret2


;CVserv checks for service mode instructions with the decoder in service
;mode. It then checks values of data1, data2, data3 and data4 with previous
;values to ensure a duplicate packet has been sent. It then checks for
;three or four byte packet to determine paged or direct CV access. 

CVserv  btfss   confg,byte5    ;40
        btfss   confg,service  ;41
        return                  ;42
        clrf    n
        nop
        call    Twonrow
        iorlw   0x00
        btfsc   STATUS,Z
        return
        btfss   confg,byte4
        goto    CVpage
        movf    data1,W
	andlw	B'11111100'	;Mask away high address bits
        xorlw   B'01110100'	;Direct mode Verfiy byte instruction
        btfsc   STATUS,Z
        goto    Ver1
        xorlw   B'00001000'	;Direct mode Write byte
        btfsc   STATUS,Z
	goto	Wr1
	xorlw	B'00000100'	;Direct mode Verify bit
	btfsc	STATUS,Z
	goto	Verbit
        return
        
;Wr1 writes the data in data3 into the CV listed in data2. If the CV
;number in data2 is 06h (CV519), 07h (CV520)or 1Ch (CV541), then the write
;does not take place.

     

Wr1     movf	data2,W    	     ;addresses start at 0       
        xorlw   0x06           	     ;  CV519   CV520   	  CV541
        btfsc   STATUS,Z             ;00000110 00000111 	00011100
        return                       ;00000110 00000110 	00000110
        xorlw   0x01                 ;00000000 00000001 	00011010
        btfsc   STATUS,Z             ;         00000001 	00000001
        return                       ;         00000000 	00011011
        xorlw   0x1B                 ;                  	00011011
        btfsc   STATUS,Z             ;                  	00000000
        ;goto    wrcon1 
	return
	movlw	0x20    	;allow CV545  (CV33)
	subwf	data2,W
	btfsc	STATUS,Z
	
	goto	wrcon1a
wrcont  movlw	09h			;only CVs 513(1) to 521(9)
	subwf	data2,W
	btfsc	STATUS,C
	return
	
wrcon1	movf    data2,W
	btfsc	mode,0			;which mode?
	addlw	0x21			;offset for mode 1
	goto	wrcon1b
wrcon1a	movf	data2,W
wrcon1b	bsf     STATUS,RP0
    movwf   EEADR
	movf    data3,W
    movwf   EEDATA
    clrwdt
    bsf     EECON1,WREN
    call    EEwrite
    goto    Ackn

;Two in a row checks that the current packet is the second identical packet
;before performing a service mode instruction or Advanced programming
;function.

Twonrow movf    data1,W        
        xorwf   lastd1,W
        btfss   STATUS,Z
        goto    Lastpak
        movf    data2,W
        xorwf   lastd2,W
        btfss   STATUS,Z
        goto    Lastpak
        movf    data3,W
        xorwf   lastd3,W
        btfss   STATUS,Z
        goto    Lastpak
        movf    data4,W
        xorwf   lastd4,W
        btfsc   STATUS,Z
        retlw   0xFF

;Lastpak saves the packet byte values to check for a repeated packet,
;necessary to execute a service mode instruction.

Lastpak movf    data1,W
        movwf   lastd1
        movf    data2,W
        movwf   lastd2
        movf    data3,W
        movwf   lastd3
        movf    data4,W
        movwf   lastd4
        retlw   0x00                 

; Translate register numbers into EEprom indexes to acces configuration variables

Creg1s  movlw   0x00
        movwf   data2
        goto    CVcont

Creg2s  movlw   0x02
        movwf   data2
        goto    CVcont

Creg3s  movlw   0x03
        movwf   data2
        goto    CVcont

Creg4s  movlw   0x04
        movwf   data2
        goto    CVcont

Creg5s  movlw   0x05
        movwf   data2
        goto    CVcont

; 6 is page reg.

Creg7s  movlw   0x06
        movwf   data2
        goto    CVcont

Creg8s  movlw   0x07
        movwf   data2
        goto    CVcont

;CV541s sets data2 to 1Ch (28).    

CV541s  movlw   0x1C
		btfsc	mode,0
		addlw	0x21
        movwf   data2
        goto    CVcont

CV545s	movlw	0x20
	movwf	data2
	goto	CVcont

;CVfind determines the CV using the page register and the offset in the
;instruction. 
;will accept either the NMRA RP pages (>128) or lower pages to allow 
;programming with Lenz systems.

CVfind  decf    pagereg,W
        movwf   data2
        bcf     STATUS,C
        rlf     data2,F
;        btfss   STATUS,C		;page must be 80h or more
;        return
	bcf     STATUS,C
        rlf     data2,F
        movlw   0x03
        andwf   data1,W
        addwf   data2,F
	
CVcont  btfss   data1,3
        goto    Ver1
        goto    Wr1

;CVpgreg verifies or writes the page register with data3 (data2).

CVpgreg btfss   data1,3
        goto    Pagever
        movf    data3,W
        movwf   pagereg
        return

Pagever movf    pagereg,W                        
        xorwf   data3,W                            
        btfss   STATUS,Z
        return
        goto    Ackn
        
;CVverfy XORs the EE value at data2 with the value in data3.
;only CV 513 to 521, 541 and 545 will verify. 

Ver1    call	CVchk
	btfsc	STATUS,C
	return
ver2	movlw	0x20
		xorwf	data2,W
		btfss	STATUS,Z
		goto	ver2a
		movf	data2,W
		goto	ver2b
ver2a	movf    data2,W 	;CVs address directly
		btfsc	mode,0
		addlw	0x21 
ver2b	call	EEread
        xorwf   data3,W
        btfss   STATUS,Z
        return
        goto    Ackn

CVchk	movlw	1Ch		;is it CV541
	xorwf	data2,W
	btfsc	STATUS,Z
	goto	CVchk2
	movlw	20h		;is it CV545
	xorwf	data2,W
	btfsc	STATUS,Z
	goto	CVchk2
	movlw	09h		;only CVs 513(1) to 521(9)
	subwf	data2,W 
	return
CVchk2	bcf	STATUS,C
	return

Verbit	call	CVchk
	btfsc	STATUS,C
	return
	movf	data3,W
	andlw	b'00000111'
	movwf	arg4
	incf	arg4,F
	clrf	arg3
	bsf	STATUS,	C
;
Verb1	rlf	arg3,F		; get a 1 bit into position in Arg3
	decfsz	arg4,F
	goto	Verb1
	movlw	20h			;is it CV545
	xorwf	data2,W
	btfsc	STATUS,Z
	goto	Verb1a		;skip the offset
	movf    data2,W 	;CVs address directly
	btfsc	mode,0
	addlw	0x21
	goto	Verb1b
Verb1a	movf	data2,W 
Verb1b	call	EEread
	movwf	arg4
	btfsc	data3,4
	goto	Verb2
	movf	arg3,W		; load bit mask-> w
	andwf	arg4,W
	btfsc	data3,3		; if we test for 0, the value in w should be 0
	xorwf	arg3,W		; if we test for 1, invert the bit
	btfsc	STATUS,Z
	goto	Ackn
	return

Verb2	movf	arg3,	W	; change bit and rewrite	
	iorwf	arg4,	W	; get the orig value and set bit to 1
	btfss	data3,	3
	xorwf	arg3,	W	; clear the bit
	movwf	EEDATA
	goto	wrcon1



Dcontrl	btfss	confg,byte5
	btfsc	confg,byte4
        return
        movf	data2,W
        btfsc	STATUS,Z
        goto	Dreset
        xorlw	0x0E
        btfsc	STATUS,Z
        goto	Ackn
        return

Dreset	clrf	action		;clear action
	btfss	addcfg,reset
        return
        bsf	confg,service
        goto	Nextbyt



;Ackn sets ack output for 5ms (1024us X 5). 1024us is counted by
;bit 6 of TMR0 register.

Ackn    clrwdt
       	bsf	PORTB,6		;set ack line high
        clrf    TMR0
        movlw   .5          
        movwf   temp
aloop   btfss   TMR0,6
        goto    aloop
        bcf     TMR0,6
        decfsz  temp,F
        goto    aloop
        bcf	PORTB,6		;set ack line low
        return


        

;Generic EE write subroutine that writes EEDATA into EEADR. If the processor
;is busy with a previous write operation, the subroutine returns without
;writing. Inputs are EEADR and EEDATA.

EEwrite movlw   0x55
        movwf   EECON2
        movlw   0xAA
        movwf   EECON2
        bsf     EECON1,WR
eewrcon btfsc   EECON1,WR
        goto    eewrcon
        bcf     EECON1,WREN
        bcf     STATUS,RP0
		call	refresh		;refresh ram stack
        return

EEread	bsf		STATUS,RP0		;EEPROM read subroutine	
		movwf	EEADR		
		bsf		EECON1,RD	
		movf	EEDATA,W	
		bcf		STATUS,RP0
		return

refresh		movlw	0
	call	EEread
	movwf	CV513a
	bsf		CV513a,7	;10xxxxxx
	movlw	1
	call	EEread
	movwf	CV514a
	movlw	2		;	get CV515 settings
	call	EEread
	movwf	CV515a
	movlw	3		;	get CV516 settings
	call	EEread
	movwf	CV516a	
	movlw	4		;	get CV517 settings
	call	EEread
	movwf	CV517a
	movlw	5		;	get CV518 settings
	call	EEread
	movwf	CV518a
	movlw	8		;	get CV521
	call	EEread
	movwf	CV521a
	rlf		CV521a,F	
    rlf     CV521a,F        
    rlf     CV521a,F        
    rlf     CV521a,F        ;	now 0AAA0000
    comf	CV521a,F		;	top address is complemeted
    movlw	B'01110000'     ;	mask other bits             
    andwf   CV521a,F    	;			
	movlw	0x20			;	get CV545 settings
	call	EEread
	movwf	CV545a
	movlw	0x21		;get alternate CV set
	call	EEread
	movwf	CV513_2a
	bsf		CV513_2a,7
	movlw	0x22
	call	EEread
	movwf	CV514_2a
	movlw	0x23		;	get CV515 settings
	call	EEread
	movwf	CV515_2a
	movlw	0x24		;	get CV516 settings
	call	EEread
	movwf	CV516_2a	
	movlw	0x25		;	get CV517 settings
	call	EEread
	movwf	CV517_2a
	movlw	0x26		;	get CV518 settings
	call	EEread
	movwf	CV518_2a
	movlw	0x29		;	get CV521
	call	EEread
	movwf	CV521_2a
	rlf	CV521_2a,F	
    rlf     CV521_2a,F        
    rlf     CV521_2a,F        
    rlf     CV521_2a,F        	;	now 0AAA0000
    comf	CV521_2a,F			;  top address is complemeted
    movlw	B'01110000'     	;  mask other bits             
    andwf   CV521_2a,F
    clrf	mode
	btfsc	CV545a,7				;change mode?
	bsf		mode,0    	
	return



		org	0x2100
CV513	de	1	;address lo 6 bits
CV514	de	0xFF	;all outputs activated
CV515	de	0	; cont.
CV516	de	0	;
CV517	de	0	;
CV518	de	0	;
CV519	de	.5	;Version 5
CV520	de	.165	;Manufacturer
CV521	de	0	;address hi 3 bits

		org	0x211C
CV541	de	0x80	;config
		org	0x2120
CV545	de	0x8F	;mode  dual decoder, toggle on.


;second block of CVs for dual mode - valid when CV545 bit 7 = 1

		org	0x2121
CV513_2	de	2	;address lo 6 bits
CV514_2	de	0xFF	;all outputs activated
CV515_2	de	0	; cont
CV516_2	de	0	;
CV517_2	de	0	;
CV518_2	de	0	;
CV519_2	de	.5	;Version 5
CV520_2	de	.165	;Manufacturer
CV521_2	de	0	;address hi 3 bits

		org	0x213B
CV541_2	de	0x80	;config





        end

