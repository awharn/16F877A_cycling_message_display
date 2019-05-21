;**********************************************************************
;   This file is a basic code template for object module code         *
;   generation on the PIC16F877A. This file contains the              *
;   basic code building blocks to build upon.                         *
;                                                                     *
;   Refer to the MPASM User's Guide for additional information on     *
;   features of the assembler and linker (Document DS33014).          *
;                                                                     *
;   Refer to the respective PIC data sheet for additional             *
;   information on the instruction set.                               *
;                                                                     *
;**********************************************************************
;                                                                     *
;    Filename:      Lab8.asm                                          *
;    Date:          11/08/2017                                        *
;    File Version:  0.0.1                                             *
;                                                                     *
;    Author:        Andrew W. Harn                                    *
;    Company:       Geneva College                                    *
;                                                                     * 
;                                                                     *
;**********************************************************************
;                                                                     *
;    Files required: P16F877A.INC                                     *
;                                                                     *
;                                                                     *
;                                                                     *
;**********************************************************************
;                                                                     *
;    Notes:                                                           *
;                                                                     *
;                                                                     *
;                                                                     *
;                                                                     *
;**********************************************************************


    list        p=16f877a   ; list directive to define processor
    #include    <p16f877a.inc>  ; processor specific variable definitions
    
    __CONFIG _CP_OFF & _WDT_OFF & _BODEN_OFF & _PWRTE_OFF & _XT_OSC & _WRT_OFF & _LVP_OFF & _CPD_OFF & _DEBUG_ON

; '__CONFIG' directive is used to embed configuration data within .asm file.
; The labels following the directive are located in the respective .inc file.
; See respective data sheet for additional information on configuration word.

;***** VARIABLE DEFINITIONS

INT_VAR     UDATA_SHR      
w_temp      RES     1       ; Variable used for context saving 
status_temp RES     1       ; Variable used for context saving
pclath_temp RES	    1       ; Variable used for context saving
counter	    RES	    1	    ; Character counter
tmrctr	    RES	    1	    ; Timer second byte


TEMP_VAR    UDATA           ; Explicit address specified is not required
temp_count  RES     1       ; Temporary variable 
  
G_DATA      UDATA_OVR       ; Explicit address can be specified
flag        RES 2           ; Temporary variable (shared locations - G_DATA)

G_DATA      UDATA_OVR   
count       RES 2           ; Temporary variable (shared locations - G_DATA)

; Extern calls
extern	    LCDInit
extern	    temp_wr
extern	    i_write
extern	    d_write
extern	    LCDLine_1
extern	    LCDLine_2
;**********************************************************************
RESET_VECTOR    CODE    0x0000 ; processor reset vector
    nop                        ; nop for icd
    pagesel start
    goto    start              ; go to beginning of program


INT_VECTOR      CODE    0x0004 ; interrupt vector location

INTERRUPT

    movwf   w_temp          ; save off current W register contents
    movf    STATUS,w        ; move status register into W register
    movwf   status_temp     ; save off contents of STATUS register
    movf    PCLATH,w        ; move pclath register into w register
    movwf   pclath_temp     ; save off contents of PCLATH register

; isr code can go here or be located as a call subroutine elsewhere

    movf    pclath_temp,w   ; retrieve copy of PCLATH register
    movwf   PCLATH          ; restore pre-isr PCLATH register contents
    movf    status_temp,w   ; retrieve copy of STATUS register
    movwf   STATUS          ; restore pre-isr STATUS register contents
    swapf   w_temp,f
    swapf   w_temp,w        ; restore pre-isr W register contents
    retfie                  ; return from interrupt

MAIN_PROG       CODE

start

    nop             ; code starts here (example)
    banksel count           ; example
    clrf    count           ; example

; remaining code goes here

init
    banksel TRISA
    movlw   0x10	; Enable RA4 as input
    movwf   TRISA
    movlw   0x01	; Enable RB0 as input
    movwf   TRISB
    movlw   0x05	; Set up OPTION_REG Prescaler
    movwf   OPTION_REG
    
    pagesel LCDInit	; Set up LCD
    call    LCDInit
    
    banksel temp_wr	; Clear display instruction to temp_wr
    movlw   0x01	
    movwf   temp_wr	
    
    pagesel i_write	; Write instruction to LCD
    call    i_write
    
    banksel	temp_count	; Set status register
    clrf	temp_count	; Initialize to zero
    clrf	counter		; Initialize to zero
    
main
    banksel	TMR0
    movlw	d'130'		; Set up Timer0
    movwf	TMR0
    movlw	d'125'		; Set up TimerCounter
    movwf	tmrctr
    
    banksel	temp_count
    movf	temp_count,0	
    sublw	d'27'		; If index is set to 27...
    btfsc	STATUS,Z	; It becomes zero...
    clrf	temp_count	; And clears the counter back to 0
    
    movf	temp_count,W	; Load the table index
    pagesel	alookup
    call	alookup		; Performs a lookup operation
    banksel 	temp_wr
    movwf	temp_wr		; Writes the lookup value to temp_wr
    
    pagesel	d_write
    call	d_write		; Write the value in temp_wr to DDRAM in the display
    
    movlw	d'15'
    subwf	counter,W	; If it's the 16th character, skip to line 2
    pagesel 	LCDLine_2
    btfsc	STATUS,Z	; if zero then set cursor to the second line
    call	LCDLine_2
    incf	counter		; Keeps track of the position that is being written.
    incf	temp_count	; Keeps track of the table index
    
    pagesel	trb0		; Start checking things
    goto	trb0
    
trb0
    pagesel tra4
    movf    counter,0
    sublw   d'1'	; Test if counter is 1. Skip testing if it is
    btfsc   STATUS,Z
    goto    tra4
    
    banksel PORTB
    pagesel rb0		; Prepare for goto
    btfss   PORTB,0	; Port is active low, so skip if high
    goto    rb0
    pagesel tra4	; Continue to tra4
    goto    tra4

tra4
    pagesel checktime
    movf    counter,0
    sublw   d'33'	; Test if counter is 33. Skip testing if it is, to ignore any presses after 16 on the 2nd line
    btfsc   STATUS,Z
    goto    checktime
    
    banksel PORTA
    pagesel ra4		; Prepare for goto
    btfss   PORTA,4	; Port is active low, so skip if high
    goto    ra4
    pagesel checktime	; Check timer
    goto    checktime

    
rb0
    banksel PORTB	; Port is active low, test if released
    btfss   PORTB,0
    goto    rb0	    
    pagesel debounce	; Debounce
    call    debounce
    pagesel movlft	; Move left 1 position to blank it
    call    movlft	
    movlw   d'26'	; Location of space in lookup table, used to blank the location
    pagesel alookup
    call    alookup	; Performs a lookup operation
    banksel temp_wr
    movwf   temp_wr	; Writes the lookup value to temp_wr
    pagesel d_write
    call    d_write	; Write the value in temp_wr to DDRAM in the display
    incf    counter	; Increment counter, since write operation occurred
    pagesel movlft
    call    movlft	; Move left twice
    call    movlft
    banksel temp_count	; Reset lookup table value
    clrf    temp_count	
    pagesel main
    goto    main
        
ra4
    banksel PORTA	; Port is active low, test if released
    btfss   PORTA,4
    goto    ra4	
    pagesel debounce	; Debounce
    call    debounce
    banksel temp_count	; Reset lookup table value
    clrf    temp_count
    pagesel main
    goto    main
    
checktime
    pagesel trb0	; If timer has not expired, go back to start of loop
    btfss   INTCON,2
    goto    trb0	
    bcf	    INTCON,2	; Clear timer overflow flag
    banksel TMR0
    movlw   d'130'	; Reset the timer
    movwf   TMR0
    decfsz  tmrctr	; If timer has expired, decrement tmrctr. If zero, increment lookup value, else go back to start of loop
    goto    trb0
    pagesel movlft
    call    movlft
    pagesel main
    goto    main
    
movlft
    pagesel toline1
    movf    counter,0
    sublw   d'16'
    btfsc   STATUS,Z
    goto    toline1
    
    decf    counter	; Decrement character counter
    banksel temp_wr
    movlw   0x10	; Move cursor 1 to the left
    movwf   temp_wr
    pagesel i_write	; Write it to the LCD
    call    i_write
    return
    
toline1
    banksel temp_wr
    movlw   0x8F
    movwf   temp_wr
    pagesel i_write
    call    i_write
    decf    counter	; Equivalent amount of spaces removed from counter
    return
        
debounce
    bcf	    INTCON,2
    banksel TMR0	; Set up Timer0 and TimerCounter for Debounce Timing (approx 50ms)
    movlw   d'100'
    movwf   TMR0
    movlw   d'5'
    movwf   tmrctr
    
dbloop
    btfss   INTCON,2
    goto    dbloop
    bcf	    INTCON,2
    banksel TMR0
    movlw   d'100'
    movwf   TMR0
    decfsz  tmrctr
    goto    dbloop
    return
    
alookup			
    banksel count
    movwf   count
    pageselw alphabet ;Special assembly directive used 'pageselw' so that 
						;all 5 upper bits are written. 
						;Get the byte read and use it to
    movlw    alphabet    ;index into our jump table. If
    addwf    count,w      ;we crossed a 256-byte boundary,
    btfsc    STATUS,C                ;then increment PCLATH. Then load the
    incf     PCLATH,f                 ;program counter with computed goto.
    movwf    PCL    

alphabet		; Table containing all letters of alphabet, then space.
    retlw   'A'
    retlw   'B'
    retlw   'C'
    retlw   'D'
    retlw   'E'
    retlw   'F'
    retlw   'G'
    retlw   'H'
    retlw   'I'
    retlw   'J'
    retlw   'K'
    retlw   'L'
    retlw   'M'
    retlw   'N'
    retlw   'O'
    retlw   'P'
    retlw   'Q'
    retlw   'R'
    retlw   'S'
    retlw   'T'
    retlw   'U'
    retlw   'V'
    retlw   'W'
    retlw   'X'
    retlw   'Y'
    retlw   'Z'
    retlw   ' '
    
        END                       ; directive 'end of program'