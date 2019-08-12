; VRAM mapping stuff
.define SpriteSet           0       ; 0 for sprites to use tiles 0-255, 1 for 256+
.define NameTableAddress    $3800   ; must be a multiple of $800; usually $3800; fills $700 bytes (unstretched)
.define SpriteTableAddress  $3f00   ; must be a multiple of $100; usually $3f00; fills $100 bytes

.define debug 0

;==============================================================
; WLA-DX banking setup
; Note that this is a frame 2-only setup, allowing large data
; chunks in the first 32KB.
;==============================================================
.memorymap
defaultslot 0
slotsize $8000
slot 0 $0000
slotsize $4000
slot 1 $8000
.endme
.rombankmap
bankstotal 1
banksize $8000
banks 1
.endro

;==============================================================
; SDSC tag and SMS rom header
;==============================================================
.sdsctag 0.22,"SMS Chip-8 interpreter",SDSCNotes,"Maxim"

;==============================================================
; Memory usage
;==============================================================
.enum $c000
GameNumber      db
TileType        db
ResetFlag       db
GameControlMap  dw      ; pointer to game control map

GamePosition    dw      ; offset of where the current C8 instruction is
C8Registers     ds 16   ; 16 byte registers
C8I             dw      ; I  word register
C8Stack         ds 16*2 ; 16 level stack, builds up
C8SP            dw      ; Stack pointer
C8RandomSR      dw      ; Random number shift register
C8Input         dw      ; 16 bits -> inputs
C8DelayTimer    db      ; Delay timer
C8SoundTimer    db      ; Sound timer

C8Game          ds 4096 ; Chip-8 RAM

ScreenBuffer    ds 32*16*2

.ende

.bank 0 slot 0
.org 0

.org $0000
;==============================================================
; Boot section
;==============================================================
.section "!Boot section" force   ; Standard stuff (for the SMS anyway)
    di              ; disable interrupts (re-enable later)
    im 1            ; Interrupt mode 1
    jp main         ; jump to main program
.ends

.org $0066
;==============================================================
; Pause button handler
;==============================================================
.section "NMI handler" force
    ; Set reset flag
    push af
      ld a,1
      ld (ResetFlag),a
    pop af
    retn
.ends

.section "ZX7"
.define ZX7ToVRAM
.define ZX7ToVRAMScreenOn
.include "ZX7 decompressor.asm"
.ends

;==============================================================
; Main program
;==============================================================
.section "Main program" SEMIFREE
main:
    ld sp, $dff0    ; load stack pointer to not-quite-the-end of user RAM (avoiding paging regs)

    ; Load VDP with default values, thanks to Mike G :P
    ; hl = address of data
    ; b = size of data
    ; c = port
    ; otir = while (b>0) do {out (hl),c; b--}
    ld hl,VdpData
    ld b,VdpDataEnd-VdpData
    ld c,$bf
    otir

    call ClearVRAM

    ; Load palette
    ld hl,PaletteData
    ld b,_sizeof_PaletteData
    ld c,0
    call LoadPalette

    ; Load tiles
    ld hl,Graphics
    ld de,$4000
    call zx7_decompress

    xor a
    ld (TileType),a
    ld (GameNumber),a

ResetPoint:
    call TurnOffScreen

    ; Set sound tone and turn it off
    ld a,%10011111  ; volume
    out ($7f),a
    ld a,%10001111  ; tone 1
    out ($7f),a
    ld a,%00000010  ; tone 2
    out ($7f),a

    call TitleScreen

    call ClearNameTable

    ; To do: better border for C8 screen

    call ClearScreen    ; Clear C8 screen

    ; Draw debug labels
.if debug == 1
    jp +
    DebugLabels1:
    .asc "Inst:xxxx DT:xx ST:xx Ran#:xxxx",10
    .asc "0:xx 1:xx 2:xx 3:xx 4:xx 5:xx",10
    .asc "6:xx 7:xx 8:xx 9:xx A:xx B:xx",10
    .asc "C:xx D:xx E:xx F:xx",10
    .db 0
    DebugLabels2:
    .asc "Inp:xxxx Last:xxxx Diff:xxxx",10
    .db 0
+:  ld iy,NameTableAddress
    ld hl,DebugLabels1
    call WriteASCII
    ld iy,NameTableAddress+2*32*20
    ld hl,DebugLabels2
    call WriteASCII
.endif

    ; Turn screen on
    ld a,%11000100
;         ||||| |`- Zoomed sprites -> 16x16 pixels
;         ||||| `-- Doubled sprites -> 2 tiles per sprite, 8x16
;         ||||`---- 30 row/240 line mode
;         |||`----- 28 row/224 line mode
;         ||`------ VBlank interrupts
;         |`------- Enable display
;         `-------- Must be set (VRAM size bit)
    out ($bf),a
    ld a,$81
    out ($bf),a

    ld hl,C8Game+$200
    ld (GamePosition),hl

    ld hl,C8Stack
    ld (C8SP),hl

    ld hl,0
    ld (C8Input),hl

    xor a
    ld (C8DelayTimer),a
    ld (C8SoundTimer),a
    ld (ResetFlag),a

    ; Zero registers
    ld b,16
    ld hl,C8Registers
-:  ld (hl),a
    inc hl
    dec b
    jp nz,-

    ; Load stub into memory
    ld hl,Chip8Stub ; From
    ld de,C8Game    ; To
    ld bc,$200      ; How many
    ldir            ; Copy!

    ; Load game into memory
    ld a,(GameNumber)
    ld ix,GameLocations
    sla a
    ld b,0
    ld c,a
    add ix,bc
    ld d,(ix+1)
    ld e,(ix+0) ; de = location
    ld h,(ix+3)
    ld l,(ix+2)
    scf
    ccf
    sbc hl,de   ; hl = length
    push hl     ; put things back in the right places
    pop bc      ; bc = length
    push de
    pop hl      ; hl = location
    ld de,C8Game+$200       ; de = where to copy to
    ldir                    ; Copy

    ; Set GameControlMap
    ld hl,GameControlMaps
    ld b,0
    ld c,a  ; a is still 2*GameNumber - I want 24*
    sla c
    sla c   ; 8* is close enough
    add hl,bc
    add hl,bc
    add hl,bc   ; because I can add it 3 times. I'm lazy.
    ld (GameControlMap),hl

.if debug == 1
--: ld b,1 ; opcodes per timeslice
.else
--: ld b,11 ; opcodes per timeslice - change to a per-game setting (plus PAL?)?
.endif
-:  push bc
      call DecodeChip8
    pop bc
    dec b
    jp nz,-
    call WaitForVBlankNoInt     ; Slow it down :)
    call ScreenBufferToScreen
    call ProcessInput

    ; Decrement timers
    ld a,(C8DelayTimer)
    cp $00
    jp z,+
    dec a
+:  ld (C8DelayTimer),a

    ld a,(C8SoundTimer)
    cp $00
    jp z,+

    dec a
    ld (C8SoundTimer),a
    ld a,%10011000  ; sound on
    jp ++

+:  ld a,%10011111  ; sound off

++: out ($7f),a

.if debug == 1
    .asc "Inst:xxxx DT:xx ST:xx Ran#:xxxx"
    ld hl,NameTableAddress+2*13
    call VRAMToHL
    ld a,(C8DelayTimer)
    call WriteNumber
    ld hl,NameTableAddress+2*19
    call VRAMToHL
    ld a,(C8SoundTimer)
    call WriteNumber
    ld hl,NameTableAddress+2*(32*1+2)
    call VRAMToHL
    ld b,16
    ld hl,C8Registers
-:
    ld a,(hl)
    call WriteNumber
    in a,($be)
    in a,($be)
    in a,($be)
    in a,($be)
    in a,($be)
    in a,($be)
    ld a,17
    sub b
---:sub 6
    jp z,+
    jp nc,---
    jp ++
+:  in a,($be)
    in a,($be)
    in a,($be)
    in a,($be)
++: inc hl
    dec b
    jp nz,-
.endif
    jp --
.ends

.section "Input processor" SEMIFREE
ProcessInput:
    in a,($dc)
    cpl     ; so 1 = pressed
    ld l,a
    in a,($dd)
    cpl     ; so 1 = pressed
    ld h,a
    ; Reset handler
    bit 4,a
    jp z,+
    ld a,1
    ld (ResetFlag),a
+:
    ; Check if a reset is needed
    ld a,(ResetFlag)
    or a
    jp nz,ResetPoint  ; jump to main program start

    ; so hl = GG-R21RLDU21RLDU
    ;         21  222222111111
    ; Convert from 12 SMS to 16 C8 buttons

    ld c,12  ; how many bits to look at
    ld ix,(GameControlMap)
    ld de,0 ; input summation
-:
    bit 0,l ; Is lowest bit set = this key is pressed
    jp z,+  ; Jump ahead if it isn't

    ld a,(ix+1) ; Or mask with total so far
    or d
    ld d,a
    ld a,(ix+0)
    or e
    ld e,a

+:
    srl h
    rr l    ; shift so next bit is in position 0
    inc ix
    inc ix  ; Move to next input mask
    dec c
    jp nz,-

    ; Store result in memory
    ld (C8Input),de
.if debug == 1
    ld hl,NameTableAddress+2*(32*20+4)
    call VRAMToHL
    ld a,d
    call WriteNumber
    ld a,e
    call WriteNumber
.endif
    ret
.ends

.section "Title screen" SEMIFREE
TitleScreen:
    call ClearNameTable
    ld b,16 ; w
    ld c,8 ; h
    ld ix,LogoTilemap ; source
    ld iy,NameTableAddress+2*8 ; dest
    call DrawTilemap

    ld iy,NameTableAddress+2*32*10+2
    ld hl,TileScreenText
    call WriteASCII

    ; Turn screen on
    ld a,%11000100
    out ($bf),a
    ld a,$81
    out ($bf),a

    ; Initialise count for random number seed
    ld hl,%0101001110100100 ; lots of 1s, seems nicer

    ld a,(TileType)
    dec a
    ld (TileType),a
    call _NextTiles     ; Load tile set

-:  ; Title screen wait loop
    call WaitForVBlankNoInt ; slow it down a bit
    call _DrawGameInfo

    inc hl  ; increment random seed counter

    in a,($dc)  ; get input

    ; Any keys pressed?
    and %00111111
    cp  %00111111
    jp z,- ; If not, repeat
    ; If so, wait for all keys to be lifted
    push af
--: in a,($dc)
    and %00111111
    cp  %00111111
    jp nz,--
    pop af
    ; Process input
    bit 5,a ; Button 2
    call z,_NextTiles

    bit 2,a ; Left
    jp nz,+
    push af
      ld a,(GameNumber)
      dec a
      cp -1
      jp nz,++
      ld a,NumGames
      dec a
++:   ld (GameNumber),a
    pop af

+:  bit 3,a ; Right
    jp nz,+
    push af
      ld a,(GameNumber)
      inc a
      cp NumGames
      jp nz,++
      xor a
++:   ld (GameNumber),a
    pop af

+:  bit 1,a ; Down
    jp z,TextScroller

    bit 4,a ; Loop if button 1 not pressed
    jp nz,-

    ; Turn off screen
    ld a,%10000100
    out ($bf),a
    ld a,$81
    out ($bf),a

    ; Store random seed
    ; Make sure it's not 0
    ld a,h
    or l
    jp nz,+
    ld l,1
+:  ld (C8RandomSR),hl
    ret

; TitleScreen loop uses a,hl

_DrawGameInfo:
    push af
    push hl
      ; Clear game text area (5 lines)
      ld hl,NameTableAddress+2*(32*19)
      call VRAMToHL
      ld bc,5*32*2
-:    xor a
      out ($be),a
      dec bc
      ld a,b
      or c
      jp nz,-

      ld a,(GameNumber)   ; See which game we want
      ; Get its associated text
      ld c,a
      sla c
      ld b,0
      ld ix,GameText
      add ix,bc
      ld h,(ix+1)
      ld l,(ix+0) ; hl = location of text
      ld iy,NameTableAddress+2*(32*19)+2
      call WriteASCII
    pop hl
    pop af
    ret

_NextTiles:
    push af
    push hl
      ld a,(TileType)
      inc a
      cp NumTileSets
      jp nz,+
      xor a
+:    ld (TileType),a
      ; a = new tile set number
      sla a   ; *2
      ld ix,TileSets
      ld c,a
      ld b,0
      add ix,bc   ; ix now points to the pointer to the tileset
      ld h,(ix+1)
      ld l,(ix+0) ; hl now points to the tileset I want

      ; Set address $2000 (256x32), or $4000 for write flag -> $6000
      xor a
      out ($bf),a
      ld a,$60
      out ($bf),a
      ; Pause for 28 cycles before each write
      nop
      nop
      nop
      ; I need to output c bytes to $be, starting at hl
      ld bc,16*32
-:    ld a,(hl)
      out ($be),a
      dec bc
      inc hl
      ld a,b
      or c
      jp nz,-
    pop hl
    pop af
    ret

; I want to store text as ASCII but I also want to use nicer (sort of) chars for the button key images here.
; Thus I will use .asciitable to map the chars for those but leave the ASCII unchanged.

.asciitable
map " " to "~" = 32
map "«" = 127
map "»" = 128
map "´" = 129
map "¸" = 130
map "¹" = 131
map "²" = 132
.enda

TileScreenText:
;    12345678901234567890123456789012
.asc "Controls:",10
.asc "¹  Play",10
.asc "²  Change screen style",10
.asc "«» Change game",10
.asc "¸  Read about Chip-8",10,10
.asc "Press Pause or Reset in-game",10
.asc "to return to this menu"
.asc 0

.ends

.section "Display updater" SEMIFREE
ScreenBufferToScreen:
    push bc
    push hl
      ld hl,NameTableAddress+2*32*4
      call VRAMToHL

      ; hl = where to copy from
      ; Output to $be
      ; Output $400 bytes
      ld hl,ScreenBuffer
      ld bc,$400
-:    ld a,(hl)       ; 7
      out ($be),a
      dec bc          ; 6
      ld a,b          ; 4
      or c            ; 4
      inc hl          ; 6
      jp nz,-         ; 10 - total 37, plenty
    pop hl
    pop bc
    ret
.ends

.section "Chip-8 interpreter" SEMIFREE

DecodingRoutines:   ; Table of locations of instructions/groups for 1st nibble of 1st byte
.dw C80,C81,C82,C83,C84,C85,C86,C87,C88,C89,C8a,C8b,C8c,C8d,C8e,C8f

DecodeChip8:
    ld ix,(GamePosition)    ; Get where I am

.if debug == 1
    ld hl,NameTableAddress+2*5
    call VRAMToHL
    ld a,(ix+0)
    call WriteNumber
    ld a,(ix+1)
    call WriteNumber
.endif

    ld a,(ix+0)             ; get 1st byte of instruction
    ld c,a
    srl c       ; Shift right 3
    srl c
    srl c
    res 0,c     ; Unset bit 0
    ld b,0      ; So bc = high nibble * 2
    ld iy,DecodingRoutines
    add iy,bc
    ld h,(iy+1)
    ld l,(iy+0)
    jp (hl)

    DecodeFinished:
    inc ix
    inc ix
    DecodeFinishedNoInc:
    ld (GamePosition),ix
    ret

RegInHL:    ; pass reg no. in low nibble of a, returns its location in hl
    and $0f
    push de
      ld d,0
      ld e,a
      ld hl,C8Registers
      add hl,de
    pop de
    ret

GetRegValue ; pass reg no. in low nibble of a, returns its value in a
    push hl
      call RegInHL
      ld a,(hl)
    pop hl
    ret

ClearScreen:
    ld de,$100          ; value
    ld iy,ScreenBuffer  ; where
    ld bc,512           ; how many
-:  ld (iy+1),d
    ld (iy+0),e
    inc iy
    inc iy
    dec bc
    ld a,b
    or c
    jp nz,-
    ret

; Instruction handlers:
C80:    ; SYS/CLS/RET
    ; Check that it's 00 (if not, unhandled SYS)
    cp $00
    jp nz,DecodeFinished
    ld a,(ix+1)
    cp $e0
    jp z,_CLS
    cp $ee
    jp z,_RET
    jp DecodeFinished
_CLS:
    ; Clear screen
    call ClearScreen
    jp DecodeFinished
_RET:
    ; Return after CALL
    ld iy,(C8SP); Get stack pointer
    dec iy      ; - Decrease it
    dec iy      ; /
    ld (C8SP),iy; Store it again
    ld h,(iy+1) ; - Get call location
    ld l,(iy+0) ; /
    push hl     ; - Put that in ix
    pop ix      ; /
    jp DecodeFinished

C81:    ; JP    1nnn    Jump to nnn
    and $0f         ; - Get 0n in d
    ld d,a          ; /
    ld e,(ix+1)     ; and nn in e -> de = 0nnn
    ld ix,C8Game    ; ix=C8Game+nnn
    add ix,de
    jp DecodeFinishedNoInc

C82:    ; CALL  2nnn    Push the current location onto the stack and jump to nnn
    ld iy,(C8SP)    ; Get stack pointer
    push ix         ; \
    pop hl          ; \
    ld (iy+1),h     ; - Store current instruction location there
    ld (iy+0),l     ; /
    inc iy          ; \
    inc iy          ; - Move stack pointer forward
    ld (C8SP),iy    ; Save it
    jp C81          ; Then use the JP code

C83:    ; SE    3rnn    Skip next instruction if vr == nn
    call RegInHL

    ld a,(hl)
    cp (ix+1)
    jp nz,DecodeFinished
    inc ix      ; If it is equal then inc ix twice
    inc ix
    jp DecodeFinished

C84:    ; SNE   4rnn    Skip next instruction if vr != nn
    call RegInHL
    ld a,(hl)
    cp (ix+1)
    jp z,DecodeFinished
    inc ix      ; If it is not equal then inc ix twice
    inc ix
    jp DecodeFinished

C85:    ; SE    5xy0    Skip next instruction if vx == vy
    call RegInHL
    ld a,(hl)
    ld b,a
    ld a,(ix+1)
    srl a
    srl a
    srl a
    srl a
    call RegInHL
    ld a,(hl)
    cp b                ; compare with vx
    jp nz,DecodeFinished
    inc ix              ; skip next instruction if it is not equal
    inc ix
    jp DecodeFinished

C86:    ; LD    6rnn    load vr with value nn
    call RegInHL
    ld a,(ix+1)
    ld (hl),a
    jp DecodeFinished

C87:    ; ADD   7rnn    Add nn to vr
    call RegInHL
    ld a,(hl)       ; Get vx
    add a,(ix+1)    ; Add the value
    ld (hl),a       ; Store
    jp DecodeFinished

C88Operations:
.dw _EQ,_OR,_AND,_XOR,_ADD,_SUB,_SHR,_SUBN,_NOP,_NOP,_NOP,_NOP,_NOP,_NOP,_SHL,_NOP

C88:    ; Operations (lots of them)
        ;       8xyn    Perform operation n on vx and vy
    call RegInHL

    ld b,(hl)   ; b = vx

    ; Get register y
    ld a,(ix+1)
    srl a
    srl a
    srl a
    srl a
    push hl ; pop it at OperationEnd
      call RegInHL
      ld c,(hl)   ; c = vy

      ld a,(C8Registers+$f)   ; Load vf
      ld d,a
      ld a,(ix+1)
      and $0f     ; Which operation?
      ld iy,C88Operations
WhichOpLoop:
      or a        ; Is it 0?
      jp nz,+
      ld h,(iy+1) ; If so, jump to that operation
      ld l,(iy+0)
      jp (hl)
+:    dec a       ; Otherwise, dec a and inc iy twice
      inc iy
      inc iy
      jp WhichOpLoop

    ; Operations: resultant vx in a, vf in d (if changed)
_EQ:     ; vx = vy
      ld a,c
      jp _OperationEnd
_OR:     ; vx |= vy
      ld a,b
      or c
      jp _OperationEnd
_AND:    ; vx &= vy
      ld a,b
      and c
      jp _OperationEnd
_XOR:    ; vx ^= vy
      ld a,b
      xor c
      jp _OperationEnd
_ADD:    ; vx += vy; set vf
      ld d,0  ; reset vf
      ld a,b
      add a,c
      jp nc,_OperationEnd
      ld d,1
      jp _OperationEnd
_SUB:    ; vx -= vy; vf = !borrow
      ld d,0
      ld a,b
      sub c
      jp c,_OperationEnd
      ld d,1
      jp _OperationEnd
_SHR:    ; vx >>= 1; vf = shifted out bit
      ld d,0
      ld a,b
      srl a
      jp nc,_OperationEnd
      ld d,1
      jp _OperationEnd
_SUBN:   ; vx = vy - vx; vf = !borrow
      ld d,0
      ld a,c
      sub b
      jp c,_OperationEnd
      ld d,1
      jp _OperationEnd
_SHL:    ; vx <<= 1; vf = shifted out bit
      ld d,0
      ld a,b
      sla a
      jp nc,_OperationEnd
      ld d,1
      jp _OperationEnd
_NOP:
    ; fall through

_OperationEnd:
    pop hl
    ld (hl),a   ; Store vx
    ld a,d
    ld (C8Registers+$f),a
    jp DecodeFinished

C89:    ; SNE   9xy0    Skip next instruction if vx != vy
    call RegInHL
    ld a,(hl)
    ld b,a          ; b = vx

    ld a,(ix+1)
    srl a
    srl a
    srl a
    srl a
    call RegInHL
    ld a,(hl)
    cp b            ; compare with vx
    jp z,DecodeFinished
    inc ix          ; skip next instruction
    inc ix
    jp DecodeFinished

C8a:    ; LD I  annn    Load register I with nnn
    and $0f
    ld d,a
    ld e,(ix+1)
    ld (C8I),de
    jp DecodeFinished

C8b:    ; JP+   bnnn    Jump to nnn + v0
    and $0f         ; - Get 0n in d
    ld d,a          ; /
    ld e,(ix+1)     ; and nn in e -> de = 0nnn
    ld ix,C8Game        ; ix=C8Game
    add ix,de           ;     +nnn
    ld de,(C8Registers) ;
    add ix,de           ;     +v0
    jp DecodeFinishedNoInc

C8c:    ; RND   crnn    Load register r with a random number ANDed with nn
    call RegInHL
    call GetRandomNumber
    and (ix+1)
    ld (hl),a
    jp DecodeFinished

GetRandomNumber:
    ; Returns a random 8-bit number in a
.if debug == 1
    ld de,(C8RandomSR)  ; Load shift register value
    push hl
    ld hl,NameTableAddress+2*27
    call VRAMToHL
    ld a,d
    call WriteNumber
    ld a,e
    call WriteNumber
    pop hl
.endif
/*
; Old method: copy the PSG noise feedback register

    ld de,(C8RandomSR)  ; Load shift register value
    ld b,0              ; reset output
    ld c,8              ; no. of bits to shift by
  -:ld a,e
    srl d               ; Shift de into b
    rr e
    rr b
    and %00001001       ; Tap bits 0 and 3 (before shifting) (same as PSG noise :)
    jp pe,+             ; if odd parity, input 1 into shift register (ie. xor of tapped bits)
    set 7,d
  +:dec c
    jp nz,-

    ld (C8RandomSR),de  ; Put shift register back in RAM
    ld a,b              ; Return in a
    ret
*/
; New method: stolen from Phantasy Star
    push hl
      ld hl,(C8RandomSR)
      ld a,h         ; get high byte
      rrca           ; rotate right by 2
      rrca
      xor h          ; xor with original
      rrca           ; rotate right by 1
      xor l          ; xor with low byte
      rrca           ; rotate right by 4
      rrca
      rrca
      rrca
      xor l          ; xor again
      rra            ; rotate right by 1 through carry
      adc hl,hl      ; add C8RandomSR to itself
      jr nz,+
      ld hl,$733c    ; if last xor resulted in zero then re-seed random number generator
+:    ld a,r         ; r = refresh register = semi-random number
      xor l          ; xor with l which is fairly random
      ld (C8RandomSR),hl
    pop hl
    ret                ; return random number in a

C8d:    ; DRW   dxyn    Draw n-line sprite at I at location in vx,vy with collision flag
        ; Hard!
    ; First, get vx
    call RegInHL
    ld a,(hl)
    and $3f     ; Just low 6 bits because higher is off the screen
    ld d,a      ; d = vx

    ; Then y
    ld a,(ix+1)
    srl a       ; shift right by 4
    srl a
    srl a
    srl a       ; a = 0y
    call RegInHL
    ld a,(hl)
    and $1f     ; Just low 5 bits because higher is off the screen
    ld e,a      ; e = vy

    ; Don't destroy de now!

    ; Loop though sprite
    ld hl,C8Game
    ld bc,(C8I)
    add hl,bc       ; hl = location of sprite in ram

    ld a,(ix+1)
    and $0f
    ld b,a          ; b = number of lines in sprite
    ld iy,C8Registers+$f    ; carry flag location for XORPixel
    ld (iy+0),0             ; reset collision flag
-:  ld c,8      ; c = number of pixels per line (8)
    ld a,(hl)   ; value of line
--: rla         ; rotate left into carry
    call c,XORPixel ; if it's a 1 then draw it
    inc d       ; move right 1 pixel
    dec c       ; Repeat c times
    jp nz,--

    inc hl      ; move to next line's definition
    push af
      ld a,d      ; move back to original x
      sub 8
      ld d,a
    pop af
    inc e       ; move to next row
    dec b       ; Repeat b times
    jp nz,-

    jp DecodeFinished

XORPixel:
    ; Pass xy in de
    push hl
    push bc
    push af

      ; I want a tile number and pixel number for the tile, ie.
      ; tn = ((x/2) + 32*(y/2)) * 2 + ScreenBuffer
      ; pn = 1<<(x%2) + 1<<(y%2 + 2)
      ; de = xy

      ; Wrap around screen
/*      ld a,d
      and 63
      ld d,a
      ld a,e
      and 31
      ld e,a
*/
      ; Don't draw offscreen
      ld a,d
      and %11000000
      jp nz,_SkipPixel
      ld a,e
      and %11100000
      jp nz,_SkipPixel

      ld hl,0
      ld b,0
      ld c,d      ; bc = x
      srl c       ; bc = x/2
      add hl,bc   ; hl = x/2

      ld b,0
      ld c,e      ; bc = y
      res 0,c     ; bc = 2*(y/2)
      sla c       ; x4
      sla c       ; x8
      sla c       ; x16
      rl b
      sla c       ; x32
      rl b
      add hl,bc   ; hl = x/2 + 32*(y/2)

      sla l
      rl h        ; hl = (x/2 + 32*(y/2)) * 2

      ld bc,ScreenBuffer
      add hl,bc   ; hl = (x/2 + 32*(y/2)) * 2 + ScreenBuffer

      ld b,0
      bit 0,d         ; z if x%2=0 -> 1000; 0100 otherwise
      jp z,+
      set 2,b
      jp ++
+:    set 3,b
++:   bit 0,e         ; z if y%2=0 -> correct; >>2 otherwise
      jp z,+
      sra b
      sra b
+:

      ; Now hl = tile location in buffer and b = XOR for pixel
      ld a,(hl)
      push af
        and b   ; Are any of the pixels I want already set?
        jp z,+
        ld (iy+0),1
+:    pop af
      xor b
      ld (hl),a
      ld a,1
      inc hl
      ld (hl),a

      _SkipPixel:

    pop af
    pop bc
    pop hl
    ret

C8e:    ; SKP/SKNP  Enxx    Skip next instruction if key in Vn is (xx=9e)/is not (xx=a1) pressed
    ; SKP or SKNP?
    ld a,(ix+1)
    cp $9e
    jp z,_SKP
    cp $A1
    jp z,_SKNP
    jp DecodeFinished

_SKP:
    ld a,(ix+0)         ; get 1st byte of instruction = En
    call GetRegValue    ; now a=value in Vn
    and $0f             ; ignore high nibble
    ld hl,(C8Input)     ; Get input in hl
    ; Rotate hl by a bits
-:  or a    ; Is a 0?
    jp z,+  ; Break out if it is
    srl h   ; Else rotate hl by 1 bit
    rr  l
    dec a
    jp -
+:  bit 0,l ; Is lowest bit set?
    jp z,DecodeFinished ; If it isn't (z), it's not pressed -> don't skip
    inc ix
    inc ix
    jp DecodeFinished

_SKNP:
    ld a,(ix+0)         ; get 1st byte of instruction = En
    call GetRegValue    ; now a=value in Vn
    and $0f             ; ignore high nibble
    ld hl,(C8Input)     ; Get input in hl
    ; Rotate hl by a bits
-:  or a    ; Is a 0?
    jp z,+  ; Break out if it is
    srl h   ; Else rotate hl by 1 bit
    rr  l
    dec a
    jp -
+:  bit 0,l ; Is lowest bit set?
    jp nz,DecodeFinished ; If it is (nz), it's pressed -> don't skip
    inc ix
    inc ix
    jp DecodeFinished

C8f:    ; Various
    call RegInHL
    ; See which one
    ld a,(ix+1)
    cp $07
    jp z,_LD_Vx_DT
    cp $0A
    jp z,_LD_Vx_K
    cp $15
    jp z,_LD_DT_Vx
    cp $18
    jp z,_LD_ST_Vx
    cp $1E
    jp z,_ADD_I_VX
    cp $29
    jp z,_LD_F_Vx
    cp $33
    jp z,_LD_B_Vx
    cp $55
    jp z,_REGSTOMEM
    cp $65
    jp z,_MEMTOREGS

_LD_Vx_DT:  ; vx = DT
    ld a,(C8DelayTimer)
    ld (hl),a
    jp DecodeFinished
    
_LD_Vx_K:   ; Wait for a key to be pressed, vx = key
    ; Update screen
    call ScreenBufferToScreen
-:  ld de,(C8Input) ; Get current input state
    push ix
    push iy
    push hl
    push de
    call ProcessInput
    pop de
    pop hl
    pop iy
    pop ix
    ld bc,(C8Input)

.if debug == 1
    push hl
      ld hl,NameTableAddress+2*(32*20+14)
      call VRAMToHL
    pop hl
    ld a,d
    call WriteNumber
    ld a,e
    call WriteNumber
.endif

    ; bc = new, de = old
    ; I want to loop unless there's a 1 in bc which isn't in de
    ; I want bc=bc&(!de)>0
    ld a,d
    cpl
    and b
    ld b,a
    ld a,e
    cpl
    and c
    ld c,a
    ; See if bc contains any 1s
    or b
    jp z,-

        ; OK, we have a key... which one changed?
.if debug == 1
    push hl
      ld hl,NameTableAddress+2*(32*20+24)
      call VRAMToHL
    pop hl
    ld a,b
    call WriteNumber
    ld a,c
    call WriteNumber
.endif

    xor a
-:  bit 0,c
    jp nz,+
    ; Not this one?
    srl b
    rr c
    inc a
    jp -
+:  ; a = key number
    ld (hl),a
    jp DecodeFinished

_LD_DT_Vx:  ; DT = vx
    ld a,(hl)
    ld (C8DelayTimer),a
    jp DecodeFinished

_LD_ST_Vx:  ; ST = vx
    ld a,(hl)
    ld (C8SoundTimer),a
    jp DecodeFinished

_ADD_I_VX:  ; I += vx
    ld b,0
    ld c,(hl)
    ld hl,(C8I)
    add hl,bc
    ld (C8I),hl
    jp DecodeFinished

_LD_F_Vx:   ; Set I to point to a 5x4 sprite representing vx
            ; I = 5*vx (for me anyway)
    ld a,(hl)   ; vx
    and $0f     ; low nibble only
    ld b,a
    sla a       ; vx*2
    sla a       ; vx*4
    add a,b     ; vx*5
    ld b,0
    ld c,a
    ld (C8I),bc
    jp DecodeFinished

_LD_B_Vx:   ; Set 3 bytes from I to BCD value of vx
    ld a,(hl)   ; Get value
    ld iy,C8Game
    ld bc,(C8I)
    add iy,bc   ; Get where to write
    ld b,0      ; Digit counter
-:  cp 100      ; Is it more than 100?
    jp c,+      ; If not, move on
    sub 100
    inc b
    jp -
+:  ld (iy+0),b
    ld b,0      ; Repeat with 10
-:  cp 10
    jp c,+
    sub 10
    inc b
    jp -
+:  ld (iy+1),b
    ld (iy+2),a ; I'm left with the 1s in a
    jp DecodeFinished

_REGSTOMEM:     ; Copy x+1 registers into RAM from I
    ld hl,C8Game
    ld bc,(C8I)
    add hl,bc
    push hl
    pop de              ; to
    ld hl,C8Registers   ; from
    ld a,(ix+0)
    and $0f
    inc a
    ld b,0
    ld c,a              ; how many
    ldir
    jp DecodeFinished

_MEMTOREGS:
    ld hl,C8Game
    ld bc,(C8I)
    add hl,bc           ; from
    ld de,C8Registers   ; to
    ld a,(ix+0)
    and $0f
    inc a
    ld b,0
    ld c,a              ; how many
    ldir
    jp DecodeFinished
.ends

;==============================================================
; VDP initialisation data
;==============================================================
.section "Data" SEMIFREE
VdpData:
.db %00000100,$80
;    |||||||`- Disable sync
;    ||||||`-- Enable extra height modes
;    |||||`--- SMS mode instead of SG
;    ||||`---- Shift sprites left 8 pixels
;    |||`----- Enable line interrupts
;    ||`------ Blank leftmost column for scrolling
;    |`------- Fix top 2 rows during scrolling
;    `-------- Fix right 8 columns during scrolling
.db %10000100,$81
;    ||||| |`- Zoomed sprites -> 16x16 pixels
;    ||||| `-- Doubled sprites -> 2 tiles per sprite, 8x16
;    ||||`---- 30 row/240 line mode
;    |||`----- 28 row/224 line mode
;    ||`------ VBlank interrupts
;    |`------- Enable display
;    `-------- Must be set (VRAM size bit)
.db (NameTableAddress>>10) |%11110001,$82
.db (SpriteTableAddress>>7)|%10000001,$85
.db (SpriteSet<<2)         |%11111011,$86
.db $f|$f0,$87
;    `-------- Border palette colour (sprite palette)
.db $00,$88
;    ``------- Horizontal scroll
.db $00,$89
;    ``------- Vertical scroll
.db $ff,$8a
;    ``------- Line interrupt spacing ($ff to disable)
VdpDataEnd:

;==============================================================
; My chosen palette
;==============================================================
PaletteData:
.incbin "images/palette.bin"

.incdir "Images"

Graphics:
.incbin "graphics.png.tiles.zx7"

LogoTilemap:
.incbin "graphics.png.tilemap.bin" skip 16*9*2

.define NumTileSets 6
TileSets:
.dw TVTiles,LCDTiles,StrangeTiles,InsertNameHere,Checked,Scanlines
TVTiles:
.incbin "tiles1.png.tiles.zx7"
LCDTiles:
.incbin "tiles2.png.tiles.zx7"
StrangeTiles:
.incbin "tiles3.png.tiles.zx7"
InsertNameHere:
.incbin "tiles4.png.tiles.zx7"
Checked:
.incbin "tiles5.png.tiles.zx7"
Scanlines:
.incbin "tiles6.png.tiles.zx7"

.ends

.section "!Game data" FREE
Chip8Stub:
.db %11110000 ; 0
.db %10010000
.db %10010000
.db %10010000
.db %11110000

.db %00100000 ; 1
.db %01100000
.db %00100000
.db %00100000
.db %01110000

.db %11110000 ; 2
.db %00010000
.db %11110000
.db %10000000
.db %11110000

.db %11110000 ; 3
.db %00010000
.db %11110000
.db %00010000
.db %11110000

.db %10010000 ; 4
.db %10010000
.db %11110000
.db %00010000
.db %00010000

.db %11110000 ; 5
.db %10000000
.db %11110000
.db %00010000
.db %11110000

.db %11110000 ; 6
.db %10000000
.db %11110000
.db %10010000
.db %11110000

.db %11110000 ; 7
.db %00010000
.db %00100000
.db %01000000
.db %01000000

.db %11110000 ; 8
.db %10010000
.db %11110000
.db %10010000
.db %11110000

.db %11110000 ; 9
.db %10010000
.db %11110000
.db %00010000
.db %11110000

.db %11110000 ; A
.db %10010000
.db %11110000
.db %10010000
.db %10010000

.db %11100000 ; B
.db %10010000
.db %11100000
.db %10010000
.db %11100000

.db %11110000 ; C
.db %10000000
.db %10000000
.db %10000000
.db %11110000

.db %11100000 ; D
.db %10010000
.db %10010000
.db %10010000
.db %11100000

.db %11110000 ; E
.db %10000000
.db %11110000
.db %10000000
.db %11110000

.db %11110000 ; F
.db %10000000
.db %11110000
.db %10000000
.db %10000000

.incdir "games"
G01:.incbin "Blinky.c8"
G02:.incbin "Blitz.c8"
G03:.incbin "Breakout.c8"
G04:.incbin "Brix.c8"
G05:.incbin "Cave.ch8"
G06:.incbin "Chip-8 Logo.ch8"
G07:.incbin "Connect 4.c8"
G08:.incbin "Filter.ch8"
G09:.incbin "Guess [B].c8"
G10:.incbin "Hidden!.c8"
G11:.incbin "IBM Logo.ch8"
G12:.incbin "Kaleid.c8"
G13:.incbin "Maze [B].c8"
G14:.incbin "Merlin.c8"
G15:.incbin "Missile.c8"
G16:.incbin "Pong [A].c8"
G17:.incbin "Pong 2.c8"
G18:.incbin "Puzzle.c8"
G19:.incbin "Rocket (fixed).ch8"
G20:.incbin "Space Invaders [v0.91].c8"
G21:.incbin "Squash.c8"
G22:.incbin "Syzygy.c8"
G23:.incbin "Tank.c8"
G24:.incbin "Tapeworm.ch8"
G25:.incbin "Tetris.c8"
G26:.incbin "Ufo.c8"
G27:.incbin "Vers.c8"
G28:.incbin "Vertical Brix.c8"
G29:.incbin "Wall.c8"
G30:.incbin "Wipeoff.c8"
GEnd:nop

.define NumGames 30

GameText:
; An array of NumGames words pointing to null-terminated strings holding each game's name, CR, description/help
; Size limit 5 lines including title
.dw D01,D02,D03,D04,D05,D06,D07,D08,D09,D10,D11,D12,D13,D14,D15,D16,D17,D18,D19,D20,D21,D22,D23,D24,D25,D26,D27,D28,D29,D30
D01:
;    123456789012345678901234567890 ; 30 for TV cutting off sides
; UDLR12 = ´¸«»¹²

.asc "Blinky",10
.asc "A Pac-Man clone. Eat dots;",10
.asc "avoid ghosts; eat power pills",10
.asc "to eat ghosts.",10
.asc "´¸«» to move.",0
D02:
.asc "Blitz",10
.asc "Bomber clone. Bomb all the",10
.asc "towers before your plane",10
.asc "flies too low.",10
.asc "¹ to drop a bomb.",0
D03:
.asc "Breakout",10
.asc "Break the bricks with your",10
.asc "ball."10
.db 10
.asc "«» to move.",0
D04:
.asc "Brix",10
.asc "A Breakout clone - break the",10
.asc "bricks with your ball.",10
.db 10
.asc "«» to move.",0
D05:
.asc "Cave",10
.asc "Manoeuvre through the cave",10
.asc "without hitting the walls.",10
.db 10
.asc "¹ to start, ´¸«» to move.",0
D06:
.asc "Chip-8 Logo",10
.asc "Draws a picture.",10
.db 10
.db 10
.asc "No controls.",0
D07:
.asc "Connect 4          (2 players)",10
.asc "Make 4 in a row to win.",10
.db 10
.db 10
.asc "«» to move, ¹ to drop a coin.",0
D08:
.asc "Filter",10
.asc "Catch the falling blocks.",10
.db 10
.db 10
.asc "«» to move.",0
D09:
.asc "Guess",10
.asc "Think of a number from 0 to",10
.asc "62. The game will guess your",10
.asc "number!",10
.asc "¹ if it's shown, ² if not.",0
D10:
.asc "Hidden!",10
.asc "Find pairs of matching cards.",10
.db 10
.db 10
.asc "´¸«» to move, ¹ to turn card.",0
D11:
.asc "IBM Logo",10
.asc "Draws a picture.",10
.db 10
.db 10
.asc "No controls.",0
D12:
.asc "Kaleidoscope",10
.asc "Draws a pattern based on your",10
.asc "input.",10
.db 10
.asc "´¸«» to draw, ¹ to finish.",0
D13:
.asc "Maze",10
.asc "Draws a random maze pattern.",10
.db 10
.db 10
.asc "No controls.",0
D14:
.asc "Merlin",10
.asc "Simon clone. Repeat the",10
.asc "pattern it shows to progress.",10
.asc "«´ to select",10
.asc "¸» a square.",0
D15:
.asc "Missile",10
.asc "You have 10 shots to hit 8",10
.asc "targets for 5 points each.",10
.db 10
.asc "¹ to fire.",0
D16:
.asc "Pong               (2 players)",10
.asc "Bounce the ball past your",10
.asc "opponent's paddle to win a",10
.asc "point.",10
.asc "´¸ to move.",0
D17:
.asc "Pong 2             (2 players)",10
.asc "Bounce the ball past your",10
.asc "opponent's paddle to win a",10
.asc "point.",10
.asc "´¸ to move.",0
D18:
.asc "Puzzle",10
.asc "Try to rearrange the tiles",10
.asc "back to the original order.",10
.db 10
.asc "´¸«» to move a tile.",0
D19:
.asc "Rocket",10
.asc "Realistic space simulation.",10
.db 10
.db 10
.asc "¹ to blast off.",0
D20:
.asc "Space Invaders",10
.asc "Stop the alien invasion!",10
.db 10
.db 10
.asc "«» to move, ¹ to fire.",0
D21:
.asc "Squash",10
.asc "See how long it takes to lose",10
.asc "5 super-fast balls.",10
.db 10
.asc "´¸ to move.",0
D22:
.asc "Syzygy",10
.asc "Eat food for points, but don't",10
.asc "eat yourself!",10
.asc "¹=no border, ²=border.",10
.asc "´¸«» to move.",0
D23:
.asc "Tank",10
.asc "Shoot the target with 25",10
.asc "bombs. Lose 5 every time it",10
.asc "touches you.",10
.asc "´¸«» to move, ¹ to fire.",0
D24:
.asc "Tapeworm",10
.asc "Like Tron, only with only",10
.asc "one player so it's quite",10
.asc "pointless.",10
.asc "´¸«» to change direction.",0
D25:
.asc "Tetris",10
.asc "Make a solid horizontal line,",10
.asc "it will disappear for 1 point",10
.asc "«» to move,",10
.asc "¹ to rotate, ¸ to drop.",0
D26:
.asc "UFO",10
.asc "Shoot the UFOs. The smaller",10
.asc "one is worth more points, and",10
.asc "you only have 15 shots.",10
.asc "«´» to fire.",0
D27:
.asc "Vers               (2 players)",10
.asc "Tron clone. Don't hit the",10
.asc "wall, your track or your",10
.asc "opponent's track.",10
.asc "´¸«» to move.",0
D28:
.asc "Vertical Brix",10
.asc "Breakout clone, buggy",10
.asc "sometimes. Break the bricks",10
.asc "with your ball.",10
.asc "´¸ to move.",0
D29:
.asc "Wall",10
.asc "See how many times you can",10
.asc "return the super-fast ball.",10
.db 10
.asc "´¸ to move.",0
D30:
.asc "Wipeoff",10
.asc "Difficult Breakout clone",10
.asc "Break the bricks with your",10
.asc "ball.",10
.asc "«» to move.",0

GameLocations:
; An array of NumGames+1 words giving the start location of each game
; A game's size is given by [n+1]-[n]
.dw G01,G02,G03,G04,G05,G06,G07,G08,G09,G10,G11,G12,G13,G14,G15,G16,G17,G18,G19,G20,G21,G22,G23,G24,G25,G26,G27,G28,G29,G30,GEnd

GameControlMaps:
; An array of NumGames*12 words giving the game's control mapping
; ie. game n's controls are at GameControlMaps+24*n
.define btn0 1<<$0
.define btn1 1<<$1
.define btn2 1<<$2
.define btn3 1<<$3
.define btn4 1<<$4
.define btn5 1<<$5
.define btn6 1<<$6
.define btn7 1<<$7
.define btn8 1<<$8
.define btn9 1<<$9
.define btnA 1<<$a
.define btnB 1<<$b
.define btnC 1<<$c
.define btnD 1<<$d
.define btnE 1<<$e
.define btnF 1<<$f

; Chip-8 keypad:
;   1 2 3 C
;   4 5 6 D
;   7 8 9 E
;   A 0 B F
; My control definitions:
; P1U,P1D,P1L,P1R,P11,P12,P2U,P2D,P2L,P2R,P21,P22
; where each entry is the Chip-8 button that the SMS button corresponds to
.dw btn3,btn6,btn7,btn8,btnF,btn1,0   ,0   ,0   ,0   ,0   ,0    ; Blinky
.dw 0   ,0   ,0   ,0   ,btn5,0   ,0   ,0   ,0   ,0   ,0   ,0    ; Blitz
.dw 0   ,0   ,btn4,btn6,0   ,0   ,0   ,0   ,0   ,0   ,0   ,0    ; Breakout
.dw 0   ,0   ,btn4,btn6,0   ,0   ,0   ,0   ,0   ,0   ,0   ,0    ; Brix
.dw btn2,btn8,btn4,btn6,btnF,0   ,0   ,0   ,0   ,0   ,0   ,0    ; Cave
.dw 0   ,0   ,0   ,0   ,0   ,0   ,0   ,0   ,0   ,0   ,0   ,0    ; Chip-8 Logo
.dw 0   ,0   ,btn4,btn6,btn5,0   ,0   ,0   ,btn4,btn6,btn5,0    ; Connect 4
.dw 0   ,0   ,btn4,btn6,0   ,0   ,0   ,0   ,0   ,0   ,0   ,0    ; Filter
.dw 0   ,0   ,0   ,0   ,btn5,btn0,0   ,0   ,0   ,0   ,0   ,0    ; Guess - 1=yes, 2=no
.dw btn2,btn8,btn4,btn6,btn5,0   ,0   ,0   ,0   ,0   ,0   ,0    ; Hidden
.dw 0   ,0   ,0   ,0   ,0   ,0   ,0   ,0   ,0   ,0   ,0   ,0    ; IBM Logo
.dw btn2,btn8,btn4,btn6,btn0,0   ,0   ,0   ,0   ,0   ,0   ,0    ; Kaleid
.dw 0   ,0   ,0   ,0   ,0   ,0   ,0   ,0   ,0   ,0   ,0   ,0    ; Maze
.dw btn5,btn7,btn4,btn8,0   ,0   ,0   ,0   ,0   ,0   ,0   ,0    ; Merlin
.dw 0   ,0   ,0   ,0   ,btn8,btn8,0   ,0   ,0   ,0   ,0   ,0    ; Missile
.dw btn1,btn4,0   ,0   ,0   ,0   ,btnC,btnD,0   ,0   ,0   ,0    ; Pong
.dw btn1,btn4,0   ,0   ,0   ,0   ,btnC,btnD,0   ,0   ,0   ,0    ; Pong 2
.dw btn8,btn2,btn4,btn6,0   ,0   ,0   ,0   ,0   ,0   ,0   ,0    ; Puzzle
.dw 0   ,0   ,0   ,0   ,btnF,0   ,0   ,0   ,0   ,0   ,0   ,0    ; Rocket
.dw 0   ,0   ,btn4,btn6,btn5,0   ,0   ,0   ,0   ,0   ,0   ,0    ; Space Invaders
.dw btn1,btn4,0   ,0   ,0   ,0   ,0   ,0   ,0   ,0   ,0   ,0    ; Squash
.dw btn3,btn6,btn7,btn8,btnE+btnB,btnF,0,0 ,0   ,0   ,0   ,0    ; Syzygy - 1=no border/count score, 2=with border
.dw btn8,btn2,btn4,btn6,btn5,0   ,0   ,0   ,0   ,0   ,0   ,0    ; Tank
.dw btn2,btn8,btn4,btn6,btnF,0   ,0   ,0   ,0   ,0   ,0   ,0    ; Tapeworm
.dw 0   ,btn7,btn5,btn6,btn4,0   ,0   ,0   ,0   ,0   ,0   ,0    ; Tetris
.dw btn5,0   ,btn4,btn6,0   ,0   ,0   ,0   ,0   ,0   ,0   ,0    ; UFO
.dw btn7,btnA,btn1,btn2,0   ,0   ,btnC,btnD,btnB,btnF,0   ,0    ; Vers
.dw btn1,btn4,0   ,0   ,btn7,0   ,0   ,0   ,0   ,0   ,0   ,0    ; VBrix
.dw btn1,btn4,0   ,0   ,0   ,0   ,0   ,0   ,0   ,0   ,0   ,0    ; Wall
.dw 0   ,0   ,btn4,btn6,0   ,0   ,0   ,0   ,0   ,0   ,0   ,0    ; Wipeoff
.undef btn0,btn1,btn2,btn3,btn4,btn5,btn6,btn7,btn8,btn9,btnA,btnB,btnC,btnD,btnE,btnF

.incdir ""
.ends

.section "Text scroller" SEMIFREE
.enum C8Game    ; safe place to put it
ScrollValue     db  ; VScroll number
NextLine        db  ; Number of line to draw to
EndOfFile       db  ; 1 = don't scroll any more
.ende

TextScroller:
    ld hl,_TextData

    xor a  ; vscroll
    ld (ScrollValue),a
    ld (EndOfFile),a
    ld a,25
    ld (NextLine),a

    ld c,1
    _MainScrollLoop:
    call WaitForVBlankNoInt

    ; Check input
    in a,($dc)
    bit 1,a ; Down
    jp z,_ScrollDown

    bit 4,a ; Button 1
    jp z,_EndScroll

    bit 0,a ; Up
    jp z,_MainScrollLoop    ; no auto-scroll

    dec c
    jp nz,_MainScrollLoop
    ld c,5
    jp _ScrollDown

    jp _MainScrollLoop

_ScrollDown:
    ld a,(EndOfFile)
    or a
    jp nz,_MainScrollLoop

    ld a,(ScrollValue)
    inc a
    cp 224
    jp nz,+
    xor a
    +:
    ld (ScrollValue),a
    out ($bf),a
    ld a,$89
    out ($bf),a

    ld a,(ScrollValue)
    and 7
    jp nz,_MainScrollLoop

    ld a,(NextLine)    ; increment other line counter
    inc a
    cp 28
    jp nz,+
    xor a
    +:
    ld (NextLine),a

    ld b,a
    call _WriteLine
    jp _MainScrollLoop

_WriteLine:
; draw up to 32 chars to screen row b
; read from hl, and modify it
; fill with blanks after LF (10)
    push af
    push bc
      push hl
        ld h,b
        ld l,0  ; hl = b*256
        srl h   ; >>2 so hl=b*64
        rr l
        srl h
        rr l
        ld bc,NameTableAddress
        add hl,bc ; add NameTableAddress, now hl = VRAM location for line

        ; add 2 to avoid the leftmost column
        inc hl
        inc hl

        call VRAMToHL
      pop hl

      ld c,32+2
-:    ; Read char
      ld a,(hl)
      or a
      jp nz,+
      ld a,1
      ld (EndOfFile),a
+:    cp 10
      jp z,_LF  ; end at LF
      sub $20
      jp c,_NextChar  ; skip control chars
      out ($be),a
      push ix ; VDP delay
      pop ix
      xor a
      out ($be),a
_NextChar:
      inc hl
      dec c
      jp nz,-

    pop bc
    pop af
    ret

_LF:    ; output blanks to the end of the line. c = number left to do. If c=32 then don't?
      ld a,c
      cp 32
      jp z,_NextChar

      xor a
      sla c   ; c*2
-:
      push ix
      pop ix
      out ($be),a
      dec c
      jp nz,-
      ld c,1
      jp _NextChar

_EndScroll:
    call TurnOffScreen
    ; reset VScroll
    xor a
    out ($bf),a
    ld a,$89
    out ($bf),a
    ; wait for button not to be pressed
-:  in a,($dc)
    and %00111111
    cp  %00111111
    jp nz,-
    jp TitleScreen


SDSCNotes:
_TextData:
.incbin "info.txt"
.db 0   ; null terminator

.ends

;==============================================================
; Clear VRAM
;==============================================================
.section "Clear VRAM" SEMIFREE
ClearVRAM:
    push hl
    push bc
    push af
      ld hl,0
      call VRAMToHL
      ld bc, 16*1024  ; Counter for 16KB of VRAM
-:    xor a           ; Value to write
      out ($BE),a     ; Output to VRAM address, which is auto-incremented after each write
      dec bc
      ld a,b
      or c
      jp nz,-
    pop af
    pop bc
    pop hl
    ret
.ends

;==============================================================
; Palette loader
; Parameters:
; hl = location
; b  = number of values to write
; c  = palette index to start at (<32)
;==============================================================
.section "Palette loader" FREE
LoadPalette:
    push af
    push bc
    push hl
      ld a,c
      out ($bf),a     ; Palette index
      ld a,$c0
      out ($bf),a     ; Palette write identifier
      ld c,$be
      otir            ; Output b bytes starting at hl to port c
    pop hl
    pop bc
    pop af
    ret
.ends

.section "Turn off screen" FREE
TurnOffScreen:
    push af
      ld a,%10000100  ; 28 line mode
      out ($bf),a
      ld a,$81
      out ($bf),a
    pop af
    ret
.ends

.section "Clear name table" FREE
ClearNameTable:
    push hl
    push bc
    push af
      ld hl,NameTableAddress
      call VRAMToHL
      ld bc,$700      ; for unstretched screens only!
-:    xor a           ; Value to write
      out ($BE),a ; Output to VRAM address, which is auto-incremented after each write
      dec bc
      ld a,b
      or c
      jp nz,-
    pop af
    pop bc
    pop hl
    ret
.ends

.section "Wait for VBlank without interrupts" FREE
WaitForVBlankNoInt:
    push bc
    push af
-:    call GetVCount
      cp $c1
      jp nz,-
    pop af
    pop bc
    ret
.ends

;==============================================================
; Tilemap loader
; Parameters:
; b  = width  (tiles)
; c  = height (tiles)
; ix = location of tile number data (bytes)
; iy = name table address of top-left tile
; Sets all tile flags to zero.
;==============================================================
.section "Draw image" FREE
DrawTilemap:
    push af
    push bc     ; Width, height
    push de     ; Width, height counters
    push hl     ; h = high byte
    push ix     ; ROM location
    push iy     ; VRAM location
_DrawRow:
      call VRAMToIY     ; Move to the right place
      ld d,b                  ; no. of tiles to loop through per row
_DrawTile:
      ld a,(ix+0)
      inc ix
      out ($be),a
      ld a,(ix+0)
      inc ix
      out ($be),a
      dec d
      jp nz,_DrawTile

      ld de,64                ; Move name table address
      add iy,de

      dec c
      jp nz,_DrawRow
    pop iy
    pop ix
    pop hl
    pop de
    pop bc
    pop af
    ret
.ends

;==============================================================
; Write ASCII text pointed to by hl to VRAM
; Stops when it finds a null byte, skips control characters,
; understands \n
; Pass name table address in iy, it will be modified
;==============================================================
.section "Write ASCII" FREE
VRAMToIY:
    push hl
      push iy
      pop hl
      call VRAMToHL
    pop hl
    ret

WriteASCII:
    push af
    push bc
    push hl
      call VRAMToIY
_WriteTilesLoop:
      ld a,(hl)  ; Value to write
      cp $00    ; compare a with $00, set z flag if they match
      jp z,_WriteTilesLoopEnd  ; if so, it's the string end so stop writing it
      cp 10    ; Check for LF
      jp z,_NewLine
      sub $20 ; Convert to tile index
      jp c,_SkipControlChar
      out ($BE),a  ; Output to VRAM address, which is auto-incremented after each write
      ld a,%00000000
      push hl
      pop hl  ; delay
      out ($BE),a
_SkipControlChar:
      inc hl
      jp _WriteTilesLoop
_NewLine:
      ; Go to the next line, ie. next multiple of 32=$20
      push hl
        push iy
        pop hl
        ld bc,64
        add hl,bc
        push hl
        pop iy
        call VRAMToIY
      pop hl
_NoNewLine:
      inc hl
      jp _WriteTilesLoop
_WriteTilesLoopEnd:
    pop hl
    pop bc
    pop af
    ret
.ends

;==============================================================
; Set VRAM address to hl
;==============================================================
.section "VRAM address to hl" FREE
VRAMToHL:
    push af
      ld a,l
      out ($BF),a
      ld a,h
      or $40
      out ($BF),a
    pop af
    ret
.ends

.section "Get VCount" FREE
;==============================================================
; V Counter reader
; Waits for 2 consecuitive identical values (to avoid garbage)
; Returns in a *and* b
;==============================================================
GetVCount:  ; returns scanline counter in a and b
    in a,($7e)  ; get value
-:  ld b,a      ; store it
    in a,($7e)  ; and again
    cp b        ; Is it the same?
    jr nz,-     ; If not, repeat
    ret         ; If so, return it in a (and b)
.ends
