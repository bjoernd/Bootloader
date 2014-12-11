; boot loader from http://www.brokenthorn.com/Resources/OSDev3.html
org 0x7c00					; start address
bits 16						; 16 bit mode

; Entry point: skip the OEM param block
start:
    jmp loader

;*************************************************;
;	OEM Parameter block
;*************************************************;

TIMES 0Bh-$+start DB 0

bpbBytesPerSector:  	DW 512
bpbSectorsPerCluster: 	DB 1
bpbReservedSectors: 	DW 1
bpbNumberOfFATs: 	    DB 2
bpbRootEntries: 	    DW 224
bpbTotalSectors: 	    DW 2880
bpbMedia: 	            DB 0xF0
bpbSectorsPerFAT: 	    DW 9
bpbSectorsPerTrack: 	DW 18
bpbHeadsPerCylinder: 	DW 2
bpbHiddenSectors: 	    DD 0
bpbTotalSectorsBig:     DD 0
bsDriveNumber: 	        DB 0
bsUnused: 	            DB 0
bsExtBootSignature: 	DB 0x29
bsSerialNumber:	        DD 0xa0a1a2a3
bsVolumeLabel: 	        DB "MOS FLOPPY "
bsFileSystem: 	        DB "FAT12   "

; char *bootmsg = "...";
bootmsg db "Welcome to BDB, the BD BootLoader!",0

; print_string(char const *msg)
;   -> print zero-terminated string passed through SI
print_string:
	lodsb				; load string byte (AL = *(SI++))
	or al, al
	jz print_done		; 0 reached -> return
	mov ah, 0xe			; select 'print char'
	int 0x10			; BIOS Interrupt
	jmp print_string	; next byte
print_done:
	ret

loader:
	; first, set segment regs to 0
	xor bx, bx
	mov es, bx
	mov ds, bx
	; load string ptr to SI
	mov si, bootmsg
	; print()
	call print_string
	; stop CPU
	cli
	hlt

times 510 - ($-$$) db 0		; fill with 0 bytes
dw 0xAA55					; boot signature
