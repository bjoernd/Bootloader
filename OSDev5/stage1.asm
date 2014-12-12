; boot loader from http://www.brokenthorn.com/Resources/OSDev3.html
org 0x0					; start address
bits 16					; 16 bit mode

; Entry point: skip the OEM param block
start:
    jmp loader

;*************************************************;
;	OEM Parameter block
;
; This block describes the FAT12 file system that
; our disk image contains _behind_ the boot sector.
;*************************************************;

TIMES 0Bh-$+start DB 0

bpbBytesPerSector:  	DW 512
bpbSectorsPerCluster: 	DB 1
bpbReservedSectors: 	DW 1
bpbNumberOfFATs: 	    DB 2
bpbRootEntries: 	    DW 224
bpbTotalSectors: 	    DW 2880
bpbMedia: 	            DB 0xF8
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
bootmsg db "Welcome to the BD BootLoader",0
msgFailure db "Loading failed", 0
msgCRLF db 0x0d, 0x0a, 0
msgProgress db ".", 0

absoluteSector db 0
absoluteHead   db 0
absoluteTrack  db 0

datasector     dw 0
cluster        dw 0

ImageName      db "STAGE2  IMG"

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

; void read_sectors(CX = number of sectors,
;					AX = start sector,
;					ES:BX = buffer to copy to)
read_sectors:
	.main
		mov di, 5			; floppies may fail -> 5 retries
	.sectorloop
		push ax
		push bx
		push cx
		call lba2chs
		mov ah, 0x2			; BIOS: read sector
		mov al, 0x1			; read single sector
		mov ch, byte [absoluteTrack]	; track
		mov cl, byte [absoluteSector]	; sector
		mov dh, byte [absoluteHead]     ; head
		mov dl, byte [bsDriveNumber]    ; drive ID
		int 0x13
		jnc .success
		; failure...
		xor ax, ax						; BIOS: reset disk
		int 0x13
		dec di
		pop cx
		pop bx
		pop ax
		jnz .sectorloop
		int 0x18						; "No disk found" interrupt
	.success
		mov si, msgProgress
		call print_string
		pop cx
		pop bx
		pop ax
		add bx, word [bpbBytesPerSector]	; loop tail: move target pointer by sector size
		inc ax								; move to next sector
		loop .main							; loop as long as CX > 0
		ret

;************************************************;
; Convert CHS to LBA
; LBA = (cluster - 2) * sectors per cluster
;************************************************;

cluster2lba:
          sub     ax, 0x0002                          ; zero base cluster number
          xor     cx, cx
          mov     cl, BYTE [bpbSectorsPerCluster]     ; convert byte to word
          mul     cx
          add     ax, WORD [datasector]               ; base data sector
          ret
     
;************************************************;
; Convert LBA to CHS
; AX=>LBA Address to convert
;
; absolute sector = (logical sector / sectors per track) + 1
; absolute head   = (logical sector / sectors per track) MOD number of heads
; absolute track  = logical sector / (sectors per track * number of heads)
;
;************************************************;

lba2chs:
          xor     dx, dx                              ; prepare dx:ax for operation
          div     WORD [bpbSectorsPerTrack]           ; calculate
          inc     dl                                  ; adjust for sector 0
          mov     BYTE [absoluteSector], dl
          xor     dx, dx                              ; prepare dx:ax for operation
          div     WORD [bpbHeadsPerCylinder]          ; calculate
          mov     BYTE [absoluteHead], dl
          mov     BYTE [absoluteTrack], al
          ret

loader:
	cli
	; adjust segment registers to segment at 0x07c0
	mov ax, 0x07c0
	mov ds, ax
	mov es, ax
	mov fs, ax
	mov gs, ax
	
	; we want a stack now (in the 1st page)
	mov ax, 0x0000
	mov ss, ax
	mov sp, 0xFFFF
	sti

	; load string ptr to SI
	mov si, bootmsg
	; print()
	call print_string
	mov si, msgCRLF
	call print_string

load_root:
	; The fun begins ... load the FAT12 root directory
	; 1) Compute size of the root directory -> CX
	xor cx, cx
	xor dx, dx
	mov ax, 32 					 ; 32 bytes per directory entry
	mul word [bpbRootEntries]    ; compute number of entries in root dir
	div word [bpbBytesPerSector] ; compute sectors used by root dir
	xchg ax, cx 				 ; store in CX
	
	; 2) Compute location of the root directory in the FAT -> AX
	mov al, byte [bpbNumberOfFATs]
	mul word [bpbSectorsPerFAT]			; number of FAT sectors
	add ax, word [bpbReservedSectors]	; + number of reserved sectors
	mov word [datasector], ax
	add word [datasector], cx			; *datasector = ax + cx
	
	mov bx, 0x200						; load root dir to 0x200
	call read_sectors

	; Look for STAGE2.SYS now -> this runs over the 32byte large root directory
	; entries and checks the first 11 bytes (8 + 3) for the proper file name
	mov cx, word [bpbRootEntries] ; iterate over entries in FAT root
	mov di, 0x0200				  ; we just loaded the root dir to 0x200
  .loop
  	push cx
	mov cx, 0xb					  ; strcmp(*idx, imagename, 11)
	mov si, ImageName
	push di
	rep cmpsb
	pop di
	je load_fat					  ; load success -> read fat
	pop cx
	add di, 32					  ; try next entry (FAT entries have 32 bytes each)
	loop .loop
	jmp fail
	
load_fat:
	; now we load the FAT itself so that we can figure out in which 
	; *cluster* the image file lies.
	mov dx, word [di + 0x1a]	  ; get cluster from directory entry
	mov word [cluster], dx		  ; store cluster#
	
	; compute size of FAT -> CX
	xor ax, ax
	mov al, byte [bpbNumberOfFATs]
	mul word [bpbSectorsPerFAT]
	mov cx, ax
	
	; compute location of first FAT -> AX
	mov ax, word [bpbReservedSectors]
	; read FAT to memory (7c00 : 0x200)
	; -> this overwrites the root dir copy, but we don't need it anymore
	mov bx, 0x200
	call read_sectors
	
	; es:bx := 0x50:0x00 -> target buffer for the stage2 file
	mov ax, 0x50
	mov es, ax
	mov bx, 0x0000
	push bx
	
	; load the stage2 file
load_image:
	; FAT details: We got the first cluster # from the directory entry.
	; The remaining entries are within the FAT and we follow a chain of
	; entries here until we find the END-OF-CHAIN marker 0xFF0.
	mov ax, word [cluster]
	pop bx
	; Cluster # is local to the file system. We need to translate this into
	; a logical block address, that is the linear sector number within the disk.
	call cluster2lba
	xor cx, cx
	mov cl, byte [bpbSectorsPerCluster]
	call read_sectors
	push bx
	
	; Now we get the next (12bit !) entry from the FAT. This requires:
	; 1) load base address...
	mov ax, word [cluster]
	mov cx, ax
	mov dx, ax
	; 2) ... and add half of it to get to the middle of the next cluster
	shr dx, 1
	add cx, dx
	mov bx, 0x0200
	add bx, cx
	; 3) ... dereference the 16bit word containing those 12 bits
	mov dx, word[bx]
	; 4) ... and depending on odd or even cluster# use the ...
	test ax, 1
	jnz .ODD_CLUSTER

.EVEN_CLUSTER:
    ; 4a) ... lowest 12 bits ...
	and dx, 0000111111111111b
	jmp .load_done

.ODD_CLUSTER
    ; 4b) ... or the highest 12 bits.
	shr dx, 4
	
.load_done:
    ; We now loaded the sector and dx contains the next cluster number to load.
	mov word[cluster], dx
	; If it is 0xFF0, we found the end marker.
	cmp dx, 0xff0
	jb load_image

JUMP_TO_STAGE2:
    ; image is loaded to 0x50:0x0000
	mov si, msgCRLF
	call print_string
	; so we jump there
	push word 0x50
	push word 0x00
	retf
	
fail:
	mov si, msgFailure
	call print_string
	int 0x16				; wait for keypress
	int 0x19				; reboot

times 510 - ($-$$) db 0		; fill with 0 bytes
dw 0xAA55					; boot signature
