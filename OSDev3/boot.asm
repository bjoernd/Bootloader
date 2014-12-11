; boot loader from http://www.brokenthorn.com/Resources/OSDev3.html
org 0x7c00					; start address
bits 16						; 16 bit mode

start:
	cli
	hlt

times 510 - ($-$$) db 0		; fill with 0 bytes
dw 0xAA55					; boot signature
