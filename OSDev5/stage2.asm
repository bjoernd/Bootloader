org 0x0
bits 16

jmp main

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

main:
	cli
	push cs
	pop ds
	
	mov si, msg
	call print_string
	
	cli
	hlt

msg db "[Stage 2] Preparing to load the kernel..."
