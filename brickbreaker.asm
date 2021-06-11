[org 0x0100]
jmp start
marks:db 0xf,0xa,0x5
brickn:db 0x43
scoren: dw 0x0000
lives:db 'lives:'
livesn:dw 0x3
status:dw 'GAME OVER!'
won:dw 'YOU WON!'
score:db 'score:'
bar:dw 0xe62,0xe7e
ball:dw 0xdd0
movement:dw 0x2
oldisr:dd 0
oldisr1:dd 0
bouncen:dw 0
bounce:dw 'BONUS!! 50'
delay:
	push cx
	mov cx,0xffff
	d1:
	
	dec cx
	cmp cx,0
	jne d1
	mov cx,0xffff
	d2:
	
	dec cx
	cmp cx,0
	jne d2
	
	pop cx
	ret

clrscr:
	 push es
	 push ax 
	 push di 
	mov  ax, 0xb800               
	mov  es, ax             ; point es to video base               
	mov  di, 0              ; point di to top left column 
	nextloc:      
	mov  word [es:di], 0x0720 ; clear next char on screen               
	add  di, 2              ; move to next screen location               
	cmp  di, 4000           ; has the whole screen cleared               
	jne  nextloc            ; if no clear next position 
	pop  di              
	pop  ax               
	pop  es               
	ret 

printnum: 
	 push bp
	 mov bp, sp
	 push es
	 push ax
	 push bx
	 push cx
	 push dx
	 mov ax, 0xb800
	 mov es, ax ; point es to video base
	 mov ax, [bp+4] ; load number in ax
	 mov bx, 10 ; use base 10 for division
	 mov cx, 0 ; initialize count of digits
	nextdigit: mov dx, 0 ; zero upper half of dividend
	 div bx ; divide by 10
	 add dl, 0x30 ; convert digit into ascii value
	 push dx ; save ascii value on stack
	 inc cx ; increment count of values
	 cmp ax, 0 ; is the quotient zero
	 jnz nextdigit ; if no divide it again
	nextpos: pop dx ; remove a digit from the stack
	 mov dh, 0x07 ; use normal attribute
	 mov [es:di], dx ; print char on screen
	 add di, 2 ; move to next screen location
	 loop nextpos ; repeat for all digits on stack
	 
	 pop dx
	 pop cx
	 pop bx
	 pop ax
	 pop es
	 pop bp
	 ret 2 

print_boarder:
	push bp
	mov bp,sp
	push 0xb800
	pop es
	mov di,0
	mov ax,0x03db
	b1:
		mov word[es:di],ax
		add di,2
		cmp di,160
		jne b1
	b2:
		mov word[es:di],ax
		add di,100
		mov word[es:di],ax
		add di,58
		mov word[es:di],ax
		add di,2
		cmp di,3840
		jl b2
	b3:
		mov word[es:di],ax
		add di,2
		cmp di,4000
		jne b3
	pop bp
	ret

print_marks:
push bp
	mov bp,sp
	push 0xb800
	pop es
	mov di,0
	mov di,110
	add di,320
	mov word[es:di-6],0x18b2
	mov word[es:di-4],0x18b2
	
	mov ah,0
	mov al,byte[marks]
	push ax
	call printnum
	add di,156
	mov word[es:di-6],0x12b2
	mov word[es:di-4],0x12b2
	mov ah,0
	mov al,byte[marks+1]
	push ax
	call printnum
	add di,156
	mov word[es:di-6],0x14b2
	mov word[es:di-4],0x14b2
	mov ah,0
	mov al,byte[marks+2]
	push ax
	call printnum
	pop bp
	ret

print_score:
     mov  ah, 0x13           ; service 13 - print string
	 mov  al, 1              ; subservice 01 – update cursor 
	 mov  bh, 0              ; output on page 0               
	 mov  bl, 7              ; normal attrib               
	 mov  dx, 0x0635         ; row 10 column 3               
	 mov  cx, 6             ; length of string               
	 push cs               
	 pop  es                 ; segment of string       
	 mov  bp, score        ; offset of string          
	 int  0x10
	 mov di,1080
	mov ax,word[scoren]
	push ax
	call printnum
	ret

print_lives1:
	mov di,1080
	add di,320
	push 0xb800
	pop es
	mov ax,word[cs:livesn]
	push ax
	call printnum
	ret
print_lives:
     mov  ah, 0x13           ; service 13 - print string
	 mov  al, 1              ; subservice 01 – update cursor 
	 mov  bh, 0              ; output on page 0               
	 mov  bl, 7              ; normal attrib               
	 mov  dx, 0x0835         ; row 10 column 3               
	 mov  cx, 6             ; length of string               
	 push cs               
	 pop  es                 ; segment of string       
	 mov  bp, lives        ; offset of string          
	 int  0x10

	add di,318
	push 0xb800
	pop es
	mov ax,word[cs:livesn]
	push ax
	call printnum
	ret


print_status:
	MOV BX,0
	cmp word[livesn],0
	jne continue
	mov  ah, 0x13           ; service 13 - print string
	 mov  al, 1              ; subservice 01 – update cursor 
	 mov  bh, 0              ; output on page 0               
	 mov  bl, 12              ; normal attrib               
	 mov  dx, 0x0c20         ; row 10 column 3               
	 mov  cx, 10             ; length of string               
	 push cs               
	 pop  es                 ; segment of string       
	 mov  bp, status        ; offset of string          
	 int  0x10
continue:
	ret

kbisr:        
	push ax
	push es 
	mov  ax, 0xb800
	mov  es, ax             ; point es to video memory
	push cs
	pop ds
	in   al, 0x60           ; read a char from keyboard port
	cmp  al,4Bh          ; is the key left
	jne  nextcmp            ; no, try next comparison
	cmp word[cs:bar],3684
	jl nomatch
	call clear_bar
	sub word[cs:bar],2   
	sub word[cs:bar+2],2
	call print_bar 
	jmp  nomatch            ; leave interrupt routine 
 
nextcmp:
	cmp  al, 4Dh           ; is the key right
	jne  nomatch            ; no, leave interrupt routine 
	cmp word[cs:bar],3750
	jge nomatch
	call clear_bar
	add word[cs:bar],4   
	add word[cs:bar+2],4
	call print_bar
nomatch:
	mov  al, 0x20
	out  0x20, al           ; send EOI to PIC 
	pop  es
	pop  ax
	iret 

leftwall:
	mov cx,2
	l1:
	cmp word[cs:ball],cx
	je l2
	add cx,160
	cmp cx,4000
	jle l1
	jg leftexit
	l2:
	cmp word[cs:movement],1
	jne l3
	mov word[cs:movement],2
	jmp leftexit
	l3:
	cmp word[cs:movement],3
	jne leftexit
	mov word[cs:movement],4
	leftexit:
	ret
rightwall:
	mov cx,98
	r1:
	cmp word[cs:ball],cx
	je r2
	add cx,160
	cmp cx,3840
	jle r1
	jg rightexit
	r2:
	cmp word[cs:movement],2
	jne r3
	mov word[cs:movement],1
	jmp rightexit
	r3:
	cmp word[cs:movement],4
	jne rightexit
	mov word[cs:movement],3
	rightexit:
	ret
top:
	mov cx,320
	cmp word[cs:ball],cx
	jg texit
	
	t2:
	cmp word[cs:movement],1
	jne t3
	mov word[cs:movement],3
	jmp texit
	t3:
	cmp word[cs:movement],2
	jne texit
	mov word[cs:movement],4
	texit:
	ret
collidebar:
	push ax
	mov cx,0
	mov ax,word[cs:ball]
	add ax,160
	cmp word[cs:bar],ax
	jg barexit
	cmp word[cs:bar+2],ax
	jl barexit
	mov cx,1
	cmp word[cs:movement],4
	jne c1
	mov word[cs:movement],2
	jmp barexit
	c1:
	cmp word[cs:movement],3
	jne barexit
	mov word[cs:movement],1
barexit:
	pop ax
	ret

bottom:
	call collidebar
	cmp cx,1
	je bexit
	mov cx,3640
	cmp word[cs:ball],cx
	jle bexit
	call beep
	call beep
	call beep
	call delay
	dec word[cs:livesn]
	call print_lives1
	call clear_ball
	call clear_bar
	mov word[bar],0xe62
	mov word[bar+2],0xe7e
	mov word[ball],0xdd0
	mov word[movement],0x2
	call print_ball
	call print_bar 
	bexit:
	ret
collidebrick:
	push bp
	mov bp,sp
	mov ax,0xb800
	mov es,ax
	mov di, word[cs:ball]
	cmp word[cs:movement],1
	jne cmp1
	sub di,162
	jmp position
	
cmp1:
	cmp word[cs:movement],2
	jne cmp2
	sub di,158
	jmp position
cmp2: 
	cmp word[cs:movement],3
	jne cmp3
	add di,158
	jmp position
cmp3:
	cmp word[cs:movement],4
	add di,162
	jmp position
position:
	cmp di,320
	jnl colour
	cmp di,1600
	jng colour
	finish:
	pop bp
	ret
colour: 
	mov ax, word[es:di]
	cmp ah,0x18
	jne nc1
	add word[cs:scoren],0xf
	jmp f1
nc1:
	cmp ah,0x12
	jne nc2
	add word[cs:scoren],0xa
	jmp f1
nc2:
	cmp ah,0x14
	jne finish
	add word[cs:scoren],0x5
	f1:
	call beep
	call beep
	call beep
	mov word[es:di],0x0820
	mov word[es:di+2],0x0820
	mov word[es:di-2],0x0820
	dec byte[brickn]
	cmp word[cs:movement],1
	jne w1
	mov word[cs:movement],3
	sub word[ball],162
	jmp w4
	w1:
	cmp word[cs:movement],2
	jne w2
	mov word[cs:movement],4
	sub word[ball],160
	jmp w4
	w2:
	cmp word[cs:movement],3
	jne w3
	mov word[cs:movement],1
	add word[ball],160
	jmp w4
	w3:
	cmp word[cs:movement],4
	jne w4
	mov word[cs:movement],2
	add word[ball],162
	 w4: call print_score
	 pop bp
	ret


move_ball:
	push bp
	mov bp,sp
	mov ax,0
	call leftwall
	call rightwall
	call top
	call bottom
	call clear_ball
	call collidebrick
	cmp word[cs:movement],1
	jne m1
	sub word[ball],162
	m1:
	cmp word[cs:movement],2
	jne m2
	sub word[ball],158
	m2:
	cmp word[cs:movement],3
	jne m3
	add word[ball],158
	m3:
	cmp word[cs:movement],4
	jne m4
	add word[ball],162
	m4:
	call print_ball
	pop bp
	ret

clear_ball:
	push bp
	mov bp,sp
	push 0xb800
	pop es
	mov di,word[cs:ball]
	mov ax,0x0720
	mov word[es:di],ax
	pop bp
	ret

clear_bar:
	push bp
	mov bp,sp
	push 0xb800
	pop es
	mov di,word[cs:bar]
	bar2:
	mov ax,0x0720
	mov word[es:di],ax
	add di,2
	cmp di,word[cs:bar+2]
	jne bar2
	pop bp
	ret

print_bar:
	push bp
	mov bp,sp
	push 0xb800
	pop es
	mov di,word[bar]
	bar1:
	mov ax,0x07df
	mov word[es:di],ax
	add di,2
	cmp di,word[bar+2]
	jne bar1
	pop bp
	ret

print_ball:
	push bp
	mov bp,sp
	push 0xb800
	pop es
	mov di,word[ball]
	mov ax,0x076f
	mov word[es:di],ax
	pop bp
	ret

print_bricks:
	push bp
	mov bp,sp
	push 0xb800
	pop es
	mov cx,1
	mov di,160
	
	repeate:
	mov ax,160
	mul cx
	 add ax,80
	next:
	 
	 cmp di,ax
	 jnl nextline
	 add di,12
	 mov word[es:di],0x18b2
	 mov word[es:di+2],0x18b2
	jmp next
	nextline:
	inc cx
	add di,70
	mov ax,160
	mul cx
	 add ax,80
	next1:
	 
	 cmp di,ax
	 jnl nextline1
	 add di,12
	 mov word[es:di],0x12b2
	 mov word[es:di+2],0x12b2
	jmp next1
	nextline1:
	inc cx
	add di,70
	mov ax,160
	mul cx
	 add ax,80
	next2:
	 
	 cmp di,ax
	 jnl nextline2
	 add di,12
	 mov word[es:di],0x14b2
	 mov word[es:di+2],0x14b2
	jmp next2
	nextline2:
	inc cx
	add di,70
	cmp cx, 10
	jne repeate
	pop bp
	ret

statuscheck:
	cmp word[cs:livesn],0x000
	jg gamecontinue
	jle break
	break:
	
	xor  ax, ax
	 mov  es, ax
	 mov ax, [cs:oldisr]        ; read old offset in ax
	 mov bx, [cs:oldisr+2]      ; read old segment in bx
	 mov [es:9*4], ax        ; restore old offset from ax
	 mov [es:9*4+2], bx      ; restore old segment from bx
	 mov ax, [cs:oldisr1]        ; read old offset in ax
	 mov bx, [cs:oldisr1+2]      ; read old segment in bx
	 mov [es:8*4], ax        ; restore old offset from ax
	 mov [es:8*4+2], bx      ; restore old segment from bx
	 sti                     ; enable interrupts 
	 call clrscr
	 call print_status
	mov ax, 0x4c00          ; terminate program
	int 0x21 
	gamecontinue:
	ret

beep:
	push ax
	push bx
	push cx
	push dx
	push si
	 
	 in al,61h
	 push ax
	 mov bx,6818
	 mov al,6Bh
	 out 43h,al
	 mov ax,bx
	 out 24h,al
	 mov al,ah
	 out 42h,al
	 in al,61h
	 or al,3h
	 out 61h,al
	 mov cx,03h
	 mov cx,0d04h
	 mov ax,86h
	 int 15h
	 pop ax
	 out 61h,al
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	ret

print_bounce:
	mov  ah, 0x13           ; service 13 - print string
	 mov  al, 1              ; subservice 01 – update cursor 
	 mov  bh, 0              ; output on page 0               
	 mov  bl, 12              ; normal attrib               
	 mov  dx, 0x1020         ; row 10 column 3               
	 mov  cx, 10             ; length of string               
	 push cs               
	 pop  es                 ; segment of string       
	 mov  bp, bounce        ; offset of string          
	 int  0x10
	 ret
print_won:
	 mov  ah, 0x13           ; service 13 - print string
	 mov  al, 1              ; subservice 01 – update cursor 
	 mov  bh, 0              ; output on page 0               
	 mov  bl, 12              ; normal attrib               
	 mov  dx, 0x1038         ; row 10 column 3               
	 mov  cx, 8             ; length of string               
	 push cs               
	 pop  es                 ; segment of string       
	 mov  bp, won        ; offset of string          
	 int  0x10
	ret

timer:
	push ax
	push es 
	mov  ax, 0xb800
	mov  es, ax
	cmp byte[brickn],0x0000
	jl timer2
	inc word[bouncen]
	CALL move_ball
	call statuscheck
	jmp timer3
	timer2:
	cmp word[bouncen],2184
	jg timer1
	add word[scoren],50
	call clrscr
	call print_bounce
	timer1:	
	xor  ax, ax
	 mov  es, ax
	 mov ax, [cs:oldisr]        ; read old offset in ax
	 mov bx, [cs:oldisr+2]      ; read old segment in bx
	 mov [es:9*4], ax        ; restore old offset from ax
	 mov [es:9*4+2], bx      ; restore old segment from bx
	 mov ax, [cs:oldisr1]        ; read old offset in ax
	 mov bx, [cs:oldisr1+2]      ; read old segment in bx
	 mov [es:8*4], ax        ; restore old offset from ax
	 mov [es:8*4+2], bx      ; restore old segment from bx
	 sti                     ; enable interrupts 
	 
	 call print_won
	mov ax, 0x4c00          ; terminate program
	int 0x21 
	timer3:
	mov  al, 0x20
	out  0x20, al
	pop  es
	pop  ax
	iret 

start:
	call clrscr
	mov di,0
	push es
	call print_boarder
	call print_marks
	call print_score
	call print_lives
	call print_bar
	call print_ball
	call print_bricks
	pop es
	xor  ax, ax               
	mov  es, ax             ; point es to IVT base               
	mov ax, [es:9*4]               
	mov [cs:oldisr], ax        ; save offset of old routine               
	mov ax, [es:9*4+2]               
	mov [cs:oldisr+2], ax 
	mov ax, [es:8*4]               
	mov [cs:oldisr1], ax        ; save offset of old routine               
	mov ax, [es:8*4+2]               
	mov [cs:oldisr1+2], ax 
	cli                     ; disable interrupts
	mov  word [es:9*4], kbisr ; store offset at n*4
	mov  [es:9*4+2], cs     ; store segment at n*4+2
	mov  word [es:8*4], timer
	mov  [es:8*4+2], cs
	 
	mov  dx, start          ; end of resident portion          
	add  dx, 15             ; round up to next para            
	mov  cl, 4           
    shr  dx, cl             ; number of paras          
	mov  ax, 0x3100         ; terminate and stay resident       
	int  0x21 
