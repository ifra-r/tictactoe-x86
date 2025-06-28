[org 0x100]
    jmp start 

; blocks in diff colors
            red: equ 0x04DB  
            black: equ 0x70DB
            white: equ 0x07DB 
            green: equ 0x02DB
; postions:
            start_grid: dw 356                                                         
            ; box pos , isOccupied? 0 -> empty, 1 -> cross, 2 ->circle
            box1: dw 352  
                  db 0x0        
            box2: dw 378  
                  db 0x0 
            box3: dw 404  
                  db 0x0 
            box4: dw 1472 
                  db 0x0
            box5: dw 1498 
                  db 0x0
            box6: dw 1524 
                  db 0x0
            box7: dw 2594 
                  db 0x0
            box8: dw 2620 
                  db 0x0
            box9: dw 2646 
                  db 0x0

turn: db 0      ; 1 -> cross , 2 -> circle
oldIsr: dd 0x0   

start: 
        call clrscr
        call draw_grid
        mov word [turn], 1          ; set turn

        call set_Isr
        infloop:    
                    mov ah, 0
                    int 0x16            ; get keystroke in al

                    cmp al, 27          ; cmp it with escape key code           ;  ;0x01        ; 0x11B ??? 
                    jne infloop

                    ; restore oldisr at its position (unhookin interrupt)
                
                    save_oldisr_and_end_prog:
                                mov ax, [oldIsr]
                                mov bx, [oldIsr+2]

                                cli
                                mov [es:9*4], ax
                                mov [es:9*4+2], bx
                                sti

                    terminate_prog:
                            mov ax, 0x4c00
                            int 0x21
is_draw:
    ; rec res, doesnt clean it
    push bp
    mov bp, sp 
    pusha

    mov si, box1+2

    mov cx, 9
    here1:
        mov bl, byte [si]
        cmp bl, 0
        je false_draw

        add si, 3
        loop here1

    mov word [bp+4], 0x1        ;ret true
    jmp exit_draw

    false_draw:
        mov word [bp+4], 0x0

    exit_draw:
    popa
    pop bp
    ret
handle_input:
    ;rec box address
    push bp
    mov bp, sp
    pusha

    mov si, [bp+4]      ; box address
    cmp byte [si+2], 0x0
                ; jne not_occupied
                je not_occupied
                mov word [es:0], red
                jmp exit_handle_input

    not_occupied:
        cmp byte [turn], 0x1
        jne circle_turn

        cross_turn:
                push red
                push cross
                push word [si]
                call draw_symbol

                mov byte [si+2], 0x1  ; for cross
                call change_turn
                jmp exit_handle_input

        circle_turn:

            cmp byte [turn], 0x2
            jne exit_handle_input

                push green
                push circle
                push word [si]
                call draw_symbol

                mov byte [si+2], 0x2 ;for circle
                call change_turn
    
    exit_handle_input:
    popa
    pop bp
    ret 2

myisr:
    pusha

    mov dx, 0x60                ; to read word 
    in al, dx
    cmp al, 0x02

    push 0xb800
    pop es 

    ; Check if it's a key release (break code)
                test al, 0x80
                jnz exit   ; If highest bit is set, it's a break code, ignore it

    one:
        cmp al, 0x02           ;scancode cmp
        jne two

        push word box1
        call handle_input
        jmp checkWinStatus
 
    two:
        cmp al, 3
        jne three
 
        push word box2
        call handle_input
        jmp checkWinStatus
    three:
        cmp al, 4
        jne four
 
        push word box3
        call handle_input
        jmp checkWinStatus
    four:
        cmp al, 5
        jne five
 
        push word box4
        call handle_input
        jmp checkWinStatus
    five:
        cmp al, 6
        jne six

        push word box5
        call handle_input
        jmp checkWinStatus
    six:
        cmp al, 7
        jne seven

        push word box6
        call handle_input
        jmp checkWinStatus
    seven:
        cmp al, 8
        jne eight

        push word box7
        call handle_input
        jmp checkWinStatus
    eight:
        cmp al, 9
        jne nine

        push word box8
        call handle_input
        jmp checkWinStatus
    nine:
        cmp al, 0xA
        jne checkWinStatus

        push word box9
        call handle_input
        jmp checkWinStatus

    checkWinStatus:
        push 0x0
        call IsWon
        pop ax
        cmp ax, 0
        je check_draw_status
        won:
                call win_display

                mov al, 0x20
                out 0x20, al
                popa
                ; jmp far [cs:oldIsr]  
                jmp save_oldisr_and_end_prog        ; end game

    check_draw_status:
        push 0x0
        call is_draw
        pop ax        
        cmp ax, 0
        je exit

        draww:
                call draw_display

                mov al, 0x20
                out 0x20, al
                popa
                ; jmp far [cs:oldIsr]  
                jmp save_oldisr_and_end_prog        ; end game


    exit:
        ; Send EOI to PIC
                mov al, 0x20
                out 0x20, al

    popa
    jmp far [cs:oldIsr]          ; giving control to org ISR (it will clean up stack so no need of iret)
win_display:
    pusha

    push 0xB800
    pop es

    mov byte [es:152], 'W' 
    mov byte [es:154], 'O' 
    mov byte [es:156], 'N' 
    mov byte [es:158], '!' 

    mov bl, byte [turn]
    xor bh, bh
    cmp bx, 1
    je crossWon

    cicleWon:
            mov word [es:148], red
            jmp exit_winDisplay

    crossWon:
            mov word [es:148], green

    exit_winDisplay:
        popa
        ret
draw_display:
    pusha
    push 0xB800
    pop es
    mov byte [es:152], 'D' 
    mov byte [es:154], 'R' 
    mov byte [es:156], 'A' 
    mov byte [es:158], 'W' 
    popa
    ret
check_equal_and_not_empty:
                                ; Stack: ===========•
                                ; Res
                                ; Num 
                                ; Num
                                ; Num
                                ; Ip
                                ; Bp

    ; check if all three are equal and non zero
    push bp
    mov bp, sp 
    pusha

    mov ax, [bp+4]       ; ax = num1
    cmp ax, [bp+6]       ; compare num1 with num2
    jne false

    cmp ax, [bp+8]      ; compare num1 with num3
    jne false

    ; All equal
        ; none zero?
            cmp word [bp+8], 0
            je false

            mov word [bp+10], 0x1     ; store 1 at result
            jmp done

    false:
        mov word [bp+10], 0     ; store 0 at result

    done:
        popa
        pop bp
        ret 6                ; pop 3 params (6 bytes), leave result ptr on stack
IsWon:     ; Rec result, doesn't clean it
    push bp
    mov bp, sp
    pusha
    
    Row1:
        push word 0x0
        mov bh, 0
        mov bl, byte [box1+2]
        push bx
        mov bh, 0
        mov bl, byte [box2+2]
        push bx
        mov bh, 0
        mov bl, byte [box3+2]
        push bx
        call check_equal_and_not_empty
        pop ax
        cmp ax, 1
        je retTrue
        
    Row2:
        push word 0x0
        mov bh, 0
        mov bl, byte [box4+2]
        push bx
        mov bh, 0
        mov bl, byte [box5+2]
        push bx
        mov bh, 0
        mov bl, byte [box6+2]
        push bx
        call check_equal_and_not_empty
        pop ax
        cmp ax, 1
        je retTrue
        
    Row3:
        push word 0x0
        mov bh, 0
        mov bl, byte [box7+2]
        push bx
        mov bh, 0
        mov bl, byte [box8+2]
        push bx
        mov bh, 0
        mov bl, byte [box9+2]
        push bx
        call check_equal_and_not_empty
        pop ax
        cmp ax, 1
        je retTrue
        
    Col1:
        push word 0x0
        mov bh, 0
        mov bl, byte [box1+2]
        push bx
        mov bh, 0
        mov bl, byte [box4+2]
        push bx
        mov bh, 0
        mov bl, byte [box7+2]
        push bx
        call check_equal_and_not_empty
        pop ax
        cmp ax, 1
        je retTrue
        
    Col2:
        push word 0x0
        mov bh, 0
        mov bl, byte [box2+2]
        push bx
        mov bh, 0
        mov bl, byte [box5+2]
        push bx
        mov bh, 0
        mov bl, byte [box8+2]
        push bx
        call check_equal_and_not_empty
        pop ax
        cmp ax, 1
        je retTrue
        
    Col3:
        push word 0x0
        mov bh, 0
        mov bl, byte [box3+2]
        push bx
        mov bh, 0
        mov bl, byte [box6+2]
        push bx
        mov bh, 0
        mov bl, byte [box9+2]
        push bx
        call check_equal_and_not_empty
        pop ax
        cmp ax, 1
        je retTrue
        
    Diag1:
        push word 0x0
        mov bh, 0
        mov bl, byte [box1+2]
        push bx
        mov bh, 0
        mov bl, byte [box5+2]
        push bx
        mov bh, 0
        mov bl, byte [box9+2]
        push bx
        call check_equal_and_not_empty
        pop ax
        cmp ax, 1
        je retTrue
        
    Diag2:
        push word 0x0
        mov bh, 0
        mov bl, byte [box3+2]
        push bx
        mov bh, 0
        mov bl, byte [box5+2]
        push bx
        mov bh, 0
        mov bl, byte [box7+2]
        push bx
        call check_equal_and_not_empty
        pop ax
        cmp ax, 1
        je retTrue
        
    mov word [bp+4], 0x0   ; ret false as res
    jmp exitWon
    
    retTrue:
        mov word [bp+4], 0x1
        
    exitWon:
        popa
        pop bp
        ret  ; don't remove res from stack 
set_Isr:
    push es
    push ax
    push bx
    xor ax, ax
    mov es, ax
    mov ax, [es:9*4]
    mov [oldIsr], ax
    mov ax, [es:9*4+2]
    mov [oldIsr+2], ax          ; oldisr saved
    cli                         ; disabling interrupts to avoid program crash 
    mov word [es:9*4], myisr
    mov word [es:9*4+2], cs
    sti                         ; interrupts enabled agaim
    pop bx
    pop ax
    pop es
    ret
change_turn: 
    cmp byte [turn], 1
        jne set_to_one

    ; If it was 1, set to 2
    mov byte [turn], 2
    jmp done_change

    set_to_one:
        mov byte [turn], 1   

    done_change:
        ret  
clrscr: 
    push ax
    push es
    push di

    mov ax, 0xb800
    mov es, ax
    mov di, 0                     ;location indexer

    nextposition:
        mov word[es:di], 0x0720   ;black ;space  character
        add di, 2                 ;next cell
        cmp di, 4000              ;total cells - 80*25= 2000 (2 byte cells) so 4000
        jnz nextposition

    pop di
    pop es
    pop ax
    ret
cross:
        dw 0000000000000000b,
        dw 0000010001000000b,
        dw 0000001010000000b,
        dw 0000000100000000b,
        dw 0000001010000000b,
        dw 0000010001000000b

circle:
        dw 0000000000000000b,
        dw 0000001110000000b,
        dw 0000010001000000b,
        dw 0000010001000000b,
        dw 0000001110000000b,
        dw 0000000000000000b

draw_symbol:
    ;rec attribyte wpprd,  symbol address and pos (to print at)

    ; sub routine setup
            push bp
            mov bp, sp
            pusha

    ; using bits
    push 0xb800
    pop es
    mov cx, 6             ; totak 6 rows
    xor di, di
    mov bx, [bp+6]          ; address of symbol
    mov si, [bp+4]          ; pos to print cross at
    mov ax, [bp+8]
    outerloop:
        push cx
        ; deal with one word
        mov cx, 16
        mov dx, word  [bx+di]           ; [circle+di]
        push si
        CLC
        here:
            shl dx, 1
            jc draw_cross_here                 
                jmp updates

            draw_cross_here:
                mov word [es:si], ax ;red   ;_dot         
        updates:
            add si, 2
            loop here

        pop si
        add si, 160
        pop cx
        add di, 2          ;move to next word
        loop outerloop

    popa
    pop bp
    ret 6 
draw_grid:
    pusha
    ; set up es amd start index for printing 
            push 0xb800
            pop es
            mov si, [start_grid]

    mov cx, 3
    outerloop2:
        push cx 
        push si
        mov cx, 3
        innerloop2:
            push si
            call draw_box
            exit2:
                add si, 26   ; ADJUST ACC. HARDCORD!!!!!!!!!!!!
                loop innerloop2
        pop si
        pop cx
        add si, 1120 ;960    ; ADJUST ACC. HARDCORD!!!!!!!!!!!! move down to draw bottom row bpx
        loop outerloop2

    popa
    ret

draw_box:
    ; receives start index

    ; sub routine setup
            push bp
            mov bp, sp
            pusha

    ; set es for printing
            push 0xb800
            pop es

    ; set environment for loop
            mov cx, 6
            mov si, [bp+4]  ; start index to print at

    ; 6 rows, 12 cols
    outerloop1:
            push si
            push cx
            mov dx, cx      ; to use it as a row index in inner loop condition for boundaries
            mov cx, 12

            innerloop1:
                        not_boundary:
                            mov word [es:si], white
                            jmp exit_loop1

                            exit_loop1:
                                add si, 2           ; move to next pos
                        loop innerloop1

            pop cx
            pop si
            add si, 160
            loop outerloop1   

    popa
    pop bp
    ret 2

