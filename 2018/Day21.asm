; AoC 2018 Day21 - from elfcode to elf.
; Compile with:
;
; nasm -f elf64 Day21.asm
; gcc -m64 -o Day21 Day21.o
;
; you have to enter you desired value for register 0
; before the label begin at mov r9, <value>
; because I was lazy and did not add program argument support
; For part 1, comment out the lines at the end
; For part 2, get creative in printing values until the right one appears ^^

extern printf

section .data
    fmt: db "%d", 10, 0

section .text

global main
main:
    ; init
    mov rax, 0
    mov rbx, 0
    mov rcx, 0
    mov rdx, 0

    ; mov r9,  7967233  ; solution for part 1 here for convenience
    ; mov r9,  16477902 ; solution for part 2 here for convenience
    mov r9,  0xffff   ; enter r0 value here

begin: ; redundant `and` functionality check
    mov rbx, 123
    and rbx, 456
    cmp rbx, 72
    jne begin

    mov rbx, 0

outer: ; outer loop
    mov rax, rbx
    or  rax, 0x10000
    mov rbx, 10373714

inner: ; inner loop
    mov rdx, rax
    and rdx, 0xff
    add rbx, rdx
    and rbx, 0xffffff

    mov r8, rax
    mov r10, rdx
    mov rax, rbx
    mov r11, 65899
    mul r11
    mov rbx, rax
    mov rax, r8
    mov rdx, r10

    and rbx, 0xffffff
    cmp rax, 256
    jl  end

    shr rax, 8 ; divide rax by 256 via bitshift (optimized)
    jmp inner

end:
    cmp rbx, r9 ; comment out for part 1
    jne outer   ; comment out for part 1

    ; Print out register3's value an exit
    mov rdi, fmt
    mov rsi, rbx
    xor rax, rax
    call printf WRT ..plt

    ; if the program got here, it exits successfully

; Solution part 1: 7967233
; Solution part 2: 16477902
