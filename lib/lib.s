    .intel_syntax noprefix
    .data
Object.Typename:
    .string "Object"
    .align 8
Object.Typenamelen:
    .quad 6

IO.Typename:
    .string "IO"
    .align 8
IO.Typenamelen:
    .quad 2

String.Typename:
    .string "String"
    .align 8
String.Typenamelen:
    .quad 6

Int.Typename:
    .string "Int"
    .align 8
Int.Typenamelen:
    .quad 3

Bool.Typename:
    .string "Bool"
    .align 8
Bool.Typenamelen:
    .quad 4

Allocator.Typename:
    .string "Allocator"
    .align 8
Allocator.Typenamelen:
    .quad 9

empty_string:
    .string ""
    .align 8
empty_string_len:
    .quad 0

io_out_int_buf_size:
    .quad 21

    .bss 
heap_pos:
    .quad 0
io_out_int_buf:
    .skip 21

    .text
    .globl _start
_start:
    push    rbp
    mov     rbp, rsp

    call    allocator.init

    # (new Main).main()
    mov     rdi, 0
    call    allocator.alloc

    mov     rdi, rax
    call    Main.new
    mov     rdi, rax
    mov     rsi, rdx
    call    Main.main

    pop     rbp
    mov     rax, 60 # exit
    mov     rdi, 0
    syscall

    .globl Object.new
Object.new:
    mov     rax, rdi
    lea     rdx, QWORD PTR [rip + Object.Table]
    ret

    .globl Object.abort
Object.abort:
    mov     rax, 60
    mov     rdi, 1
    syscall

    .globl Object.type_name
Object.type_name:
    lea     rax, QWORD PTR [rip + Object.Typename]
    mov     rdx, QWORD PTR [rip + Object.Typenamelen]
    ret

    .globl Object.copy
Object.copy:
    mov     rax, rdi
    mov     rdx, rsi
    ret

    .globl String.new
String.new:
    lea     rax, QWORD PTR [rip + empty_string]
    mov     rdx, 0
    ret

    .globl String.type_name
String.type_name:
    lea     rax, QWORD PTR [rip + String.Typename]
    mov     rdx, QWORD PTR [rip + String.Typenamelen]
    ret

    .globl String.copy
String.copy:
    mov     rax, rdi
    mov     rdx, rsi
    ret

    .globl String.length
String.length:
    mov     rax, rsi
    ret

    .globl String.concat
String.concat:
    push    rbp
    mov     rbp, rsp

    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15

    mov     rbx, rsi
    add     rbx, rcx # new.lenght = other.length + self.lenght
    mov     r12, rdi # self.content
    mov     r13, rsi # self.lenght
    mov     r14, rdx # other.content
    mov     r15, rcx # other.len

    mov     rdi, rbx
    call    allocator.alloc

    mov     rdi, rax
    mov     rsi, r12
    mov     rdx, r13
    call    memory.copy

    mov     rdi, r13 # self.length
    add     rdi, rax # new.content + self.length
    mov     rsi, r14
    mov     rdx, r15
    call    memory.copy

    mov     rdx, rbx

    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx

    pop     rbp
    ret

    .globl String.substr
String.substr:
    mov     rax, rdi # self.content
    add     rax, rdx # i
    mov     rdx, rsi # l
    ret

    .globl String.Cast
String.Cast:
    push    rdi
    push    rsi

    mov     rdi, 16
    call    allocator.alloc

    pop     QWORD PTR [rax + 8]
    pop     QWORD PTR [rax]
    lea     rdx, QWORD PTR [rip + String.Table]

    ret

    .globl Int.new
Int.new:
    mov     rax, 0
    ret

    .globl Int.type_name
Int.type_name:
    lea     rax, QWORD PTR [rip + Int.Typename]
    mov     rdx, QWORD PTR [rip + Int.Typenamelen]
    ret

    .globl Int.copy
Int.copy:
    mov     rax, rdi
    ret

    .globl Int.Cast
Int.Cast:
    push    rdi

    mov     rdi, 8
    call    allocator.alloc

    pop     QWORD PTR [rax]
    lea     rdx, QWORD PTR [rip + Int.Table]
    ret

    .globl Bool.new
Bool.new:
    mov     rax, 0
    ret

    .globl Bool.type_name
Bool.type_name:
    lea     rax, QWORD PTR [rip + Bool.Typename]
    mov     rdx, QWORD PTR [rip + Bool.Typenamelen]
    ret

    .globl Bool.copy
Bool.copy:
    mov     rax, rdi
    ret

    .globl Bool.Cast
Bool.Cast:
    push    rdi

    mov     rdi, 1
    call    allocator.alloc

    pop     rdi
    mov     BYTE PTR [rax], sil
    lea     rdx, QWORD PTR [rip + Bool.Table]

    ret

    .globl IO.new
IO.new:
    mov     rax, rdi
    lea     rdx, QWORD PTR [rip + IO.Table]
    ret

    .globl IO.type_name
IO.type_name:
    lea     rax, QWORD PTR [rip + IO.Typename]
    mov     rdx, QWORD PTR [rip + IO.Typenamelen]
    ret

    .globl IO.copy
IO.copy:
    mov     rax, rdi
    mov     rdx, rsi
    ret

    .globl IO.out_string
IO.out_string:
    push    rdi
    push    rsi

    mov     rsi, rdx
    mov     rdx, rcx
    mov     rax, 1
    mov     rdi, 1
    syscall

    pop     rdx
    pop     rax
    ret

# TODO: implement IO.out_int
    .globl IO.out_int
IO.out_int:
    push    QWORD PTR [rsp + 24] # n
    call    convert.itoa
    add     rsp, 8

    mov     rsi, rdx
    mov     rdx, rax
    mov     rax, 1
    mov     rdi, 1
    syscall

    mov     rax, QWORD PTR [rsp + 8]
    mov     rdx, QWORD PTR [rsp + 16]

    ret

# TODO: implement IO.in_string
    .globl IO.in_string
IO.in_string:
    push    0
    call    String.new
    add     rsp, 8
    ret

# TODO: implement IO.in_int
    .globl IO.in_int
IO.in_int:
    mov     rax, 0
    mov     rdi, 0
    lea     rsi, BYTE PTR [rip + io_out_int_buf]
    mov     rdx, 21
    syscall

    cmp     rax, 0 
    jg      .IO.in_int_L1
    mov     rax, 0
    ret
.IO.in_int_L1:

    lea     rsi, BYTE PTR [rip + io_out_int_buf]
    push    rsi
    sub     rax, 1
    push    rax
    call    convert.atoi
    add     rsp, 16

    ret

    .globl Allocator.new
Allocator.new:
    mov     rax, rdi
    lea     rdx, QWORD PTR [rip + Allocator.Table]
    ret

    .globl Allocator.type_name
Allocator.type_name:
    lea     rax, QWORD PTR [rip + Allocator.Typename]
    mov     rdx, QWORD PTR [rip + Allocator.Typenamelen]
    ret

    .globl Allocator.copy
Allocator.copy:
    mov     rax, rdi
    mov     rdx, rsi
    ret

    .globl Allocator.alloc
Allocator.alloc:
    mov     rdi, rdx
    call    allocator.alloc
    ret

    .globl Allocator.free
Allocator.free:
    ret

    .globl allocator.init
#   void allocator.init(void);
allocator.init:
    mov     rax, 12 # sys_brk
    mov     rdi, 0
    syscall
    mov     QWORD PTR [rip + heap_pos], rax
    ret

    .globl allocator.alloc
#   void *allocator.alloc(size_t size);
allocator.alloc:
    cmp     rdi, 0
    je      .allocator.alloc_L1
    mov     rax, 12 # sys_brk
    # align size to 8 bytes
    add     rdi, 7
    and     rdi, -8
    # heap_pos + ALIGN(size)
    add     rdi, QWORD PTR [rip + heap_pos]
    push    rbx
    mov     rbx, QWORD PTR [rip + heap_pos]
    mov     QWORD PTR [rip + heap_pos], rdi
    syscall
    mov     rax, rbx
    pop     rbx
.allocator.alloc_L1:
    ret

    .globl  memory.copy
#   void* memory.copy(void* dest, const void* src, size_t size)
memory.copy:
    mov     rdi, QWORD PTR [rsp + 8] # dest
    mov     rsi, QWORD PTR [rsp + 16] # src
    mov     rax, 0 # i = 0
#   for (size_t i = 0; i < size; i++)
.memory.copy_L1:
    cmp     rax, QWORD PTR [rsp + 24] # size
    jae     .memory.copy_L2
    movzx   ecx, BYTE PTR [rsi + rax]
    mov     BYTE PTR [rdi + rax], cl
    add     rax, 1
    jmp     .memory.copy_L1
.memory.copy_L2:
    mov     rax, rdi
    ret
    
    .globl memory.compare
#   Bool memory.compare(const void* ptr1, const void* ptr2, size_t size)
memory.compare:
    mov     rdi, QWORD PTR [rsp + 8] # ptr1
    mov     rsi, QWORD PTR [rsp + 16] # ptr2
    mov     rax, 0 # i = 0
#   for (size_t i = 0; i < size; i++)
.memory.compare_L1:
    cmp     rax, QWORD PTR [rsp + 24] # size
    jae     .memory.compare_L2
    add     rax, 1
    movzx   ecx, BYTE PTR [rdi + rax - 1]
    cmp     cl, BYTE PTR [rsi + rax - 1]
    je     .memory.compare_L1
    mov     rax, 0
    ret
.memory.compare_L2:
    mov     rax, 1
    ret

    .globl class.distance
#   int64_t class.distance(void* T1, void* T2)
#   return the distance between the two classes
#   if T1 == T2, return 0
#   if T1 is not <= T2, return -1 
#   if T1 is < T2, return the distance between T1 and T2
class.distance:
    xor     eax, eax

    mov     rdi, QWORD PTR [rsp + 8] # T1
    mov     rsi, QWORD PTR [rsp + 16] # T2
    cmp     rdi, rsi
    jne     .class.distance_L2
    ret
.class.distance_L1:
    add     rax, 1
    cmp     rdi, rsi
    je      .class.distance_L3
.class.distance_L2:
    mov     rdi, QWORD PTR [rdi] # T1
    test    rdi, rdi
    jne     .class.distance_L1
    mov     rax, -1
    ret
.class.distance_L3:
    ret

    .globl convert.itoa
#   String convert.itoa(int64_t n)
#   use string immediately after calling this function
convert.itoa:
    mov     rdi, QWORD PTR [rsp + 8] # n
    lea     r10, BYTE PTR [rip + io_out_int_buf] # buf
    mov     r8, 0 # len
    mov     r9, 20
    mov     rcx, 10
    mov     rsi, rdi
    shr     rdi, 63 # check if n < 0
    cmp     dil, 0
    je      .convert.itoa_L1
    neg     rsi
.convert.itoa_L1:
    mov     rax, rsi
    cqo
    idiv    rcx
    # quotient in rax, remainder in rdx
    add     rdx, 48 # n % 10 + '0'
    mov     BYTE PTR [r10 + r9], dl
    add     r8, 1
    sub     r9, 1
    mov     rsi, rax
    cmp     rsi, 0
    jne     .convert.itoa_L1

    cmp     dil, 0
    je      .convert.itoa_L2
    mov     BYTE PTR [r10 + r9], 45 # '-'
    add     r8, 1
.convert.itoa_L2:
    mov     r9, 21
    sub     r9, r8
    lea     rdx, BYTE PTR [r10 + r9]
    mov     rax, r8
    ret

    .globl convert.atoi
#   int64_t convert.atoi(String s)
convert.atoi:
    mov     rax, 0 # n
    mov     rdi, QWORD PTR [rsp + 8] # len
    test    rdi, rdi
    je      .convert.atoi_L3

    mov     rsi, QWORD PTR [rsp + 16] # content
    movzx   r9d, BYTE PTR [rsi]
    cmp     r9, 45 # '-'
    sete    r9b
    mov     r8, 0 # i
    add     r8, r9
    mov     rdx, 10
#   for (size_t i = 0; i < len; i++)
.convert.atoi_L1:
    cmp     r8, rdi
    jae     .convert.atoi_L2
    movzx   ecx, BYTE PTR [rsi + r8]
    sub     rcx, 48 # s[i] - '0'
    imul    rax, rdx
    add     rax, rcx
    add     r8, 1
    jmp     .convert.atoi_L1
.convert.atoi_L2:
    test    r9, r9
    je      .convert.atoi_L3
    neg     rax
.convert.atoi_L3:
    ret
