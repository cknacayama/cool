    .intel_syntax noprefix
    .text
    .globl _start
_start:
    sub     rsp, 8
    push    rbp
    mov     rbp, rsp

    call    allocator.init

    # (new Main).main()
    push    0
    call    Main.New
    add     rsp, 8
    push    rdx
    push    rax
    call    Main.main
    add     rsp, 16

    pop     rbp
    add     rsp, 8

    mov     rax, 60 # exit
    mov     rdi, 0
    syscall

    .globl Object.New
Object.New:
    cmp     QWORD PTR [rsp + 8], 0
    jne     .Object.New_L1
    push    0
    call    allocator.alloc
    add     rsp, 8
    jmp     .Object.New_L2
.Object.New_L1:
    mov     rax, QWORD PTR [rsp + 8]
.Object.New_L2:
    lea     rdx, QWORD PTR [rip + Object.Table]
    ret

    .globl Object.abort
Object.abort:
    mov     rax, 60
    mov     rdi, 1
    syscall

    .globl Object.type_name
Object.type_name:
    mov     rax, QWORD PTR [rip + Object.Typenamelen]
    lea     rdx, QWORD PTR [rip + Object.Typename]
    ret

    .globl Object.copy
Object.copy:
    mov     rax, QWORD PTR [rsp + 8]
    mov     rdx, QWORD PTR [rsp + 16]
    ret

    .globl String.New
String.New:
    mov     rax, 0
    lea     rdx, QWORD PTR [rip + empty_string]
    ret

    .globl String.type_name
String.type_name:
    mov     rax, QWORD PTR [rip + String.Typenamelen]
    lea     rdx, QWORD PTR [rip + String.Typename]
    ret

    .globl String.length
String.length:
    mov     rax, QWORD PTR [rsp + 8]
    ret

    .globl String.concat
String.concat:
    push    rbp
    mov     rbp, rsp

    sub     rsp, 16

    mov     rax, QWORD PTR [rbp + 16] # self.length
    add     rax, QWORD PTR [rbp + 32] # other.length
    mov     QWORD PTR [rbp - 8], rax # new.length

    push    rax
    call    allocator.alloc
    add     rsp, 8
    mov     QWORD PTR [rbp - 16], rax # new.content

    push    QWORD PTR [rbp + 16] # self.length
    push    QWORD PTR [rbp + 24] # self.content
    # new.content
    push    rax
    call    memory.copy
    add     rsp, 24

    push    QWORD PTR [rbp + 32] # other.length
    push    QWORD PTR [rbp + 40] # other.content
    mov     rdi, QWORD PTR [rbp + 16] # self.length
    add     rdi, rax # new.content+self.length
    push    rdi
    call    memory.copy
    add     rsp, 24

    mov     rax, QWORD PTR [rbp - 8] # new.length
    mov     rdx, QWORD PTR [rbp - 16] # new.content

    add     rsp, 16

    pop     rbp
    ret

    .globl String.substr
String.substr:
    mov     rax, QWORD PTR [rsp + 32] # l
    mov     rdx, QWORD PTR [rsp + 16] # self.content
    add     rdx, QWORD PTR [rsp + 24] # i
    ret

    .globl String.To_Object
String.To_Object:
    push    16
    call    allocator.alloc
    add     rsp, 8

    mov     rdi, QWORD PTR [rsp + 8] # length
    mov     QWORD PTR [rax], rdi
    mov     rdi, QWORD PTR [rsp + 16] # content
    mov     QWORD PTR [rax + 8], rdi
    lea     rdx, QWORD PTR [rip + String.Table]

    ret

    .globl Int.New
Int.New:
    mov     rax, 0
    ret

    .globl Int.type_name
Int.type_name:
    mov     rax, QWORD PTR [rip + Int.Typenamelen]
    lea     rdx, QWORD PTR [rip + Int.Typename]
    ret

    .globl Int.copy
Int.copy:
    mov     rax, QWORD PTR [rsp + 8]
    ret

    .globl Int.To_Object
Int.To_Object:
    push    8
    call    allocator.alloc
    add     rsp, 8

    mov     rdi, QWORD PTR [rsp + 8]
    mov     QWORD PTR [rax], rdi
    lea     rdx, QWORD PTR [rip + Int.Table]

    ret

    .globl Bool.New
Bool.New:
    mov     rax, 0
    ret

    .globl Bool.type_name
Bool.type_name:
    mov     rax, QWORD PTR [rip + Bool.Typenamelen]
    lea     rdx, QWORD PTR [rip + Bool.Typename]
    ret

    .globl Bool.copy
Bool.copy:
    mov     rax, QWORD PTR [rsp + 8]
    ret

    .globl Bool.To_Object
Bool.To_Object:
    push    1
    call    allocator.alloc
    add     rsp, 8

    mov     rdi, QWORD PTR [rsp + 8]
    mov     BYTE PTR [rax], sil
    lea     rdx, QWORD PTR [rip + Bool.Table]

    ret

    .globl IO.New
IO.New:
    cmp     QWORD PTR [rsp + 8], 0
    jne     .IO.New_L1
    push    0
    call    allocator.alloc
    add     rsp, 8
    jmp     .IO.New_L2
.IO.New_L1:
    mov     rax, QWORD PTR [rsp + 8]
.IO.New_L2:
    lea     rdx, QWORD PTR [rip + IO.Table]

    ret

    .globl IO.type_name
IO.type_name:
    mov     rax, QWORD PTR [rip + IO.Typenamelen]
    lea     rdx, QWORD PTR [rip + IO.Typename]
    ret

    .globl IO.out_string
IO.out_string:
    mov     rax, 1
    mov     rdi, 1
    mov     rsi, QWORD PTR [rsp + 32] # content
    mov     rdx, QWORD PTR [rsp + 24] # length
    syscall

    mov     rax, QWORD PTR [rsp + 8]
    mov     rdx, QWORD PTR [rsp + 16]

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
    call    String.New
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
    cmp     QWORD PTR [rsp + 8], 0
    je      .allocator.alloc_L1
    mov     rax, 12 # sys_brk
    # align size to 8 bytes
    mov     rdi, QWORD PTR [rsp + 8]
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
