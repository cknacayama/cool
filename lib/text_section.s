    .intel_syntax noprefix
    .text
    .globl _start
_start:
    push    rbp
    mov     rbp, rsp

    call    allocator_init

    # (new Main).main()
    push    0
    call    Main_New
    add     rsp, 8
    push    rdx
    push    rax
    call    Main_main
    add     rsp, 16

    pop     rbp

    mov     rax, 60 # exit
    mov     rdi, 0
    syscall

    .globl Object_New
Object_New:
    cmp     QWORD PTR [rsp + 8], 0
    jne     .Object_New_L1
    push    0
    call    allocator_alloc
    add     rsp, 8
    jmp     .Object_New_L2
.Object_New_L1:
    mov     rax, QWORD PTR [rsp + 8]
.Object_New_L2:
    lea     rdx, QWORD PTR [rip + Object_Table]
    ret

    .globl Object_abort
Object_abort:
    mov     rax, 60
    mov     rdi, 1
    syscall

    .globl Object_type_name
Object_type_name:
    mov     rax, QWORD PTR [rip + Object_Typenamelen]
    lea     rdx, QWORD PTR [rip + Object_Typename]
    ret

    .globl Object_copy
Object_copy:
    mov     rax, QWORD PTR [rsp + 8]
    mov     rdx, QWORD PTR [rsp + 16]
    ret

    .globl String_New
String_New:
    mov     rax, 0
    lea     rdx, QWORD PTR [rip + _empty_string]
    ret

    .globl String_type_name
String_type_name:
    mov     rax, QWORD PTR [rip + String_Typenamelen]
    lea     rdx, QWORD PTR [rip + String_Typename]
    ret

    .globl String_length
String_length:
    mov     rax, QWORD PTR [rsp + 8]
    ret

    .globl String_concat
String_concat:
    push    rbp
    mov     rbp, rsp

    sub     rsp, 16

    mov     rax, QWORD PTR [rbp + 16] # self.length
    add     rax, QWORD PTR [rbp + 32] # other.length
    mov     QWORD PTR [rbp - 8], rax # new.length

    push    rax
    call    allocator_alloc
    add     rsp, 8
    mov     QWORD PTR [rbp - 16], rax # new.content

    push    QWORD PTR [rbp + 16] # self.length
    push    QWORD PTR [rbp + 24] # self.content
    # new.content
    push    rax
    call    memory_copy
    add     rsp, 24

    push    QWORD PTR [rbp + 32] # other.length
    push    QWORD PTR [rbp + 40] # other.content
    mov     rdi, QWORD PTR [rbp + 16] # self.length
    add     rdi, rax # new.content+self.length
    push    rdi
    call    memory_copy
    add     rsp, 24

    mov     rax, QWORD PTR [rbp - 8] # new.length
    mov     rdx, QWORD PTR [rbp - 16] # new.content

    add     rsp, 16

    pop     rbp
    ret

    .globl String_substr
String_substr:
    mov     rax, QWORD PTR [rsp + 32] # l
    mov     rdx, QWORD PTR [rsp + 16] # self.content
    add     rdx, QWORD PTR [rsp + 24] # i
    ret

    .globl String_To_Object
String_To_Object:
    push    16
    call    allocator_alloc
    add     rsp, 8

    mov     rdi, QWORD PTR [rsp + 8] # length
    mov     QWORD PTR [rax], rdi
    mov     rdi, QWORD PTR [rsp + 16] # content
    mov     QWORD PTR [rax + 8], rdi
    lea     rdx, QWORD PTR [rip + String_Table]

    ret

    .globl Int_New
Int_New:
    mov     rax, 0
    ret

    .globl Int_type_name
Int_type_name:
    mov     rax, QWORD PTR [rip + Int_Typenamelen]
    lea     rdx, QWORD PTR [rip + Int_Typename]
    ret

    .globl Int_copy
Int_copy:
    mov     rax, QWORD PTR [rsp + 8]
    ret

    .globl Int_To_Object
Int_To_Object:
    push    8
    call    allocator_alloc
    add     rsp, 8

    mov     rdi, QWORD PTR [rsp + 8]
    mov     QWORD PTR [rax], rdi
    lea     rdx, QWORD PTR [rip + Int_Table]

    ret

    .globl Bool_New
Bool_New:
    mov     rax, 0
    ret

    .globl Bool_type_name
Bool_type_name:
    mov     rax, QWORD PTR [rip + Bool_Typenamelen]
    lea     rdx, QWORD PTR [rip + Bool_Typename]
    ret

    .globl Bool_copy
Bool_copy:
    mov     rax, QWORD PTR [rsp + 8]
    ret

    .globl Bool_To_Object
Bool_To_Object:
    push    1
    call    allocator_alloc
    add     rsp, 8

    mov     rdi, QWORD PTR [rsp + 8]
    mov     BYTE PTR [rax], sil
    lea     rdx, QWORD PTR [rip + Bool_Table]

    ret

    .globl IO_New
IO_New:
    cmp     QWORD PTR [rsp + 8], 0
    jne     .IO_New_L1
    push    0
    call    allocator_alloc
    add     rsp, 8
    jmp     .IO_New_L2
.IO_New_L1:
    mov     rax, QWORD PTR [rsp + 8]
.IO_New_L2:
    lea     rdx, QWORD PTR [rip + IO_Table]

    ret

    .globl IO_type_name
IO_type_name:
    mov     rax, QWORD PTR [rip + IO_Typenamelen]
    lea     rdx, QWORD PTR [rip + IO_Typename]
    ret

    .globl IO_out_string
IO_out_string:
    mov     rax, 1
    mov     rdi, 1
    mov     rsi, QWORD PTR [rsp + 32] # content
    mov     rdx, QWORD PTR [rsp + 24] # length
    syscall

    mov     rax, QWORD PTR [rsp + 8]
    mov     rdx, QWORD PTR [rsp + 16]

    ret

# TODO: implement IO_out_int
    .globl IO_out_int
IO_out_int:
    push    rbp
    mov     rbp, rsp

    mov     rdi, QWORD PTR [rbp + 32] # n
    lea     r10, BYTE PTR [rip + _io_out_int_buf] # buf
    mov     r8, 0 # len
    mov     r9, 20
    mov     rcx, 10
    mov     rsi, rdi
    shr     rdi, 63 # check if n < 0
    cmp     dil, 0
    je      .IO_out_int_L1
    neg     rsi
.IO_out_int_L1:
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
    jne     .IO_out_int_L1

    cmp     dil, 0
    je      .IO_out_int_L2
    mov     BYTE PTR [r10 + r9], 45 # '-'
    add     r8, 1
.IO_out_int_L2:
    mov     rax, 1
    mov     rdi, 1
    mov     r9, 21
    sub     r9, r8
    lea     rsi, BYTE PTR [r10 + r9]
    mov     rdx, r8
    syscall

    mov     rax, QWORD PTR [rbp + 16]
    mov     rdx, QWORD PTR [rbp + 24]

    pop     rbp
    ret

# TODO: implement IO_in_string
    .globl IO_in_string
IO_in_string:
    push    0
    call    String_New
    add     rsp, 8
    ret

# TODO: implement IO_in_int
    .globl IO_in_int
IO_in_int:
    push    0
    call    Int_New
    add     rsp, 8
    ret

    .globl allocator_init
#   void allocator_init(void);
allocator_init:
    mov     rax, 12 # sys_brk
    mov     rdi, 0
    syscall
    mov     QWORD PTR [rip + heap_pos], rax
    ret

    .globl allocator_alloc
#   void *allocator_alloc(size_t size);
allocator_alloc:
    cmp     QWORD PTR [rsp + 8], 0
    je      .allocator_alloc_L1
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
.allocator_alloc_L1:
    ret

    .globl  memory_copy
#   void* memory_copy(void* dest, const void* src, size_t size)
memory_copy:
    mov     rdi, QWORD PTR [rsp + 8] # dest
    mov     rsi, QWORD PTR [rsp + 16] # src
    mov     rax, 0 # i = 0
#   for (size_t i = 0; i < size; i++)
.memory_copy_L1:
    cmp     rax, QWORD PTR [rsp + 24] # size
    jae     .memory_copy_L2
    movzx   ecx, BYTE PTR [rsi + rax]
    mov     BYTE PTR [rdi + rax], cl
    add     rax, 1
    jmp     .memory_copy_L1
.memory_copy_L2:
    mov     rax, rdi
    ret
    
    .globl memory_compare
#   Bool memory_compare(const void* ptr1, const void* ptr2, size_t size)
memory_compare:
    mov     rdi, QWORD PTR [rsp + 8] # ptr1
    mov     rsi, QWORD PTR [rsp + 16] # ptr2
    mov     rax, 0 # i = 0
#   for (size_t i = 0; i < size; i++)
.memory_compare_L1:
    cmp     rax, QWORD PTR [rsp + 24] # size
    jae     .memory_compare_L2
    add     rax, 1
    movzx   ecx, BYTE PTR [rdi + rax - 1]
    cmp     cl, BYTE PTR [rsi + rax - 1]
    je     .memory_compare_L1
    mov     rax, 0
    ret
.memory_compare_L2:
    mov     rax, 1
    ret
