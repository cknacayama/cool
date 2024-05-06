    .intel_syntax noprefix
    .text
    .globl _start
_start:
    push    rbp
    mov     rbp, rsp

    call    allocator_init

    # (new Main).main()
    mov     rdi, 0
    call    Main_New
    mov     rdi, rax
    mov     rsi, rdx
    call    Main_main

    pop     rbp

    mov     rax, 60 # exit
    mov     rdi, 0
    syscall

    .globl Object_New
Object_New:
    push    rbp
    mov     rbp, rsp

    cmp     rdi, 0
    jne     .Object_New_L1
    call    allocator_alloc
    jmp     .Object_New_L2
.Object_New_L1:
    mov     rax, rdi
.Object_New_L2:
    lea     rdx, QWORD PTR [rip + Object_Table]

    pop     rbp
    ret

    .globl Object_abort
Object_abort:
    push    rbp
    mov     rbp, rsp

    mov     rax, 60
    mov     rdi, 1
    syscall

    pop     rbp
    ret

    .globl Object_type_name
Object_type_name:
    mov     rax, QWORD PTR [rip + Object_Typenamelen]
    lea     rdx, QWORD PTR [rip + Object_Typename]
    ret

    .globl Object_copy
Object_copy:
    mov     rax, rdi
    mov     rdx, rsi
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
    mov     rax, rsi
    ret

    .globl String_concat
String_concat:
    push    rbp
    mov     rbp, rsp

    sub     rsp, 48

    # self
    mov     QWORD PTR [rbp - 8], rdi # self.length
    mov     QWORD PTR [rbp - 16], rsi # self.content

    # other
    mov     QWORD PTR [rbp - 24], rdx # other.length
    mov     QWORD PTR [rbp - 32], rcx # other.content

    add     rdi, rdx # self.length + other.length
    mov     QWORD PTR [rbp - 40], rdi # new.length
    call    allocator_alloc

    mov     QWORD PTR [rbp - 48], rax # new.content

    mov     rdi, rax # new.content
    mov     rsi, QWORD PTR [rbp - 16] # self.content
    mov     rdx, QWORD PTR [rbp - 8] # self.length
    call    memory_copy

    mov     rdi, QWORD PTR [rbp - 8] # self.length
    add     rdi, rax # new.content+self.length
    mov     rsi, QWORD PTR [rbp - 32] # other.content
    mov     rdx, QWORD PTR [rbp - 24] # other.length
    call    memory_copy

    mov     rax, QWORD PTR [rbp - 40] # new.length
    mov     rdx, QWORD PTR [rbp - 48] # new.content

    add     rsp, 48

    pop     rbp
    ret

    .globl String_substr
String_substr:
    mov     rax, rcx
    add     rdx, rsi
    ret

    .globl String_To_Object
String_To_Object:
    push    rbp
    mov     rbp, rsp

    sub     rsp, 16

    # self
    mov     QWORD PTR [rbp - 8], rdi # length
    mov     QWORD PTR [rbp - 16], rsi # content

    mov     rdi, 16
    call    allocator_alloc

    mov     rdi, QWORD PTR [rbp - 8] # length
    mov     QWORD PTR [rax], rdi
    mov     rdi, QWORD PTR [rbp - 16] # content
    mov     QWORD PTR [rax + 8], rdi
    lea     rdx, QWORD PTR [rip + String_Table]

    add     rsp, 16

    pop     rbp
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
    mov     rax, rdi
    ret

    .globl Int_To_Object
Int_To_Object:
    push    rbx

    mov     rbx, rdi
    mov     rdi, 8
    call    allocator_alloc

    mov     QWORD PTR [rax], rbx
    lea     rdx, QWORD PTR [rip + Int_Table]

    pop     rbx
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
    mov     rax, rdi
    ret

    .globl Bool_To_Object
Bool_To_Object:
    push    rbx

    mov     rbx, rdi
    mov     rdi, 1
    call    allocator_alloc

    mov     BYTE PTR [rax], bl
    lea     rdx, QWORD PTR [rip + Bool_Table]

    pop     rbx
    ret

    .globl IO_New
IO_New:
    push    rbp
    mov     rbp, rsp

    cmp     rdi, 0
    jne     .IO_New_L1
    call    allocator_alloc
    jmp     .IO_New_L2
.IO_New_L1:
    mov     rax, rdi
.IO_New_L2:
    lea     rdx, QWORD PTR [rip + IO_Table]

    pop     rbp
    ret

    .globl IO_type_name
IO_type_name:
    mov     rax, QWORD PTR [rip + IO_Typenamelen]
    lea     rdx, QWORD PTR [rip + IO_Typename]
    ret

    .globl IO_out_string
IO_out_string:
    push    rbp
    mov     rbp, rsp

    sub     rsp, 16

    # self
    mov     QWORD PTR [rbp - 8], rdi
    mov     QWORD PTR [rbp - 16], rsi

    mov     rax, 1
    mov     rdi, 1
    mov     rsi, rcx # content
    # length is already in rdx
    syscall

    mov     rax, QWORD PTR [rbp - 8]
    mov     rdx, QWORD PTR [rbp - 16]

    add     rsp, 16

    pop     rbp
    ret

# TODO: implement IO_out_int
    .globl IO_out_int
IO_out_int:
    mov     rax, rdi
    mov     rdx, rsi
    ret

# TODO: implement IO_in_string
    .globl IO_in_string
IO_in_string:
    call    String_New
    ret

# TODO: implement IO_in_int
    .globl IO_in_int
IO_in_int:
    call    Int_New
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
    cmp     rdi, 0
    je      .allocator_alloc_L1

    mov     rax, 12 # sys_brk
    # align size to 8 bytes
    add     rdi, 7
    and     rdi, -8
    # heap_pos + ALIGN(size)
    add     rdi, QWORD PTR [rip + heap_pos]
    mov     QWORD PTR [rip + heap_pos], rdi
    syscall
.allocator_alloc_L1:
    mov     rax, QWORD PTR [rip + heap_pos]
    ret

    .globl  memory_copy
#   void* memory_copy(void* dest, const void* src, size_t size)
memory_copy:
    mov     rax, 0 # i = 0
#   for (size_t i = 0; i < size; i++)
.memory_copy_L1:
    cmp     rax, rdx
    jae     .memory_copy_L2
    mov     ch, BYTE PTR [rsi + rax]
    mov     BYTE PTR [rdi + rax], ch
    inc     rax
    jmp     .memory_copy_L1
.memory_copy_L2:
    mov     rax, rdi
    ret

