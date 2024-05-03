    .text
    .globl main
main:
    push    rbp
    mov     rbp, rsp

    # (new Main).main()
    call    Main_New
    mov     rdi, rax
    mov     rsi, rdx
    call    Main_main

    mov     eax, 0
    pop     rbp
    ret

void:
    mov     rax, 0
    mov     rdx, 0
    ret

Object_New:
    push    rbp
    mov     rbp, rsp

    cmp     rdi, 0
    jne     .Object_New_L1
    call    malloc
    jmp     .Object_New_L2
.Object_New_L1:
    mov     rax, rdi
.Object_New_L2:
    lea     rdx, [rip+Object_Table]

    pop     rbp
    ret

Object_abort:
    push    rbp
    mov     rbp, rsp

    mov     rax, 60
    mov     rdi, 1
    syscall

    pop     rbp
    ret

Object_type_name:
    mov     rax, [rip+Object_Typenamelen]
    lea     rdx, [rip+Object_Typename]
    ret

Object_copy:
    mov     rax, rdi
    mov     rdx, rsi
    ret

String_New:
    mov     rax, 0
    lea     rdx, [rip+_empty_string]
    ret

String_type_name:
    mov     rax, [rip+String_Typenamelen]
    lea     rdx, [rip+String_Typename]
    ret

String_length:
    mov     rax, rsi
    ret

String_concat:
    push    rbp
    mov     rbp, rsp

    sub     rsp, 48

    # self
    mov     QWORD PTR [rbp-8], rdi # self.length
    mov     QWORD PTR [rbp-16], rsi # self.content

    # other
    mov     QWORD PTR [rbp-24], rdx # other.length
    mov     QWORD PTR [rbp-32], rcx # other.content

    add     rdi, rdx # self.length+other.length
    mov     QWORD PTR [rbp-40], rdi # new.length
    call    malloc

    mov     QWORD PTR [rbp-48], rax # new.content

    mov     rdi, rax # new.content
    mov     rsi, [rbp-16] # self.content
    mov     rdx, [rbp-8] # self.length
    call    memcpy

    mov     rdi, [rbp-8] # self.length
    add     rdi, rax # new.content+self.length
    mov     rsi, [rbp-32] # other.content
    mov     rdx, [rbp-24] # other.length
    call    memcpy

    mov     rax, [rbp-40] # new.length
    mov     rdx, [rbp-48] # new.content

    add     rsp, 48

    pop     rbp
    ret

String_substr:
    mov     rax, rcx
    add     rdx, rsi
    ret

Int_New:
    mov     rax, 0
    ret

Int_type_name:
    mov     rax, [rip+Int_Typenamelen]
    lea     rdx, [rip+Int_Typename]
    ret

Int_copy:
    mov     rax, rdi
    ret

Bool_New:
    mov     rax, 0
    ret

Bool_type_name:
    mov     rax, [rip+Bool_Typenamelen]
    lea     rdx, [rip+Bool_Typename]
    ret

Bool_copy:
    mov     rax, rdi
    ret

IO_New:
    push    rbp
    mov     rbp, rsp

    cmp     rdi, 0
    jne     .IO_New_L1
    call    malloc
    jmp     .IO_New_L2
.IO_New_L1:
    mov     rax, rdi
.IO_New_L2:
    lea     rdx, [rip+IO_Table]

    pop     rbp
    ret

IO_type_name:
    mov     rax, [rip+IO_Typenamelen]
    lea     rdx, [rip+IO_Typename]
    ret

IO_out_string:
    push    rbp
    mov     rbp, rsp

    sub     rsp, 16

    # self
    mov     QWORD PTR [rbp-8], rdi
    mov     QWORD PTR [rbp-16], rsi

    mov     rax, 1
    mov     rdi, 1
    mov     rsi, rcx # content
    # length is already in rdx
    syscall

    mov     rax, [rbp-8]
    mov     rdx, [rbp-16]

    add     rsp, 16

    pop     rbp
    ret

IO_out_int:
    push    rbp
    mov     rbp, rsp

    sub     rsp, 16

    # self
    mov     QWORD PTR [rbp-8], rdi
    mov     QWORD PTR [rbp-16], rsi

    lea     rdi, [rip+_int_fmt_string]
    mov     rsi, rdx # n
    mov     rax, 0
    call    printf

    mov     rax, [rbp-8]
    mov     rdx, [rbp-16]

    add     rsp, 16

    pop     rbp
    ret

IO_in_string:
IO_in_int:
