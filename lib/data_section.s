    .intel_syntax noprefix
    .data
Object_Table:
    .quad Object_New
    .quad Object_abort
    .quad Object_type_name
    .quad Object_copy

String_Table:
    .quad String_New
    .quad Object_abort
    .quad String_type_name
    .quad Object_copy
    .quad String_length
    .quad String_concat
    .quad String_substr

Int_Table:
    .quad Int_New
    .quad Object_abort
    .quad Int_type_name
    .quad Int_copy

Bool_Table:
    .quad Bool_New
    .quad Object_abort
    .quad Bool_type_name
    .quad Bool_copy

IO_Table:
    .quad IO_New
    .quad Object_abort
    .quad IO_type_name
    .quad Object_copy
    .quad IO_out_string
    .quad IO_out_int
    .quad IO_in_string
    .quad IO_in_int

Object_Typename:
    .string "Object"
Object_Typenamelen:
    .quad .-Object_Typename-1

IO_Typename:
    .string "IO"
IO_Typenamelen:
    .quad .-IO_Typename-1

String_Typename:
    .string "String"
String_Typenamelen:
    .quad .-String_Typename-1

Int_Typename:
    .string "Int"
Int_Typenamelen:
    .quad .-Int_Typename-1

Bool_Typename:
    .string "Bool"
Bool_Typenamelen:
    .quad .-Bool_Typename-1

_empty_string:
    .string ""
_empty_stringlen:
    .quad 0

_int_fmt_string:
    .string "%ld"
_int_fmt_stringlen:
    .quad .-_int_fmt_string-1

