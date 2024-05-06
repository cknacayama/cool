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
    .align 8
Object_Typenamelen:
    .quad 6

IO_Typename:
    .string "IO"
    .align 8
IO_Typenamelen:
    .quad 2

String_Typename:
    .string "String"
    .align 8
String_Typenamelen:
    .quad 6

Int_Typename:
    .string "Int"
    .align 8
Int_Typenamelen:
    .quad 3

Bool_Typename:
    .string "Bool"
    .align 8
Bool_Typenamelen:
    .quad 4

_empty_string:
    .string ""
    .align 8
_empty_stringlen:
    .quad 0

    .align 8
heap_pos:
    .quad 0
