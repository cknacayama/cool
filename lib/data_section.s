Object.Table:
    .quad 0
    .quad Object.new
    .quad Object.abort
    .quad Object.type_name
    .quad Object.copy

String.Table:
    .quad Object.Table
    .quad String.new
    .quad Object.abort
    .quad String.type_name
    .quad Object.copy
    .quad String.length
    .quad String.concat
    .quad String.substr

Int.Table:
    .quad Object.Table
    .quad Int.new
    .quad Object.abort
    .quad Int.type_name
    .quad Int.copy

Bool.Table:
    .quad Object.Table
    .quad Bool.new
    .quad Object.abort
    .quad Bool.type_name
    .quad Bool.copy

IO.Table:
    .quad Object.Table
    .quad IO.new
    .quad Object.abort
    .quad IO.type_name
    .quad Object.copy
    .quad IO.out_string
    .quad IO.out_int
    .quad IO.in_string
    .quad IO.in_int

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

