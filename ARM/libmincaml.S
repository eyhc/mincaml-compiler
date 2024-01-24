.data
format_int:    .asciz "%d"
format_float:  .asciz "%f"
format_nwline: .asciz "\n"

.text

.global _min_caml_exit
_min_caml_exit:
    mov r0, #0
    bl exit

.global _min_caml_print_int
_min_caml_print_int:
    push {r0, r1, lr}
    mov r1, r0
    ldr r0, =format_int
    bl printf
    pop {r0, r1, lr}
    bx lr

.global _min_caml_print_newline
_min_caml_print_newline:
    push {r0, lr}
    ldr r0, =format_nwline
    bl printf
    pop {r0, lr}
    bx lr

.global _min_caml_abs
_min_caml_abs:
    push {lr}
    bl abs
    pop {lr}
    bx lr

.global _min_caml_create_array
_min_caml_create_array:
    push {r2, r3, r4, lr}
    mov r2, r0

    @ allocation
    push {r1, r2}
    lsl r0, r2, #2
    bl malloc
    pop {r1, r2}

    @ initialization
    mov r3, #0
while:
    cmp r3, r2
    bge wend
    lsl r4, r3, #2
    add r4, r0, r4
    str r1, [r4]
    add r3, r3, #1
    bal while
wend:
    pop {r2, r3, r4, lr}
    bx lr

@ Floats .....
.global _min_caml_create_float_array
_min_caml_create_float_array:
    push {lr}
    bl _min_caml_create_array
    pop {lr}
    bx lr

.global _min_caml_print_float
_min_caml_print_float:
    push {lr}
    vcvt.f64.f32 d0, s0
    vmov r2, r3, d0
    ldr r0, =format_float
    bl printf
    pop {lr}
    bx lr

.global _min_caml_cos
_min_caml_cos:
    push {lr}
    vmov.f32 r0, s0
    bl cosf
    vmov.f32 s0, r0
    pop {lr}
    bx lr

.global _min_caml_sin
_min_caml_sin:
    push {lr}
    vmov.f32 r0, s0
    bl sinf
    vmov.f32 s0, r0
    pop {lr}
    bx lr

.global _min_caml_sqrt
_min_caml_sqrt:
    push {lr}
    vmov.f32 r0, s0
    bl sqrtf
    vmov.f32 s0, r0
    pop {lr}
    bx lr

.global _min_caml_abs_float
_min_caml_abs_float:
    push {lr}
    vmov.f32 r0, s0
    bl fabsf
    vmov.f32 s0, r0
    pop {lr}
    bx lr

.global _min_caml_float_of_int
_min_caml_float_of_int:
    push {lr}
    vmov.f32 r0, s0
    bl __aeabi_i2f
    vmov.f32 s0, r0
    pop {lr}
    bx lr

.global _min_caml_int_of_float
.global _min_caml_truncate
_min_caml_int_of_float:
_min_caml_truncate:
    push {lr}
    vmov.f32 r0, s0
    bl __aeabi_f2iz
    vmov.f32 s0, r0
    pop {lr}
    bx lr

