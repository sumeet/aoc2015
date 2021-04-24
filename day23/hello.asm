section .text
global main
extern printf

main:
  mov r9, 1
  mov r10, 0
i0:
cmp r9, 1
je i22


i1:
inc r9

i2:
mov rax, r9
mov r8, 3
mul r8
mov r9, rax


i3:
mov rax, r9
mov r8, 3
mul r8
mov r9, rax


i4:
mov rax, r9
mov r8, 3
mul r8
mov r9, rax


i5:
inc r9

i6:
mov rax, r9
mov r8, 3
mul r8
mov r9, rax


i7:
inc r9

i8:
mov rax, r9
mov r8, 3
mul r8
mov r9, rax


i9:
inc r9

i10:
inc r9

i11:
mov rax, r9
mov r8, 3
mul r8
mov r9, rax


i12:
inc r9

i13:
inc r9

i14:
mov rax, r9
mov r8, 3
mul r8
mov r9, rax


i15:
inc r9

i16:
inc r9

i17:
mov rax, r9
mov r8, 3
mul r8
mov r9, rax


i18:
inc r9

i19:
inc r9

i20:
mov rax, r9
mov r8, 3
mul r8
mov r9, rax


i21:
jmp i40

i22:
mov rax, r9
mov r8, 3
mul r8
mov r9, rax


i23:
mov rax, r9
mov r8, 3
mul r8
mov r9, rax


i24:
mov rax, r9
mov r8, 3
mul r8
mov r9, rax


i25:
mov rax, r9
mov r8, 3
mul r8
mov r9, rax


i26:
inc r9

i27:
inc r9

i28:
mov rax, r9
mov r8, 3
mul r8
mov r9, rax


i29:
inc r9

i30:
mov rax, r9
mov r8, 3
mul r8
mov r9, rax


i31:
inc r9

i32:
inc r9

i33:
mov rax, r9
mov r8, 3
mul r8
mov r9, rax


i34:
inc r9

i35:
inc r9

i36:
mov rax, r9
mov r8, 3
mul r8
mov r9, rax


i37:
inc r9

i38:
mov rax, r9
mov r8, 3
mul r8
mov r9, rax


i39:
mov rax, r9
mov r8, 3
mul r8
mov r9, rax


i40:
cmp r9, 1
je end


i41:
inc r10

i42:
test r9, 1
jz i46


i43:
mov rax, r9
mov r8, 3
mul r8
mov r9, rax


i44:
inc r9

i45:
jmp i47

i46:
mov rax, r9
mov rdx, 0
mov r8, 2
div r8
mov r9, rax


i47:
jmp i40


end:
  mov rsi, r10
  mov rdi, message
  mov rax, 0
  call printf
  ret

message db "%d", 10, 0
