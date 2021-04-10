section .text
global main
extern printf

main:
  mov r9, 0
  mov r10, 0
i0:
cmp r9, 1
je i22


i1:
inc r9

i2:
mov eax, r9
mov r8, 3
mul r8
mov r9, eax


i3:
mov eax, r9
mov r8, 3
mul r8
mov r9, eax


i4:
mov eax, r9
mov r8, 3
mul r8
mov r9, eax


i5:
inc r9

i6:
mov eax, r9
mov r8, 3
mul r8
mov r9, eax


i7:
inc r9

i8:
mov eax, r9
mov r8, 3
mul r8
mov r9, eax


i9:
inc r9

i10:
inc r9

i11:
mov eax, r9
mov r8, 3
mul r8
mov r9, eax


i12:
inc r9

i13:
inc r9

i14:
mov eax, r9
mov r8, 3
mul r8
mov r9, eax


i15:
inc r9

i16:
inc r9

i17:
mov eax, r9
mov r8, 3
mul r8
mov r9, eax


i18:
inc r9

i19:
inc r9

i20:
mov eax, r9
mov r8, 3
mul r8
mov r9, eax


i21:
jmp i40

i22:
mov eax, r9
mov r8, 3
mul r8
mov r9, eax


i23:
mov eax, r9
mov r8, 3
mul r8
mov r9, eax


i24:
mov eax, r9
mov r8, 3
mul r8
mov r9, eax


i25:
mov eax, r9
mov r8, 3
mul r8
mov r9, eax


i26:
inc r9

i27:
inc r9

i28:
mov eax, r9
mov r8, 3
mul r8
mov r9, eax


i29:
inc r9

i30:
mov eax, r9
mov r8, 3
mul r8
mov r9, eax


i31:
inc r9

i32:
inc r9

i33:
mov eax, r9
mov r8, 3
mul r8
mov r9, eax


i34:
inc r9

i35:
inc r9

i36:
mov eax, r9
mov r8, 3
mul r8
mov r9, eax


i37:
inc r9

i38:
mov eax, r9
mov r8, 3
mul r8
mov r9, eax


i39:
mov eax, r9
mov r8, 3
mul r8
mov r9, eax


i40:
cmp r9, 1
je i48


i41:
inc r10

i42:
test r9, 1
jz i46


i43:
mov eax, r9
mov r8, 3
mul r8
mov r9, eax


i44:
inc r9

i45:
jmp i47

i46:
mov eax, r9
mov r8, 2
div r8
mov r9, eax


i47:
jmp i40


i48:
end:
  mov esi, r10
  mov edi, message
  mov eax, 0
  call printf
  ret

message db "%d", 10, 0
