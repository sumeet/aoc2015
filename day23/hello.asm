BITS 32;
section .text
global main
extern printf

main:
  mov eax, 0
  mov ebx, 0
i1:
  inc eax
  nop
i2:
  cmp eax, 1
  je i4
i3:
  mov ecx, 3
  mul ecx
i4:
  inc eax
  nop
end:
  push eax
  push message
  call printf
  add esp, 8
  ret

message db "%d", 10, 0
