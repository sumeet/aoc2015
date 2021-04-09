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
  push 3
  mul esp
  add esp, 4
i4:
  inc eax
  nop
end:
  mov esi, eax
  mov edi, message
  mov eax, 0
  call printf
  ret

message db "%d", 10, 0
