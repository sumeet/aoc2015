section .text
global main
extern printf

main:
;;mov rax, 0x16
;;cpuid
;;mov rsi, rax
;;mov rdi, message
;;mov rax, 0
;;call printf
;;
;;mov rsi, rbx
;;mov rdi, message
;;mov rax, 0
;;call printf
;;
;;ret


mov eax, 0x80000002
cpuid
mov [buffer], eax
mov [buffer+4], ebx
mov [buffer+8], ecx
mov [buffer+12], edx

mov eax, 0x80000003
cpuid
mov [buffer+16], eax
mov [buffer+20], ebx
mov [buffer+24], ecx
mov [buffer+28], edx

mov eax, 0x80000004
cpuid
mov [buffer+32], eax
mov [buffer+36], ebx
mov [buffer+40], ecx
mov [buffer+44], edx

mov rsi, buffer
mov rdi, message
mov rax, 0
call printf
ret

message db "%s", 10, 0

section .data
buffer db "0000000000000000000000000000000000000000000000", 0
