    global value

    section .text

value:
    mov edx, edi
    sar edi, 1
    mov ecx, edi
    neg ecx
    cmovns edi, ecx
    mov rax, [rsi+rdi*8]
    and rax, -2
    mov rcx, rax
    neg rcx
    test edx, edx
    cmovs rax, rcx
    or rax, 1
    ret
