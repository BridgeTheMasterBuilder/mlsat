all : asm.o
	ar -rcs asm.a asm.o
	ld -shared -z noexecstack -o asm.so asm.o

# asm.a : asm.o
# 	ar -rcs asm.a asm.o

# asm.so: asm.o
# 	ld -pie -o asm.so asm.o

asm.o : assignment.o
	ld -r -z noexecstack -o asm.o assignment.o

assignment.o : assignment.asm
	nasm -felf64 assignment.asm

clean :
	rm *.o
