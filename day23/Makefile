hello: hello.o
	gcc -no-pie -m64 -o hello hello.o

hello.o: hello.asm
	nasm -felf64 hello.asm -l hello.lst

cpuid: cpuid.o
	gcc -no-pie -m64 -o cpuid cpuid.o

cpuid.o: cpuid.asm
	nasm -felf64 cpuid.asm -l cpuid.lst

clean:
	rm -f hello hello.o
