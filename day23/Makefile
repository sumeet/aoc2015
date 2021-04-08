hello: hello.o
	gcc -no-pie -m32 -o hello hello.o

hello.o: hello.asm
	nasm -felf32 hello.asm

clean:
	rm -f hello hello.o
