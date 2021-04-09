hello: hello.o
	gcc -no-pie -m64 -o hello hello.o

hello.o: hello.asm
	nasm -felf64 hello.asm -l hello.lst

clean:
	rm -f hello hello.o
