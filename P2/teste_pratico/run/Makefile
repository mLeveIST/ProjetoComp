CC=gcc -m32 # produce elf32 code in 64-bit machines
CFLAGS=-g -Dunderscore -fno-stack-protector # when __stack_chk_fail in Ubuntu

libminor.a: dbl.o linux.o lib.o sys.o expr.o
	ar crl $@ $^

expr.o: expr.c
	gcc -m32 -g -Dunderscore -fno-stack-protector -DPOSTFIX -c $< -o $@

%.o: %.asm
	nasm -felf32 $< -o $@

clean::
	rm -f libminor.a *.o expr.asm a.out core