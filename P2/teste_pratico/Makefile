minor: minor.y minor.l
	make -C lib
	byacc -dv minor.y
	flex -dl minor.l
	pburg -T minor.brg
	gcc lex.yy.c y.tab.c yyselect.c -Ilib lib/node.o lib/tabid.o lib/main.o -o minor
	make -C run
	-cp run/libminor.a libminor.a

out:: out.asm minor
	nasm -felf32 out.asm -o out.o
	ld -m elf_i386 out.o libminor.a -o out

clean::
	make -C lib clean
	make -C run clean
	rm -f *.o minor libminor.a lex.yy.c y.tab.c y.tab.h yyselect.c y.output *.out *.asm *~
