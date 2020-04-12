minor: minor.y minor.l
	make -C lib
	byacc -dv minor.y
	flex -dl minor.l
	gcc lex.yy.c y.tab.c -Ilib lib/libutil.a

clean::
	make -C lib clean
	rm -f *.o minor lex.yy.c y.tab.c y.tab.h y.output
