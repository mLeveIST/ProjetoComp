minor: minor.y minor.l
	make -C lib
	#byacc -dv minor.y
	flex -dl minor.l
	gcc -Ilib lib/libutil.a lex.yy.c #y.tab.c

clean::
	make -C lib clean
	rm -f *.o minor lex.yy.c y.tab.c y.tab.h y.output
