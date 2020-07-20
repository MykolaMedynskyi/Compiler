make: komp.y komp.l
	bison -t -d komp.y
	flex komp.l
	g++ -std=c++11 -o compiler lex.yy.c komp.tab.c -lfl
