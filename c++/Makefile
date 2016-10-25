CC = gcc

C++ = g++

CFLAGS = -Wall -ansi -pedantic 

main: y.tab.o lex.yy.o main.o tree.h tree.o pretty.hh pretty.o eval.h eval.o memory.h memory.o search.o tree_interface.o actualMain.o
	$(CC) lex.yy.o y.tab.o tree.o pretty.o eval.o memory.o search.o tree_interface.o actualMain.o  main.o -g -o seq_search -ll -lstdc++

y.tab.c y.tab.h: tiny.y
	bison --yacc --defines tiny.y

lex.yy.c: tiny.l y.tab.h
	flex tiny.l
check:
	./check
clean:
	rm y.tab.* lex.yy.*; rm *.o; rm tiny
