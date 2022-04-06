
all:
	ml-lex while.lex
	ml-yacc while.yacc
	sml loader.sml
clean:
	rm bool.lex.sml bool.yacc.desc bool.yacc.sig bool.yacc.sml
