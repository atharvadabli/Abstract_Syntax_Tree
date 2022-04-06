open AST
(* User  declarations *)


%%
(* required declarations *)
%name While

%term
   CONST of string | NUM of int | ID of string
| WHILE | DOUBLECOLON | INT | BOOL | LEFTCURLY | RIGHTCURLY | NEWLINE
| ASSIGNMENT | READ | WRITE | IF | ELSE | ENDIF | THEN | DO | ENDWH | VARI
| LEFTPAREN | RIGHTPAREN | TRUE | FALSE | AND | OR | NOT | LT | GT | LTE | GTE | EOF 
| EQUAL | ADD | SUB | MUL | NOTEQUAL | DIV | MOD | COLON | SEMICOLON | COMMA | TILDE | MINUS | PLUS | statement | PROGR


%nonterm exp of AST.exp | VARIABLE of AST.id  | BLOCK of AST.block
        | DECLARATIONSEQ of AST.DEC
        | variablelist of AST.variablelist | COMMANDSEQ of AST.commandseq | TYPE of AST.Type | command of AST.command | PROGRAM of AST.program 
        | IDENTIFIER of AST.id | NUMERAL of int 
        
%pos int

(*optional declarations *)

%eop EOF
%noshift EOF


%left OR AND
%left EQUAL NOTEQUAL
%left GT LT GTE LTE
%left MINUS
%left PLUS
%left MUL
%left MOD DIV
%right NOT
%right TILDE


%start PROGRAM

%verbose

%%
  PROGRAM : PROGR IDENTIFIER DOUBLECOLON BLOCK (AST.PROG(IDENTIFIER, BLOCK))
  BLOCK : DECLARATIONSEQ LEFTCURLY COMMANDSEQ RIGHTCURLY (AST.BLK(DECLARATIONSEQ, COMMANDSEQ))

  DECLARATIONSEQ : VARI variablelist COLON TYPE SEMICOLON DECLARATIONSEQ (AST.DEC(variablelist, TYPE, DECLARATIONSEQ))
                 | (AST.dec_empty())
    NUMERAL : NUM (NUM) 

  (*VARIABLE : IDENTIFIER (IDENTIFIER)
*)
  
  IDENTIFIER : ID (AST.string_to_iden(ID))

  variablelist : IDENTIFIER COMMA variablelist (IDENTIFIER :: variablelist) 
                  | IDENTIFIER ([IDENTIFIER]) 
  TYPE : INT (AST.INT) | BOOL (AST.BOOL)
  COMMANDSEQ : command SEMICOLON COMMANDSEQ  (AST.SEQ(AST.com_seq_to_list(command, COMMANDSEQ)))
                | (AST.com_list_to_seq(AST.empty_command_sequence()))

  command : IDENTIFIER ASSIGNMENT exp (AST.SET(IDENTIFIER, exp))
          | READ IDENTIFIER (AST.read(IDENTIFIER))
          | WRITE exp (AST.write(exp))
          | IF exp THEN LEFTCURLY COMMANDSEQ RIGHTCURLY ELSE LEFTCURLY COMMANDSEQ RIGHTCURLY ENDIF (AST.ITE(exp, COMMANDSEQ1, COMMANDSEQ2))
          | WHILE exp DO LEFTCURLY COMMANDSEQ RIGHTCURLY ENDWH (AST.WH(exp, COMMANDSEQ))

  exp : TRUE (AST.bool(AST.TT))
      | FALSE (AST.bool(AST.FF))
      | NUMERAL (AST.num(NUMERAL))
      | TILDE exp (AST.Uniexp(exp, AST.TILDE))
      | LEFTPAREN exp RIGHTPAREN (exp)
      | NOT exp (AST.Uniexp(exp, AST.NOT))
      | PLUS exp (AST.plus_exp(exp))
      | exp PLUS exp (AST.Addexp( exp1, AST.PLUS, exp2))
      | exp MINUS exp (AST.Addexp( exp1, AST.MINUS, exp2))
      | exp MUL exp (AST.mulexp(exp1,AST.MUL, exp2))
      | exp DIV exp (AST.mulexp(exp1, AST.DIV, exp2))
      | exp MOD exp (AST.mulexp(exp1, AST.MOD, exp2))
      | exp GT exp (AST.Relexp( exp1, AST.GT, exp2))
      | exp LT exp (AST.Relexp( exp1, AST.LT, exp2))
      | exp GTE exp (AST.Relexp( exp1, AST.GEQ, exp2))
      | exp LTE exp (AST.Relexp(exp1, AST.LEQ,  exp2))
      | exp EQUAL exp (AST.Relexp( exp1, AST.EQ, exp2))
      | exp NOTEQUAL exp (AST.Relexp( exp1, AST.NEQ, exp2))
      | exp AND exp (AST.Relexp( exp1, AST.AND, exp2))
      | exp OR exp (AST.Relexp( exp1, AST.OR, exp2))
      | IDENTIFIER (AST.iden(IDENTIFIER))
      