structure Tokens= Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token
  type lexarg = string
  type arg = lexarg
  val linep = ref 1; (* Line pointer *)


  exception badCharacter
  val pos = ref 1
  val c = ref 1
  val out = ref "["
  val eof = fn () => (Tokens.EOF(!pos, !pos))
 val error : string * int * int -> unit = fn (e,l1,l2) => TextIO.output(TextIO.stdOut,"lex:line "^Int.toString l1^" l2="^Int.toString l2^": "^e^"\n")
 val badCh : int * char -> unit = fn(l1,ch) => TextIO.output(TextIO.stdOut,"lex:line "^Int.toString l1^": Invalid character "^Int.toString(ord ch)^"="^str(ch)^"\n")  val giveCol = fn () => !c

%%
%full

%header (functor WhileLexFun(structure Tokens: While_TOKENS));

alpha=[A-Za-z0-9];
digit=[0-9];
ws=[\ \t\n];

%%
\n => (lex());
{ws}+    => (lex());
{digit}+ => (c := !c + size(yytext) ; out := !out^"NUM \""^yytext^"\","; Tokens.NUM (List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext), !pos, !pos));

"while" => (Tokens.WHILE(!pos,!pos));
"var" => (Tokens.VARI(!pos,!pos));
"program" => (Tokens.PROGR(!pos,!pos));
"::" => (Tokens.DOUBLECOLON(!pos,!pos));
"int" => (Tokens.INT(!pos,!pos));
"bool" => (Tokens.BOOL(!pos,!pos));
"{" => (Tokens.LEFTCURLY(!pos,!pos));
"}" => (Tokens.RIGHTCURLY(!pos,!pos));
":=" => (Tokens.ASSIGNMENT(!pos,!pos));
"read" => (Tokens.READ(!pos,!pos));
"write" => (Tokens.WRITE(!pos,!pos));
"if" => (Tokens.IF(!pos,!pos));
"else" => (Tokens.ELSE(!pos,!pos));
"endif" => (Tokens.ENDIF(!pos,!pos));
"then" => (Tokens.THEN(!pos,!pos));
"do" => (Tokens.DO(!pos,!pos));
"endwh" => (Tokens.ENDWH(!pos,!pos));
"(" => (Tokens.LEFTPAREN(!pos,!pos));
")" => (Tokens.RIGHTPAREN(!pos,!pos));
"tt" => (Tokens.TRUE( !pos,!pos));
"ff" => (Tokens.FALSE(!pos,!pos));
"&&" => (Tokens.AND(!pos,!pos));
"||" => (Tokens.OR(!pos,!pos));
"!" => (Tokens.NOT(!pos,!pos));
"<" => (Tokens.LT(!pos,!pos));
">" => (Tokens.GT(!pos,!pos));
"<=" => (Tokens.LTE(!pos,!pos));
">=" => (Tokens.GTE(!pos,!pos));
"=" => (Tokens.EQUAL(!pos,!pos));
"+" => (Tokens.PLUS(!pos,!pos));
"-" => (Tokens.SUB(!pos,!pos));
"*" => (Tokens.MUL(!pos,!pos));
"<>" => (Tokens.NOTEQUAL(!pos,!pos));
"/" => (Tokens.DIV(!pos,!pos));
"%" => (Tokens.MOD(!pos,!pos));
":" => (Tokens.COLON(!pos,!pos));
";" => (Tokens.SEMICOLON(!pos,!pos));
"," => (Tokens.COMMA(!pos,!pos));
"~" => (Tokens.TILDE(!pos,!pos));
{alpha}+ => (c := !c + size(yytext) ; out := !out^"ID \""^yytext^"\","; Tokens.ID(yytext,!pos,!pos));
[^\n] => (lex());
.        => (error(yytext,!pos,!c); out := "["; c := 1; pos := 1; raise badCharacter; lex());
