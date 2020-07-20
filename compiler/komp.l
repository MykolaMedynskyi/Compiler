%option noyywrap
%option yylineno

%{
#include "komp.tab.h"
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <string>
using namespace std;
%}

%x COMMENT

%%


"[" { BEGIN(COMMENT); }

<COMMENT>"]"  { BEGIN(INITIAL); }
<COMMENT>.    {}

"DECLARE"     { return DECLARE; }
"BEGIN"       { return BEGINN; }
"END"         { return END; }

"IF"          { return IF; }
"THEN"        { return THEN; }
"ELSE"        { return ELSE; }
"ENDIF"       { return ENDIF; }

"WHILE"       { return WHILE; }
"DO"          { return DO; }
"ENDWHILE"    { return ENDWHILE; }
"ENDDO"       { return ENDDO; }

"FOR"         { return FOR; }
"FROM"        { return FROM; }
"TO"          { return TO; }
"DOWNTO"      { return DOWNTO; }
"ENDFOR"      { return ENDFOR; }

"READ"        { return READ; }
"WRITE"       { return WRITE; }

"PLUS"        { return PLUS; }
"MINUS"       { return MINUS; }
"TIMES"       { return TIMES; }
"DIV"         { return DIV; }
"MOD"         { return MOD; }

"ASSIGN"      { return ASSIGN; }
"EQ"          { return EQ; }
"NEQ"         { return NEQ; }
"LE"          { return LE; }
"GE"          { return GE; }
"LEQ"         { return LEQ; }
"GEQ"         { return GEQ; }

";"           { return SEMICOLON; }
","           { return COMMA; }

"("           { return OP; }
")"           { return CL; }
":"           { return COLON; }


([-]?[1-9][0-9]*)|[0] 	{ yylval.str = (char *)strdup(yytext); return num; }
[_a-z]+       { yylval.str = (char *)strdup(yytext); return pidentifier; }

[ \t\n]+      { }

.             { cout << "ERROR, line: " << yylineno << " unknown symbol: "<< yytext << "\n";
                   exit(1); }

%%