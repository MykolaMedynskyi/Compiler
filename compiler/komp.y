%{
	#include <math.h>
  	#include <stdio.h>
 	#include <string.h>
	#include <stdlib.h>
  	#include <ctype.h>
  	#include <stdarg.h>
  	#include <iostream>
  	#include <fstream>
  	#include <string>
  	#include <map>
  	#include <vector>
  	#include <algorithm>

  	using namespace std;
	int yylex();
        extern int yylineno;
        extern FILE *yyin;

	long long pointer = 16;

	long long ifCounter = 0;

	vector<long long> ifs;
	vector<long long> elses;
	vector<long long> dos;
	vector<long long> whileDos;

	vector<long long> forjumps;

	struct value {
		bool isId;
		bool isPid;
		bool isPidPid;
		bool isPidNum;
		string name;
		string pointerS;
		long long pointerL;
		long long val;
	};

	value curVal;
	value preVal;
	value lastVal;


	struct var {
		long long value;
		bool isNull;
		long long pos;
		bool isTab;
		bool isFor;
		long long shift;
		long long size;
		long long start;
		long long end;
	};


        vector<string> code_stack;
        map<string, var> vars;

        int yyerror(const string str);
        void pc(string str);
        void check_name(string name);
        void check_name2(string name);
        void declare_variable(string name, bool isF);
        void declare_array(string name, long long from, long long to);
        void assign();
        void read();
	void write();
	void pplus();
	void mminus();
	void ttimes();
	void ddiv();
	void mmod();
	void prepare();
	void eelse();
	void wwhile();
	void ddo();

	void generate(long long value);
	void forceGenerate(long long value);

	void createNum(string num);
	void createPid(string name);
	void createPidPid(string name, string pointer);
	void createPidNum(string name, string pointer);
	void justValue();

	void ifOnly();
	void ifElse();
	void whileDo();
	void doWhile();

	void forTo(string name, string from, string to);
	void forDownto(string name, string from, string to);

	void endForTo(string name);
	void endForDownto(string name);

	void eq();
	void neq();
	void le();
	void ge();
	void leq();
	void geq();

	long long getPos(value v);

%}

%union {
    	char* str;
    	long long int num;
}

%token <str> num
%token <str> pidentifier

%token <str> DECLARE
%token <str> BEGINN
%token <str> END
%token <str> IF
%token <str> THEN
%token <str> ELSE
%token <str> ENDIF
%token <str> WHILE
%token <str> DO
%token <str> ENDWHILE
%token <str> ENDDO
%token <str> FOR
%token <str> FROM
%token <str> TO
%token <str> DOWNTO
%token <str> ENDFOR
%token <str> READ
%token <str> WRITE
%token <str> PLUS
%token <str> MINUS
%token <str> TIMES
%token <str> DIV
%token <str> MOD
%token <str> ASSIGN
%token <str> EQ
%token <str> NEQ
%token <str> LE
%token <str> GE
%token <str> LEQ
%token <str> GEQ
%token <str> SEMICOLON
%token <str> COMMA
%token <str> OP
%token <str> CL
%token <str> COLON

%type <str> value
%type <str> identifier
%type <str> expression
//%type <str> declarations
%type <str> command
%type <str> condition


%%

program: DECLARE declarations BEGINN commands END                 { pc("HALT"); }
	| BEGINN commands END                                     { pc("HALT"); }

declarations: declarations COMMA pidentifier			  { declare_variable($3, false); }
	| declarations COMMA pidentifier OP num COLON num CL	  { declare_array($3, atoll($5), atoll($7)); }
	| pidentifier 						  { declare_variable($1, false);  }
	| pidentifier OP num COLON num CL			  { declare_array($1, atoll($3), atoll($5)); }

commands: commands command					  {}
	| command						  {}

command: identifier ASSIGN expression SEMICOLON			  	  { assign(); }
	| IF condition THEN commands ELSE {eelse();} commands ENDIF	  { ifElse(); }
	| IF condition THEN commands ENDIF		   	  	  { ifOnly(); }
	| _while_ condition DO commands ENDWHILE	 	  	  { whileDo(); }
	| _do_ commands _while_ condition ENDDO 	  		  { doWhile(); }
	| FOR pidentifier FROM value TO value { forTo($2, $4, $6); } DO commands ENDFOR { endForTo($2); }
	| FOR pidentifier FROM value DOWNTO value  { forDownto($2, $4, $6); } DO commands ENDFOR { endForDownto($2); }
	| READ identifier SEMICOLON				  	  { read(); }
	| WRITE value SEMICOLON					  	  { write(); }

expression: value						  { justValue(); }
	| value PLUS value					  { pplus(); }
	| value MINUS value					  { mminus(); }
	| value TIMES value					  { ttimes(); }
	| value DIV value					  { ddiv(); }
	| value MOD value					  { mmod(); }

condition: value EQ value 					  { eq(); }
	| value NEQ value					  { neq(); }
	| value LE value					  { le(); }
	| value GE value					  { ge(); }
	| value LEQ value					  { leq(); }
	| value GEQ value					  { geq(); }

value: num							  { createNum($$); }
	| identifier						  {}

identifier: pidentifier						  { createPid($$); }
	| pidentifier OP pidentifier CL				  { createPidPid($$, $3); }
	| pidentifier OP num CL					  { createPidNum($$, $3); }

_while_: WHILE                                                    { wwhile(); }
_do_: DO							  { ddo(); }

%%

void endForDownto(string name){
	long long j = forjumps.back();
	forjumps.pop_back();


	pc("LOAD " + to_string(vars[name].pos));
	pc("DEC");
	pc("STORE " + to_string(vars[name].pos));
	pc("JUMP " + to_string(j));

	string v11 = "JUMPHERE " + name;
	string v12 = "JZERO " + to_string(code_stack.size());
	replace(code_stack.begin(), code_stack.end(), v11, v12);

	vars.erase(name);
}

void forDownto(string name, string from, string to) {
	if (vars[from].isNull && !vars[from].isTab) {
		yyerror(from + " not initialized");
	}
	if (vars[to].isNull && !vars[to].isTab) {
		yyerror(to + " not initialized");
	}
	declare_variable(name, true);

	prepare();
	pc("STORE " + to_string(vars[name].pos));
	pc("LOAD 5");

	pc("DEC");

	pc("STORE " + to_string(vars[name].pos+1));
	pointer++;

	pc("LOAD " + to_string(vars[name].pos));
	forjumps.push_back(code_stack.size());

	pc("SUB " + to_string(vars[name].pos+1));
	pc("JUMPHERE " + name);

}

void endForTo(string name) {

	long long j = forjumps.back();
	forjumps.pop_back();


	pc("LOAD " + to_string(vars[name].pos));
	pc("INC");
	pc("STORE " + to_string(vars[name].pos));
	pc("JUMP " + to_string(j));

	string v11 = "JUMPHERE " + name;
	string v12 = "JZERO " + to_string(code_stack.size());
	replace(code_stack.begin(), code_stack.end(), v11, v12);

	vars.erase(name);
}

void forTo(string name, string from, string to) {
	if (vars[from].isNull && !vars[from].isTab) {
		yyerror(from + " not initialized");
	}
	if (vars[to].isNull && !vars[to].isTab) {
		yyerror(to + " not initialized");
	}
	declare_variable(name, true);

	prepare();
	pc("STORE " + to_string(vars[name].pos));
	pc("LOAD 5");

	pc("INC");
	pc("STORE " + to_string(vars[name].pos+1));
	pointer++;

	pc("LOAD " + to_string(vars[name].pos));
	forjumps.push_back(code_stack.size());

	pc("SUB " + to_string(vars[name].pos+1));
	pc("JUMPHERE " + name);

}

void doWhile() {
	long long p = ifs.back();
	ifs.pop_back();

	whileDos.pop_back();

	long long d = dos.back();
	dos.pop_back();

	string v11 = "JNEG ENDIF " + to_string(p);
	string v12 = "JNEG " + to_string(code_stack.size()+1);
	string v21 = "JPOS ENDIF " + to_string(p);
	string v22 = "JPOS " + to_string(code_stack.size()+1);
	string v31 = "JZERO ENDIF " + to_string(p);
	string v32 = "JZERO " + to_string(code_stack.size()+1);
	replace(code_stack.begin(), code_stack.end(), v11, v12);
	replace(code_stack.begin(), code_stack.end(), v21, v22);
	replace(code_stack.begin(), code_stack.end(), v31, v32);
	pc("JUMP " + to_string(d));

}

void whileDo() {
	long long p = ifs.back();
	ifs.pop_back();

	long long w = whileDos.back();
	whileDos.pop_back();

	string v11 = "JNEG ENDIF " + to_string(p);
	string v12 = "JNEG " + to_string(code_stack.size()+1);
	string v21 = "JPOS ENDIF " + to_string(p);
	string v22 = "JPOS " + to_string(code_stack.size()+1);
	string v31 = "JZERO ENDIF " + to_string(p);
	string v32 = "JZERO " + to_string(code_stack.size()+1);
	replace(code_stack.begin(), code_stack.end(), v11, v12);
	replace(code_stack.begin(), code_stack.end(), v21, v22);
	replace(code_stack.begin(), code_stack.end(), v31, v32);

	pc("JUMP " + to_string(w));

}

void ddo() {
	dos.push_back(code_stack.size());
}

void wwhile() {
	whileDos.push_back(code_stack.size());
}

void eelse() {
	pc("JUMP HERE " + to_string(ifs.back()));
	elses.push_back(code_stack.size());
}

void ifElse() {
	long long p = ifs.back();
	ifs.pop_back();

	long long e = elses.back();
	elses.pop_back();

	string v11 = "JNEG ENDIF " + to_string(p);
	string v12 = "JNEG " + to_string(e);
	string v21 = "JPOS ENDIF " + to_string(p);
	string v22 = "JPOS " + to_string(e);
	string v31 = "JZERO ENDIF " + to_string(p);
	string v32 = "JZERO " + to_string(e);
	replace(code_stack.begin(), code_stack.end(), v11, v12);
	replace(code_stack.begin(), code_stack.end(), v21, v22);
	replace(code_stack.begin(), code_stack.end(), v31, v32);

	string v41 = "JUMP HERE " + to_string(p);
	string v42 = "JUMP " + to_string(code_stack.size());
	replace(code_stack.begin(), code_stack.end(), v41, v42);

}

void ifOnly() {
	long long p = ifs.back();
	ifs.pop_back();
	string v11 = "JNEG ENDIF " + to_string(p);
	string v12 = "JNEG " + to_string(code_stack.size());
	string v21 = "JPOS ENDIF " + to_string(p);
	string v22 = "JPOS " + to_string(code_stack.size());
	string v31 = "JZERO ENDIF " + to_string(p);
	string v32 = "JZERO " + to_string(code_stack.size());
	replace(code_stack.begin(), code_stack.end(), v11, v12);
	replace(code_stack.begin(), code_stack.end(), v21, v22);
	replace(code_stack.begin(), code_stack.end(), v31, v32);

}

void leq() {
	ifs.push_back(ifCounter);
	prepare();
	pc("SUB 5");
	pc("JPOS ENDIF " + to_string(ifCounter));

	ifCounter++;
}

void geq() {
	ifs.push_back(ifCounter);
	prepare();
	pc("SUB 5");
	pc("JNEG ENDIF " + to_string(ifCounter));

	ifCounter++;
}

void ge() {
	ifs.push_back(ifCounter);
	prepare();
	pc("SUB 5");
	pc("JNEG ENDIF " + to_string(ifCounter));
	pc("JZERO ENDIF " + to_string(ifCounter));

	ifCounter++;
}

void le() {
	ifs.push_back(ifCounter);
	prepare();
	pc("SUB 5");
	pc("JPOS ENDIF " + to_string(ifCounter));
	pc("JZERO ENDIF " + to_string(ifCounter));

	ifCounter++;
}

void neq() {
	ifs.push_back(ifCounter);
	prepare();
	pc("SUB 5");
	pc("JZERO ENDIF " + to_string(ifCounter));

	ifCounter++;
}

void eq() {
	ifs.push_back(ifCounter);
	prepare();
	pc("SUB 5");
	pc("JNEG ENDIF " + to_string(ifCounter));
	pc("JPOS ENDIF " + to_string(ifCounter));

	ifCounter++;
}

void justValue() {
	if (curVal.isId) {
		long long pos = vars[curVal.name].pos;
		if (pos != 0) {
			pc("LOAD " + to_string(pos));
			if (curVal.val < 0) {
				pc("SUB " + to_string(pos));
				pc("SUB " + to_string(pos));
			}
		} else {
			pc("SUB 0");
		}
	} else if (curVal.isPid) {
		if (vars[curVal.name].isNull) {
			yyerror(curVal.name + " not initialized");
		}

		long long pos = vars[curVal.name].pos;
		pc("LOAD " + to_string(pos));
	} else if (curVal.isPidPid) {
		if (!vars[curVal.name].isTab) {
			yyerror("wrong type: "+ curVal.name + " is not array");
		}
		pc("LOAD " + to_string(vars[curVal.pointerS].pos));
		pc("ADD " + to_string(vars[curVal.name].pos));
		pc("STORE 5");
		pc("LOADI 5");
	} else {
		check_name2(curVal.name);
		bool neg = false;

		if (curVal.pointerL < 0) {
			neg = true;
		}
		if (neg) {
			long long p = -curVal.pointerL;
			pc("LOAD " + to_string(vars[to_string(p)].pos));
			pc("SUB " + to_string(vars[to_string(p)].pos));
			pc("SUB " + to_string(vars[to_string(p)].pos));
		} else if (curVal.pointerL == 0) {
			pc("SUB 0");
		} else {
			pc("LOAD " + to_string(vars[to_string(curVal.pointerL)].pos));
		}

		pc("ADD " + to_string(vars[curVal.name].pos));
		pc("STORE 5");
		pc("LOADI 5");
	}
	lastVal = preVal;
	pc("STORE 4");
}

void createPidNum(string name, string pointer) {
	check_name2(name);

	var vr = vars[name];
	if (!vr.isTab) {
		yyerror("wrong type: "+ name + " is not array");
	}
	if (atoll(pointer.c_str()) < vr.start || atoll(pointer.c_str()) > vr.end) {
		yyerror("Array index out of bounds");
	}

	value v;
	v.isId = false;
	v.isPid = false;
	v.isPidPid = false;
	v.isPidNum = true;
	v.name = name;
	v.pointerL = atoll(pointer.c_str());

	generate(v.pointerL);

	lastVal = preVal;
	preVal = curVal;
	curVal = v;
}

void createPidPid(string name, string pointer) {
	check_name2(name);
	check_name2(pointer);
	value v;
	v.isId = false;
	v.isPid = false;
	v.isPidPid = true;
	v.isPidNum = false;
	v.name = name;
	v.pointerS = pointer;

	lastVal = preVal;
	preVal = curVal;
	curVal = v;
}

void createPid(string name) {

	check_name2(name);
	value v;
	v.isId = false;
	v.isPid = true;
	v.isPidPid = false;
	v.isPidNum = false;
	v.name = name;

	lastVal = preVal;
	preVal = curVal;
	curVal = v;
}

void createNum(string num) {
	long long val = atoll(num.c_str());
	value v;

	if (val > 0) {
		v.name = num;
	} else {
		v.name = to_string(-val);
	}

	v.isId = true;
	v.isPid = false;
	v.isPidPid = false;
	v.isPidNum = false;
	v.val = val;
	generate(val);

	lastVal = preVal;
	preVal = curVal;
	curVal = v;
}

void mmod() {
	pc("SUB 0");
	pc("STORE 4");
	prepare();  // K = A % B =>    B - 5;  A - 0
	pc("JZERO " + to_string(code_stack.size() + 1  + 73));
	pc("JPOS " + to_string(code_stack.size() + 1  + 7));
	pc("STORE 6");
	pc("SUB 6");
	pc("SUB 6");
	pc("STORE 4");
	pc("SUB 0");
	pc("DEC");
	pc("JUMP " + to_string(code_stack.size() + 1  + 4));
	pc("STORE 6");
	pc("STORE 4");
	pc("SUB 0");
	pc("INC");
	pc("STORE 7");
	pc("LOAD 5");
	pc("JZERO " + to_string(code_stack.size() + 1  + 58));
	pc("JPOS " + to_string(code_stack.size() + 1  + 7));
	pc("SUB 5");
	pc("SUB 5");
	pc("STORE 5");
	pc("STORE 8");
	pc("LOAD 7");
	pc("DEC");
	pc("JUMP " + to_string(code_stack.size() + 1  + 3));
	pc("STORE 8");
	pc("LOAD 7");
	pc("INC");
	pc("STORE 7");
	pc("LOAD 4");
	pc("SUB 8");
	pc("JNEG " + to_string(code_stack.size() + 1  + 26));
	pc("LOAD 4");
	pc("SUB 8");
	pc("JNEG " + to_string(code_stack.size() + 1  + 8));
	pc("JZERO " + to_string(code_stack.size() + 1  + 39));
	pc("LOAD 8");
	pc("SHIFT 1");
	pc("STORE 8");
	pc("LOAD 9");
	pc("SHIFT 1");
	pc("STORE 9");
	pc("JUMP " + to_string(code_stack.size() + 1  - 11));
	pc("LOAD 8");
	pc("SHIFT 2");
	pc("STORE 8");
	pc("SUB 0");
	pc("INC");
	pc("STORE 9");
	pc("LOAD 4");
	pc("SUB 8");
	pc("STORE 4");
	pc("LOAD 5");
	pc("STORE 8");
	pc("LOAD 4");
	pc("SUB 8");
	pc("JPOS " + to_string(code_stack.size() + 1  - 21));
	pc("JZERO " + to_string(code_stack.size() + 1  + 17));
	pc("LOAD 7");
	pc("JPOS " + to_string(code_stack.size() + 1  + 13));
	pc("JNEG " + to_string(code_stack.size() + 1  + 8));
	pc("LOAD 6");
	pc("JPOS " + to_string(code_stack.size() + 1  + 3));
	pc("LOAD 5");
	pc("SUB 4");
	pc("JUMP " + to_string(code_stack.size() + 1  + 10));
	pc("LOAD 4");
	pc("SUB 8");
	pc("JUMP " + to_string(code_stack.size() + 1  + 7));
	pc("LOAD 4");
	pc("SUB 4");
	pc("SUB 4");
	pc("JUMP " + to_string(code_stack.size() + 1  + 3));
	pc("LOAD 4");
	pc("JUMP " + to_string(code_stack.size() + 1  + 1));
	pc("SUB 0");
	pc("STORE 4");

}

void ddiv() {
	pc("SUB 0");
	pc("STORE 4");
	pc("INC");
	pc("STORE 10");
	prepare();  // K = A // B =>    B - 5;  A - 0
	pc("JZERO " + to_string(code_stack.size() + 1  + 83));
	pc("JPOS " + to_string(code_stack.size() + 1  + 8));
	pc("STORE 6");
	pc("SUB 6");
	pc("SUB 6");
	pc("STORE 6");
	pc("STORE 9");
	pc("SUB 0");
	pc("DEC");
	pc("JUMP " + to_string(code_stack.size() + 1  + 4));
	pc("STORE 6");
	pc("STORE 9");
	pc("SUB 0");
	pc("INC");
	pc("STORE 7");

	pc("LOAD 5");
	pc("JZERO " + to_string(code_stack.size() + 1  + 67));
	pc("JPOS " + to_string(code_stack.size() + 1  + 7));
	pc("SUB 5");
	pc("SUB 5");
	pc("STORE 5");
	pc("STORE 8");
	pc("LOAD 7");
	pc("DEC");
	pc("JUMP " + to_string(code_stack.size() + 1  + 3));
	pc("STORE 8");
	pc("LOAD 7");
	pc("INC");
	pc("STORE 7");

	pc("LOAD 9");
	pc("SUB 8");
	pc("JNEG " + to_string(code_stack.size() + 1  + 8));
	pc("JZERO " + to_string(code_stack.size() + 1  + 37));
	pc("LOAD 8");
	pc("SHIFT 1");
	pc("STORE 8");
	pc("LOAD 10");
	pc("SHIFT 1");
	pc("STORE 10");
	pc("JUMP " + to_string(code_stack.size() + 1  - 11));
	pc("LOAD 8");
	pc("SHIFT 2");
	pc("STORE 8");
	pc("LOAD 10");
	pc("SHIFT 2");
	pc("STORE 10");
	pc("LOAD 4");
	pc("ADD 10");
	pc("STORE 4");
	pc("SUB 0");
	pc("INC");
	pc("STORE 10");
	pc("LOAD 9");
	pc("SUB 8");
	pc("STORE 9");
	pc("LOAD 5");
	pc("STORE 8");
	pc("LOAD 9");
	pc("SUB 8");
	pc("JPOS " + to_string(code_stack.size() + 1  - 27));
	pc("JZERO " + to_string(code_stack.size() + 1  + 9));
	pc("LOAD 7");
	pc("JPOS " + to_string(code_stack.size() + 1  + 23));
	pc("JNEG " + to_string(code_stack.size() + 1  + 22));
	pc("LOAD 4");
	pc("SUB 4");
	pc("SUB 4");
	pc("DEC");
	pc("STORE 4");
	pc("JUMP " + to_string(code_stack.size() + 1  + 9));
	pc("LOAD 7");
	pc("JPOS " + to_string(code_stack.size() + 1  + 8));
	pc("JNEG " + to_string(code_stack.size() + 1  + 7));
	pc("LOAD 4");
	pc("ADD 10");
	pc("STORE 4");
	pc("SUB 4");
	pc("SUB 4");
	pc("STORE 4");
	pc("JUMP " + to_string(code_stack.size() + 1  + 7));
	pc("LOAD 4");
	pc("ADD 10");
	pc("STORE 4");
	pc("JUMP " + to_string(code_stack.size() + 1  + 3));
	pc("SUB 0");
	pc("STORE 4");
	pc("LOAD 4");

}

void ttimes() {
	pc("SUB 0");
	pc("STORE 4");
	prepare(); // a = b * c =>    c - 5;  b - 0
	pc("JZERO " + to_string(code_stack.size() + 1  + 60));
	pc("JPOS " + to_string(code_stack.size() + 1  + 7));
	pc("STORE 6");
	pc("SUB 6");
	pc("SUB 6");
	pc("STORE 6");
	pc("SUB 0");
	pc("DEC");
	pc("JUMP " + to_string(code_stack.size() + 1  + 3));
	pc("STORE 6");
	pc("SUB 0");
	pc("INC");
	pc("STORE 7");

	pc("LOAD 5");
	pc("JZERO " + to_string(code_stack.size() + 1  + 46));
	pc("JPOS " + to_string(code_stack.size() + 1  + 6));
	pc("SUB 5");
	pc("SUB 5");
	pc("STORE 5");
	pc("LOAD 7");
	pc("DEC");
	pc("JUMP " + to_string(code_stack.size() + 1  + 2));
	pc("LOAD 7");
	pc("INC");
	pc("STORE 7");

	pc("LOAD 6");
	pc("DEC");
	pc("JZERO " + to_string(code_stack.size() + 1  + 18));
	pc("INC");
	pc("SHIFT 2");
	pc("SHIFT 1");
	pc("SUB 6");
	pc("JZERO " + to_string(code_stack.size() + 1  + 6));
	pc("LOAD 4");
	pc("ADD 5");
	pc("STORE 4");
	pc("LOAD 6");
	pc("DEC");
	pc("STORE 6");
	pc("LOAD 5");
	pc("SHIFT 1");
	pc("STORE 5");
	pc("LOAD 6");
	pc("SHIFT 2");
	pc("STORE 6");
	pc("JUMP " + to_string(code_stack.size() + 1  - 20));
	pc("LOAD 4");
	pc("ADD 5");
	pc("STORE 4");
	pc("LOAD 7");
	pc("JPOS " + to_string(code_stack.size() + 1  + 6));
	pc("JNEG " + to_string(code_stack.size() + 1  + 5));
	pc("LOAD 4");
	pc("SUB 4");
	pc("SUB 4");
	pc("STORE 4");
	pc("JUMP " + to_string(code_stack.size() + 1  + 4));
	pc("LOAD 4");
	pc("JUMP " + to_string(code_stack.size() + 1  + 2));
	pc("SUB 0");
	pc("STORE 4");

}

void mminus() {
	prepare(); // a = b - c =>    c - 5;  b - 0
	pc("SUB 5");
	pc("STORE 4");
}

void pplus() {
	prepare(); // a = b + c =>    c - 5;  b - 0
	pc("ADD 5");
	pc("STORE 4");

}

void prepare() {
	if (curVal.isId) {
		if (curVal.val > 1) {
			pc("LOAD " + to_string(vars[curVal.name].pos));
		} else if (curVal.val < 0) {
			pc("LOAD " + to_string(vars[curVal.name].pos));
			pc("SUB " + to_string(vars[curVal.name].pos));
			pc("SUB " + to_string(vars[curVal.name].pos));
		} else if (curVal.val == 0) {
			pc("SUB 0");
		} else {
			pc("SUB 0");
			pc("INC");
		}
	} else if (curVal.isPid) {
		if (vars[curVal.name].isTab) {
			yyerror("wrong type: "+ curVal.name + " shold be array");
		}
		if (vars[curVal.name].isNull) {
			yyerror(curVal.name + " not initialized");
		}
		check_name2(curVal.name);
		pc("LOAD " + to_string(vars[curVal.name].pos));
	} else if (curVal.isPidPid) {
		if (!vars[curVal.name].isTab) {
			yyerror("wrong type: "+ curVal.name + " is not array");
		}
		check_name2(curVal.name);
		if (vars[curVal.pointerS].isNull) {
			yyerror(curVal.pointerS + " not initialized");
		}
		pc("LOAD " + to_string(vars[curVal.pointerS].pos));
		pc("ADD " + to_string(vars[curVal.name].pos));
		pc("STORE 3");
		pc("LOADI 3");
	} else {
		if (!vars[curVal.name].isTab) {
			yyerror("wrong type: "+ curVal.name + " is not array");
		}
		check_name2(curVal.name);
		bool neg = false;

		if (curVal.pointerL < 0) {
			neg = true;
		}
		if (neg) {
			long long p = -curVal.pointerL;
			pc("LOAD " + to_string(vars[to_string(p)].pos));
			pc("SUB " + to_string(vars[to_string(p)].pos));
			pc("SUB " + to_string(vars[to_string(p)].pos));
		} else if (curVal.pointerL == 0) {
			pc("SUB 0");
                } else {
			pc("LOAD " + to_string(vars[to_string(curVal.pointerL)].pos));
		}

		pc("ADD " + to_string(vars[curVal.name].pos));
		pc("STORE 3");
		pc("LOADI 3");
	}
	pc("STORE 5");


	if (preVal.isId) {
		if (preVal.val > 0) {
			pc("LOAD " + to_string(vars[preVal.name].pos));
		} else if (preVal.val < 0) {
			pc("LOAD " + to_string(vars[preVal.name].pos));
			pc("SUB " + to_string(vars[preVal.name].pos));
			pc("SUB " + to_string(vars[preVal.name].pos));
		} else {
			pc("SUB 0");
		}
	} else if (preVal.isPid) {
		if (vars[preVal.name].isTab) {
			yyerror("wrong type: "+ preVal.name + " shold be array");
		}
		if (vars[preVal.name].isNull) {
			yyerror(preVal.name + " not initialized");
		}
		check_name2(preVal.name);
		pc("LOAD " + to_string(vars[preVal.name].pos));
	} else if (preVal.isPidPid) {
		if (!vars[preVal.name].isTab) {
			yyerror("wrong type: "+ preVal.name + " is not array");
		}
		check_name2(preVal.name);
		if (vars[preVal.pointerS].isNull) {
			yyerror(preVal.pointerS + " not initialized");
		}
		pc("LOAD " + to_string(vars[preVal.pointerS].pos));
		pc("ADD " + to_string(vars[preVal.name].pos));
		pc("STORE 3");
		pc("LOADI 3");
	} else {
		if (!vars[preVal.name].isTab) {
			yyerror("wrong type: "+ preVal.name + " is not array");
		}
		check_name2(preVal.name);
		bool neg = false;

		if (preVal.pointerL < 0) {
			neg = true;
		}
		if (neg) {
			long long p = -preVal.pointerL;
			pc("LOAD " + to_string(vars[to_string(p)].pos));
			pc("SUB " + to_string(vars[to_string(p)].pos));
			pc("SUB " + to_string(vars[to_string(p)].pos));
		} else if (preVal.pointerL == 0) {
			pc("SUB 0");
		} else {
			pc("LOAD " + to_string(vars[to_string(preVal.pointerL)].pos));
		}

		pc("ADD " + to_string(vars[preVal.name].pos));
		pc("STORE 3");
		pc("LOADI 3");
	}
}

void write() {

	if (curVal.isId) {
		if (curVal.val > 0) {
			pc("LOAD " + to_string(vars[curVal.name].pos));
		} else if (curVal.val < 0) {
			pc("LOAD " + to_string(vars[curVal.name].pos));
			pc("SUB " + to_string(vars[curVal.name].pos));
			pc("SUB " + to_string(vars[curVal.name].pos));
		}
	} else if (curVal.isPid) {
		if (vars[curVal.name].isTab) {
			yyerror("wrong type: "+ curVal.name + " shold be array");
		}
		if (vars[curVal.name].isNull) {
			yyerror(curVal.name + " not initialized");
		}
		check_name2(curVal.name);
		pc("LOAD " + to_string(vars[curVal.name].pos));
	} else if (curVal.isPidPid) {
		if (!vars[curVal.name].isTab) {
			yyerror("wrong type: "+ curVal.name + " is not array");
		}
		check_name2(curVal.name);
		if (vars[curVal.pointerS].isNull) {
			yyerror(curVal.pointerS + " not initialized");
		}
		pc("LOAD " + to_string(vars[curVal.pointerS].pos));
		pc("ADD " + to_string(vars[curVal.name].pos));
		pc("STORE 3");
		pc("LOADI 3");
	} else {
		if (!vars[curVal.name].isTab) {
			yyerror("wrong type: "+ curVal.name + " is not array");
		}
		check_name2(curVal.name);
		bool neg = false;

		if (curVal.pointerL < 0) {
			neg = true;
		}
		if (neg) {
			long long p = -curVal.pointerL;
			pc("LOAD " + to_string(vars[to_string(p)].pos));
			pc("SUB " + to_string(vars[to_string(p)].pos));
			pc("SUB " + to_string(vars[to_string(p)].pos));
		} else if (curVal.pointerL == 0) {
			pc("SUB 0");
		} else {
			pc("LOAD " + to_string(vars[to_string(curVal.pointerL)].pos));
		}

		pc("ADD " + to_string(vars[curVal.name].pos));
		pc("STORE 3");
		pc("LOADI 3");
	}


	pc("PUT");
}


void read(){
	check_name2(curVal.name);

	if (curVal.isPid) {
		pc("GET");
		var v = vars[curVal.name];
		if (v.isTab) {
			yyerror("wrong type: "+ curVal.name + " shold be array");
		}
		vars[curVal.name].isNull = false;

		pc("STORE " + to_string(vars[curVal.name].pos));
	} else if (curVal.isPidPid) {
		var v = vars[curVal.name];
		if (!v.isTab) {
			yyerror("wrong type: "+ curVal.name + " is not array");
		}
		if (vars[curVal.pointerS].isNull) {
			yyerror(curVal.pointerS + " not initialized");
		}
		pc("LOAD " + to_string(vars[curVal.pointerS].pos));
		pc("ADD " + to_string(v.pos));
		pc("STORE 3");
		pc("GET");
		pc("STOREI 3");

	} else {
		var v = vars[curVal.name];
		bool neg = false;
		if (!v.isTab) {
			yyerror("wrong type: "+ curVal.name + " is not array");
		}

		if (curVal.pointerL < 0) {
			neg = true;
		}
		if (neg) {
			long long p = -curVal.pointerL;
			pc("LOAD " + to_string(vars[to_string(p)].pos));
			pc("SUB " + to_string(vars[to_string(p)].pos));
			pc("SUB " + to_string(vars[to_string(p)].pos));
		} else if (curVal.pointerL == 0) {
			pc("SUB 0");
		}  else {
			pc("LOAD " + to_string(vars[to_string(curVal.pointerL)].pos));
		}

		pc("ADD " + to_string(v.pos));
		pc("STORE 3");
		pc("GET");
		pc("STOREI 3");
	}
}

void assign() {
	value v = lastVal;
	check_name2(v.name);
	if (v.isPid) {
		if (vars[v.name].isFor) {
			yyerror("you cannt change " + v.name);
		}
		if (vars[v.name].isTab) {
			yyerror("wrong type: "+ v.name + " shold be array");
		}
		vars[v.name].isNull = false;
		pc("STORE " + to_string(vars[v.name].pos));
	} else if (v.isPidPid) {
		check_name2(v.pointerS);
		if (!vars[v.name].isTab) {
			yyerror("wrong type: "+ v.name + " is not array");
		}
		if (vars[v.pointerS].isNull) {
			yyerror(v.pointerS + " not initialized");
		}
		pc("LOAD " + to_string(vars[v.pointerS].pos));
		pc("ADD " + to_string(vars[v.name].pos));
		pc("STORE 3");
		pc("LOAD 4");
		pc("STOREI 3");
	} else {
		if (!vars[v.name].isTab) {
			yyerror("wrong type: "+ v.name + " is not array");
		}
		bool neg = false;
		if (v.pointerL < 0) {
			neg = true;
		}
		if (neg) {
			long long p = -v.pointerL;
			pc("LOAD " + to_string(vars[to_string(p)].pos));
			pc("SUB " + to_string(vars[to_string(p)].pos));
			pc("SUB " + to_string(vars[to_string(p)].pos));
		} else if (v.pointerL == 0) {
			pc("SUB 0");
		} else {
			pc("LOAD " + to_string(vars[to_string(v.pointerL)].pos));
		}

		pc("ADD " + to_string(vars[v.name].pos));
		pc("STORE 3");
		pc("LOAD 4");
		pc("STOREI 3");
	}
}

void declare_array(string name, long long int from, long long int to) {
	check_name(name);
	if (from > to) {
		yyerror("impossible table " + name);
	}
	var v;
	v.pos = pointer;
	v.isTab = true;
	v.size = to - from + 1;
	v.start = from;
	v.end = to;
	v.shift = v.pos - v.start + 1;
	vars[name] = v;

	forceGenerate(v.shift);

	pointer += to - from + 2;
}

void declare_variable(string name, bool isF) {
	check_name(name);
	var v;
	v.pos = pointer;
	v.isTab = false;
	v.size = 1;
	v.isNull = true;
	v.isFor = isF;
	if (isF) {
		v.isNull = false;
	}
	vars[name] = v;

	pointer++;
}

void forceGenerate(long long value) {
	string name = to_string(value);
	if (vars.count(name) >= 1) {
		pc("LOAD " + to_string(vars[name].pos));
		pc("STORE " + to_string(pointer));
		return;
	}

	vector<int> tmp;
	pc("SUB 0");
	if (value == 0) {
		return;
	}
	while(value != 0) {
		tmp.push_back(value%2);
		value /=2;
	}

	pc("INC");

	for(int i = tmp.size()-2; i >= 0; i--) {
		pc("SHIFT 1");
		if (tmp[i] == 1) {
			pc("INC");
		}

	}

	var v;
	v.pos = pointer;
	v.isTab = false;
	v.isNull = false;
	vars[name] = v;
	pc("STORE " + to_string(v.pos));

}

void generate(long long value) {
	if (value < 0) {
		value = -value;
	}
	string name = to_string(value);
	if (vars.count(name) >= 1) {
		return;
	}

	vector<int> tmp;
	pc("SUB 0");
	if (value == 0) {
		return;
	}
	while(value != 0) {
		tmp.push_back(value%2);
		value /=2;
	}

	pc("INC");

	for(int i = tmp.size()-2; i >= 0; i--) {
		pc("SHIFT 1");
		if (tmp[i] == 1) {
			pc("INC");
		}

	}

	var v;
	v.pos = pointer;
	v.isTab = false;
	v.isNull = false;
	vars[name] = v;
	pointer++;
	pc("STORE " + to_string(v.pos));
}



void check_name(string name) {
	if (vars.count(name) == 1) {
		yyerror("variable "+ name + "  already exist");
	}
}

void check_name2(string name) {
	if (vars.count(name) == 0) {
		yyerror("variable "+ name + " is not declared");
	}
}

void pc(string str) {
	code_stack.push_back(str);
}

int main(int argc, char* argv[]){

	if (argc != 3) {
		cout << "ERROR, wrong number arguments. \n";
		exit(1);
	}
	yyin = fopen(argv[1],"r");
	if (!yyin) {
		cout << "ERROR, cann't open input file \n";
	}

	pc("SUB 0");
	pc("STORE 11");
	pc("STORE 4");
	pc("INC");
	pc("STORE 1");
	pc("STORE 10");
	pc("DEC");
	pc("DEC");
	pc("STORE 2");

	yyparse();

	ofstream out_code(argv[2]);

	for(long long int i = 0; i < (long long int) code_stack.size(); i++) {
                out_code << code_stack.at(i) << endl;
	}




	return 0;
}

int yyerror(string str){
    cout << "ERROR, line: " << yylineno << " "<< str << "\n";
    exit(1);
}