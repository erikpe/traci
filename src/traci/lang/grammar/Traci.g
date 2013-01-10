grammar Traci;

options {
    output=AST;
    ASTLabelType=CommonTree;
}

tokens {
    ARGS;
    VECTOR;
    FUNCALL;
    REF;
    UNARY_OP;
    BLOCK;
    ASSIGN;
    GLOBAL_ASSIGN;
}

@parser::header {
package traci.lang.parser;

import java.util.HashMap;
import java.util.Map;
}

@lexer::header {
package traci.lang.parser;

import traci.lang.grammar.TraciToken;
import traci.lang.grammar.Location;
}

@lexer::members {
private String currentFilename = null;
private final Stack<Location> includeStack = new Stack<Location>();

public Token emit() {
    TraciToken tok = new TraciToken(input, state.type, state.channel, state.tokenStartCharIndex, getCharIndex() - 1, currentFilename, includeStack);
    tok.setLine(state.tokenStartLine);
    tok.setText(state.text);
    tok.setCharPositionInLine(state.tokenStartCharPositionInLine);
    emit(tok);
    return tok;
}

public void ppLine(String rowStr, String filename, String actionStr) {
    final int row = Integer.parseInt(rowStr);
    final int action = Integer.parseInt(actionStr);
    if (action == 1) {
        if (currentFilename != null) {
            includeStack.push(new Location(currentFilename, input.getLine(), 0));
        }
        input.setLine(0);
    }
    else if (action == 2) {
        final Location location = includeStack.pop();
        input.setLine(location.row);
    }
    currentFilename = filename;
}
}

scene
    : (function | statement)* EOF -> ^(BLOCK function* statement*)
    ;

function
    : DEF^ ID function_def_args block
    ;

function_def_args
    : '(' ( ID ( ',' ID )* )? ')' -> ^(ARGS ID*)
    ;

block
    : '{' (function | statement)* '}' -> ^(BLOCK function* statement*)
    ;

statement
    : IF '(' expr ')' block (ELSE block)?    -> ^(IF expr block block?)
    | WHILE '(' expr ')' block               -> ^(WHILE expr block)
    | FOR '(' ID IN expr DOTS expr ')' block -> ^(FOR ID expr expr block)
    | assignable_statement
    | RETURN assignable_statement            -> ^(RETURN assignable_statement)
    | GLOBAL ID '=' assignable_statement     -> ^(GLOBAL_ASSIGN ID assignable_statement)
    | ID '=' assignable_statement            -> ^(ASSIGN ID assignable_statement)
    ;

assignable_statement
    : (ID '{')=>id_statement
    | (ID function_call_args '{')=>function_call_statement
    | PRIMITIVE_SHAPE function_call_args? (block | ';') -> ^(PRIMITIVE_SHAPE function_call_args? block?)
    | CSG_SHAPE function_call_args? (block | ';')       -> ^(CSG_SHAPE function_call_args? block?)
    | BBOX function_call_args? (block | ';')            -> ^(BBOX function_call_args? block?)
    | TRANSFORMATION expr ';'                           -> ^(TRANSFORMATION expr)
    | COLOR expr ';'                                    -> ^(COLOR expr)
    | expr ';'!
    ;

id_statement
    : ID block -> ^(REF ID block)
    ;

function_call_statement
    : ID function_call_args block -> ^(FUNCALL ID function_call_args block)
    ;

expr
    : conditional_expr
    ;

conditional_expr
    : addition_expr ( ( '<'^ | '>'^ | '<='^ | '>='^ | '=='^ | '!='^ ) addition_expr )?
    ;

addition_expr
    : multiplication_expr ( ('+'^ | '-'^ ) multiplication_expr )*
    ;

unary_expr
    : primary_expr
    | '-' unary_expr -> ^(UNARY_OP '-' unary_expr)
    | '+' unary_expr -> ^(UNARY_OP '+' unary_expr)
    | '!' unary_expr -> ^(UNARY_OP '!' unary_expr)
    ;

multiplication_expr
    : unary_expr ( ( '*'^ | '/'^ ) unary_expr )*
    ;

primary_expr
    : constant
    | function_call
    | variable_reference
    | vector
    | '('! expr ')'!
    ;

constant
    : FLOAT
    | INT
    ;

function_call
    : ID function_call_args -> ^(FUNCALL ID function_call_args)
    ;

function_call_args
    : '(' ( expr ( ',' expr )* )? ')' -> ^(ARGS expr*)
    ;

variable_reference
    : ID -> ^(REF ID)
    ;

vector 
    : '[' expr ',' expr ',' expr ']' -> ^(VECTOR expr*)
    ;

DEF : 'def';
RETURN : 'return';
GLOBAL : 'global';
WHILE : 'while';
IF : 'if';
ELSE : 'else';
BBOX : 'bbox';
FOR : 'for';
IN : 'in';
DOTS : '..';

PRIMITIVE_SHAPE
    :	( 'box' | 'cylinder' | 'plane' | 'sphere' | 'torus' )
    ;

CSG_SHAPE
    :	( 'union' | 'difference' | 'intersection' )
    ;

TRANSFORMATION
    :	( 'translate' | 'scale' | 'rotate' | 'rotx' | 'roty' | 'rotz' )
    ;

COLOR
    :	( 'color' )
    ;

ID  :	('a'..'z'|'A'..'Z'|'_') ('a'..'z'|'A'..'Z'|'0'..'9'|'_')*
    ;

INT :	'0'..'9'+
    ;

FLOAT
    :   ('0'..'9')+ '.' ('0'..'9')* EXPONENT?
    |   '.' ('0'..'9')+ EXPONENT?
    |   ('0'..'9')+ EXPONENT
    ;

COMMENT
    :   '//' ~('\n'|'\r')* '\r'? '\n' {$channel=HIDDEN;}
    |   '/*' ( options {greedy=false;} : . )* '*/' {$channel=HIDDEN;}
    ;

WS  :   ( ' ' | '\t' | '\r' | '\n' ) {$channel=HIDDEN;}
    ;

fragment
EXPONENT
    :	('e'|'E') ('+'|'-')? ('0'..'9')+ ;

QSTRING
    :	'"' ( ~( '"' | '\\' ) | '\\' . )* '"'
    ;

PPLINE
    : '#line' WS+ row=INT WS+ QSTRING WS+ action=INT {$channel=HIDDEN; ppLine($row.text, $QSTRING.text, $action.text);}
    ;