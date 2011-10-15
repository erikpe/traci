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
    UNARY_MINUS;
    UNARY_PLUS;
    UNARY_NOT;
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
    : IF '(' expr ')' block (ELSE block)? -> ^(IF expr block block?)
    | WHILE '(' expr ')' block            -> ^(WHILE expr block)
    | assignable_statement
    | RETURN assignable_statement         -> ^(RETURN assignable_statement)
    | GLOBAL ID '=' assignable_statement  -> ^(GLOBAL_ASSIGN ID assignable_statement)
    | ID '=' assignable_statement         -> ^(ASSIGN ID assignable_statement)
    ;

assignable_statement
    : (ID '{')=>id_statement
    | (function_call_statement)=>function_call_statement
    | PRIMITIVE_SHAPE function_call_args? (block | ';') -> ^(PRIMITIVE_SHAPE function_call_args? block?)
    | CSG_SHAPE (block | ';')                           -> ^(CSG_SHAPE block?)
    | MODIFIER expr ';'                                 -> ^(MODIFIER expr)
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
    | '-' unary_expr -> ^(UNARY_MINUS unary_expr)
    | '+' unary_expr -> ^(UNARY_PLUS unary_expr)
    | '!' unary_expr -> ^(UNARY_NOT unary_expr)
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
FOR : 'for';

PRIMITIVE_SHAPE
    :	( 'box' | 'cylinder' | 'plane' | 'sphere' | 'torus' )
    ;

CSG_SHAPE
    :	( 'union' | 'difference' | 'intersection' )
    ;

MODIFIER
    :	( 'translate' | 'trx' | 'try' | 'trz' |
          'rotate' | 'rotx' | 'roty' | 'rotz' | 'color' )
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

WS  :   ( ' '
        | '\t'
        | '\r'
        | '\n'
        ) {$channel=HIDDEN;}
    ;

fragment
EXPONENT : ('e'|'E') ('+'|'-')? ('0'..'9')+ ;

