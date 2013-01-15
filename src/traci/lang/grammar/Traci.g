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

import traci.util.Log;
}

@parser::members {
private final List<ParseError> parseErrors = new ArrayList<ParseError>();

public void displayRecognitionError(String[] tokenNames,
                                    RecognitionException e) {
    String msg = getErrorMessage(e, tokenNames);
    final IncludeLocation location = ((TraciToken) e.token).location;
    parseErrors.add(new ParseError(location, msg));
}

public Iterable<ParseError> getParseErrors()
{
    return parseErrors;
}
}

@lexer::header {
package traci.lang.parser;

import traci.lang.parser.IncludeLocation.FileLocation;
import traci.util.Log;
}

@lexer::members {
private String currentFilename = null;
private final Stack<FileLocation> includeStack = new Stack<FileLocation>();
private final List<ParseError> lexerErrors = new ArrayList<ParseError>();

public Token emit() {
    final TraciToken tok = new TraciToken(input, state.type, state.channel, state.tokenStartCharIndex, getCharIndex() - 1,
            new FileLocation(currentFilename, state.tokenStartLine, state.tokenStartCharPositionInLine), includeStack);
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
            includeStack.push(new FileLocation(currentFilename, input.getLine(), 0));
        }
        input.setLine(0);
    }
    else if (action == 2) {
        final FileLocation location = includeStack.pop();
        input.setLine(location.row);
    }
    currentFilename = filename;
}

public void displayRecognitionError(String[] tokenNames,
                                    RecognitionException e) {
    final String msg = getErrorMessage(e, tokenNames);
    final IncludeLocation location;
    if (e.token != null)
    {
        location = ((TraciToken) e.token).location;
    }
    else
    {
        location = new IncludeLocation(new FileLocation(currentFilename, e.line, e.charPositionInLine), includeStack);
    }
    lexerErrors.add(new ParseError(location, msg));
}

public Iterable<ParseError> getLexerErrors()
{
    return lexerErrors;
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
    | LIGHT function_call_args? (block | ';')           -> ^(LIGHT function_call_args? block?)
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
    : VECTOR expr ',' expr ',' expr ']' -> ^(VECTOR expr*)
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
VECTOR : '[';

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

LIGHT
    :	( 'pointlight' )
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