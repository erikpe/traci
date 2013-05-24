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
    GLOBAL_ASSIGN;
}

@parser::header {
package se.ejp.traci.lang.parser;

import se.ejp.traci.util.Log;
}

@parser::members {
private final List<ParseError> parseErrors = new ArrayList<ParseError>();

public void displayRecognitionError(String[] tokenNames, RecognitionException e)
{
    String msg = getErrorMessage(e, ParseError.getCustomTokenNames(this));
    IncludeLocation location = null;

    if (e.token instanceof TraciToken)
    {
        location = ((TraciToken) e.token).location;
    }

    parseErrors.add(new ParseError(e, location, msg, ParseError.Source.PARSER));
}

public List<ParseError> getParseErrors()
{
    return parseErrors;
}
}

@lexer::header {
package se.ejp.traci.lang.parser;

import se.ejp.traci.lang.parser.IncludeLocation.FileLocation;
import se.ejp.traci.util.Log;
}

@lexer::members {
private String currentFilename = null;
private final Stack<FileLocation> includeStack = new Stack<FileLocation>();
private final List<ParseError> lexerErrors = new ArrayList<ParseError>();

public Token emit()
{
    final TraciToken tok = new TraciToken(input, state.type, state.channel, state.tokenStartCharIndex, getCharIndex() - 1,
            new FileLocation(currentFilename, state.tokenStartLine, state.tokenStartCharPositionInLine), includeStack);

    tok.setLine(state.tokenStartLine);
    tok.setText(state.text);
    tok.setCharPositionInLine(state.tokenStartCharPositionInLine);
    emit(tok);
    return tok;
}

public void ppLine(String rowStr, String filename, String actionStr)
{
    final int row = Integer.parseInt(rowStr);
    final int action = Integer.parseInt(actionStr);

    if (action == 1)
    {
        if (currentFilename != null)
        {
            includeStack.push(new FileLocation(currentFilename, input.getLine(), 0));
        }
        input.setLine(0);
    }
    else if (action == 2)
    {
        final FileLocation location = includeStack.pop();
        input.setLine(location.row);
    }

    currentFilename = filename;
}

public void displayRecognitionError(String[] tokenNames, RecognitionException e)
{
    final String msg = getErrorMessage(e, tokenNames);
    final IncludeLocation location;

    if (e.token != null && (e.token instanceof TraciToken))
    {
        location = ((TraciToken) e.token).location;
    }
    else
    {
        location = new IncludeLocation(new FileLocation(currentFilename, e.line, e.charPositionInLine), includeStack);
    }

    lexerErrors.add(new ParseError(e, location, msg, ParseError.Source.LEXER));
}

public List<ParseError> getLexerErrors()
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
    : LPAR (ID (COMMA_OP ID)*)? RPAR -> ^(ARGS ID*)
    ;

block
    : LCURLY (function | statement)* RCURLY -> ^(BLOCK function* statement*)
    ;

statement
    : IF LPAR expr RPAR block (ELSE block)?    -> ^(IF expr block block?)
    | WHILE LPAR expr RPAR block               -> ^(WHILE expr block)
    | FOR LPAR ID IN expr DOTS expr RPAR block -> ^(FOR ID expr expr block)
    | (ID ASSIGN) => assign
    | (TRANSFORMATION ~(LPAR | LCURLY | SEMICOLON)) => simplified_transformation_statement
    | expr SEMICOLON!
    | RETURN expr SEMICOLON                    -> ^(RETURN expr)
    | GLOBAL ID ASSIGN expr SEMICOLON          -> ^(GLOBAL_ASSIGN ID expr)
    ;

assign
    : ID ASSIGN expr SEMICOLON -> ^(ASSIGN ID expr)
    ;

simplified_transformation_statement
    : TRANSFORMATION expr SEMICOLON -> ^(TRANSFORMATION ^(ARGS expr*))
    ;

expr
    : conditional_expr
    ;

conditional_expr
    : addition_expr ((LT_OP^ | GT_OP^ | LTE_OP^ | GTE_OP^ | EQ_OP^ | NEQ_OP^) addition_expr)?
    ;

addition_expr
    : multiplication_expr ((PLUS_OP^ | MINUS_OP^) multiplication_expr)*
    ;

multiplication_expr
    : unary_expr ((MUL_OP^ | DIV_OP^) unary_expr)*
    ;

unary_expr
    : primary_expr
    | MINUS_OP unary_expr -> ^(UNARY_OP MINUS_OP unary_expr)
    | PLUS_OP unary_expr -> ^(UNARY_OP PLUS_OP unary_expr)
    | NOT_OP unary_expr -> ^(UNARY_OP NOT_OP unary_expr)
    ;

primary_expr
    : constant
    | function_call
    | variable_reference
    | primitive_shape
    | csg_shape
    | bbox
    | transformation
    | light
    | texture
    | pigment
    | finish
    | interior
    | camera
    | vector
    | color
    | LPAR! expr RPAR!
    ;

constant
    : FLOAT
    | INT
    | QSTRING
    ;

function_call
    : ID function_call_args block? -> ^(FUNCALL ID function_call_args block?)
    ;

function_call_args
    : LPAR (expr (COMMA_OP expr)*)? RPAR -> ^(ARGS expr*)
    ;

variable_reference
    : ID block? -> ^(REF ID block?)
    ;

primitive_shape
    : PRIMITIVE_SHAPE function_call_args block? -> ^(PRIMITIVE_SHAPE function_call_args block?)
    | PRIMITIVE_SHAPE block?                    -> ^(PRIMITIVE_SHAPE ^(ARGS) block?)
    ;

csg_shape
    : CSG_SHAPE block? -> ^(CSG_SHAPE block?)
    ;

bbox
    : BBOX function_call_args block? -> ^(BBOX function_call_args block?)
    | BBOX block?                    -> ^(BBOX ^(ARGS) block?)
    ;

transformation
    : TRANSFORMATION function_call_args block? -> ^(TRANSFORMATION function_call_args block?)
    | TRANSFORMATION block?                    -> ^(TRANSFORMATION ^(ARGS) block?)
    ;

light
    : LIGHT function_call_args block? -> ^(LIGHT function_call_args block?)
    | LIGHT block?                    -> ^(LIGHT ^(ARGS) block?)
    ;

texture
    : TEXTURE block? -> ^(TEXTURE block?)
    ;

pigment
    : PIGMENT function_call_args block? -> ^(PIGMENT function_call_args block?)
    | PIGMENT block?                    -> ^(PIGMENT ^(ARGS) block?)
    ;

finish
    : FINISH function_call_args -> ^(FINISH function_call_args)
    ;

interior
    : INTERIOR function_call_args -> ^(INTERIOR function_call_args)
    ;

camera
    : CAMERA function_call_args block? -> ^(CAMERA function_call_args block?)
    ;

vector 
    : LBRACKET expr COMMA_OP expr COMMA_OP expr RBRACKET -> ^(VECTOR LBRACKET expr*)
    ;

color
    : COLOR LBRACKET expr COMMA_OP expr COMMA_OP expr (COMMA_OP expr)? RBRACKET -> ^(COLOR expr*)
    ;

LTE_OP : '<=';
GTE_OP : '>=';
EQ_OP : '==';
NEQ_OP : '!=';
LT_OP : '<';
GT_OP : '>';
PLUS_OP : '+';
MINUS_OP : '-';
MUL_OP : '*';
DIV_OP : '/';
NOT_OP : '!';
COMMA_OP : ',';
ASSIGN : '=';
SEMICOLON : ';';
LBRACKET : '[';
RBRACKET : ']';
LPAR : '(';
RPAR : ')';
LCURLY : '{';
RCURLY : '}';
DOTS : '..';

DEF : 'def';
RETURN : 'return';
GLOBAL : 'global';
WHILE : 'while';
IF : 'if';
ELSE : 'else';
BBOX : 'bbox';
FOR : 'for';
IN : 'in';

PRIMITIVE_SHAPE
    : ('box' | 'cylinder' | 'plane' | 'sphere' | 'torus')
    ;

CSG_SHAPE
    : ('union' | 'difference' | 'intersection' )
    ;

TRANSFORMATION
    : ('identity' | 'translate' | 'scale' | 'scalex' | 'scaley' | 'scalez' | 'rotx' | 'roty' | 'rotz' | 'rotAround' | 'rotVecToVec')
    ;

COLOR
    : ('color')
    ;

LIGHT
    : ('pointlight' | 'ambientlight')
    ;

TEXTURE
    : ('texture')
    ;

PIGMENT
    : ('solid' | 'image' | 'checker')
    ;

FINISH
    : ('finish')
    ;

INTERIOR
    : ('interior')
    ;

CAMERA
    : ('camera')
    ;

ID  : ('a'..'z' | 'A'..'Z' | '_') ('a'..'z' | 'A'..'Z' | '0'..'9' | '_')*
    ;

INT : ('0'..'9')+
    ;

FLOAT
    : ('0'..'9')+ '.' ('0'..'9')* EXPONENT?
    | '.' ('0'..'9')+ EXPONENT?
    | ('0'..'9')+ EXPONENT
    ;

COMMENT
    : '//' ~('\n'|'\r')* '\r'? '\n' { $channel=HIDDEN; }
    | '/*' (options { greedy=false; } : .)* '*/' { $channel=HIDDEN; }
    ;

WS  : (' ' | '\t' | '\r' | '\n')+ { $channel=HIDDEN; }
    ;

fragment
EXPONENT
    : ('e' | 'E') ('+' | '-')? ('0'..'9')+
    ;

QSTRING
    : '"' (~('"' | '\\') | '\\' .)* '"'
    ;

PPLINE
    : '#line' WS+ row=INT WS QSTRING WS action=INT
        { $channel=HIDDEN; ppLine($row.text, ParserUtilities.unquoteQstring($QSTRING.text), $action.text); }
    ;
