parser grammar TraciParser;

options {
    tokenVocab=TraciLexer;
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

@header {
package se.ejp.traci.lang.parser;

import se.ejp.traci.util.Log;
}

@members {
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
    : if_statement
    | WHILE LPAR expr RPAR block               -> ^(WHILE expr block)
    | FOR LPAR ID IN expr DOTS expr RPAR block -> ^(FOR ID expr expr block)
    | (ID ASSIGN) => assign
    | (TRANSFORMATION ~(LPAR | LCURLY | SEMICOLON)) => simplified_transformation_statement
    | expr SEMICOLON!
    | RETURN expr SEMICOLON                    -> ^(RETURN expr)
    | GLOBAL ID ASSIGN expr SEMICOLON          -> ^(GLOBAL_ASSIGN ID expr)
    ;

if_statement
    : IF LPAR expr RPAR block (ELIF LPAR expr RPAR block)* (ELSE block)? -> ^(IF expr* block*)
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
    | material
    | camera
    | skybox
    | vector
    | color
    | LPAR! expr RPAR!
    ;

constant
    : FLOAT
    | INT
    | QSTRING
    | TRUE
    | FALSE
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

material
    : MATERIAL function_call_args block? -> ^(MATERIAL function_call_args block?)
    | MATERIAL block?                    -> ^(MATERIAL ^(ARGS) block?)
    ;

camera
    : CAMERA function_call_args block? -> ^(CAMERA function_call_args block?)
    ;

skybox
    : SKYBOX function_call_args block? -> ^(SKYBOX function_call_args block?)
    ;

vector 
    : LBRACKET expr COMMA_OP expr COMMA_OP expr RBRACKET -> ^(VECTOR LBRACKET ^(ARGS expr*))
    ;

color
    : COLOR LBRACKET expr COMMA_OP expr COMMA_OP expr (COMMA_OP expr)? RBRACKET -> ^(COLOR ^(ARGS expr*))
    ;
