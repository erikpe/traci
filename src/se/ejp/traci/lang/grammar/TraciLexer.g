lexer grammar TraciLexer;

@header {
package se.ejp.traci.lang.parser;

import se.ejp.traci.lang.parser.IncludeLocation.FileLocation;
import se.ejp.traci.util.Log;
}

@members {
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
ELIF : 'elif';
ELSE : 'else';
BBOX : 'bbox';
FOR : 'for';
IN : 'in';
TRUE : 'true';
FALSE : 'false';

PRIMITIVE_SHAPE
    : ('box' | 'cylinder' | 'plane' | 'sphere' | 'torus' | 'cone' | 'mesh')
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

MATERIAL
    : ('material')
    ;

CAMERA
    : ('camera')
    ;

SKYBOX
    : ('skybox')
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
    : '//' ~('\n'|'\r')* '\r'? '\n' {$channel=HIDDEN;}
    | '/*' (options {greedy=false;} : .)* '*/' {$channel=HIDDEN;}
    ;

WS  : (' ' | '\t' | '\r' | '\n')+ {$channel=HIDDEN;}
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
        {$channel=HIDDEN; ppLine($row.text, ParserUtilities.unquoteQstring($QSTRING.text), $action.text);}
    ;
