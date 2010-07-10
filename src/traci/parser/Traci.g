grammar Traci;

options {
	output = AST;
}

tokens {
	UNION; DIFFERENCE; INTERSECTION;
	SPHERE; CYLINDER; TORUS;
	TRANSLATE; SCALE; ROTATE;
	NUMBER; VECTOR;
}

@header {
package traci.parser;
}

@lexer::header {
package traci.parser;
}

program
	: shape
	;

shape
	: csg
	| primitive
	;

csg
	: union
	| difference
	;

union
	: 'union' '{' csg_member* '}' -> ^(UNION csg_member*)
	;

difference
	: 'difference' '{' csg_member* '}' -> ^(DIFFERENCE csg_member*)
	;

csg_member
	: shape
	| transformation
	;

primitive
	: sphere
	| cylinder
	| torus
	;

sphere
	: 'sphere' '{' primitive_member* '}' -> ^(SPHERE primitive_member*)
	;

cylinder
	: 'cylinder' '{' primitive_member* '}' -> ^(CYLINDER primitive_member*)
	;

torus
	: 'torus' '{' Number primitive_member* '}' -> ^(TORUS Number primitive_member*)
	;

primitive_member
	: transformation
	;

transformation
	: translate
	| scale
	| rotate
	;

translate
	: 'translate' '{' vector '}' -> ^(TRANSLATE vector)
	;

scale
	: 'scale' '{' vector '}' -> ^(SCALE vector)
	;

rotate
	: 'rotate' '{' vector '}' -> ^(ROTATE vector)
	;

vector
	: number number number -> ^(VECTOR number number number)
	;

number
	: Number -> ^(NUMBER Number)
	;

Number
	: '-'? Digit+ ( '.' Digit+ )?
	;

fragment Digit
	: '0' .. '9'
	;

WS
	: (' ' | '\n' | '\r' | '\t')+ { $channel = HIDDEN; }
	; // ignore whitespace 
