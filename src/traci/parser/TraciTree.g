tree grammar TraciTree;

options {
	tokenVocab = Traci;
	ASTLabelType = CommonTree;
}

@header {
package traci.parser;

import traci.math.Transformation;
import traci.math.Vector;
import traci.model.shape.Shape;
import traci.model.shape.csg.Csg;
import traci.model.shape.csg.Union;
}

@members {
	private Double extractNumber(CommonTree numberToken)
	{
		return Double.parseDouble(numberToken.getText());
	}
}

program returns [Shape result]
	: s = shape { $result = s; }
	;

shape returns [Shape result]
	: c = csg { $result = c; }
	;

csg returns [Csg result]
	: u = union { $result = u; }
	;

union returns [Union result]
	@init { result = new Union(); }
	: ^(UNION ( s = shape { result.add(s); } |
	            t = transformation { result.transform(t); } )* )
	;

transformation returns [Transformation result]
	: t = translate { $result = t; }
	;

translate returns [Transformation result]
	: ^(TRANSLATE ( v = vector
	  { $result = Transformation.translate(21, 22, 23); } ) )
	;

vector returns [Vector result]
	: ^(VECTOR ( n0 = number n1 = number n2 = number
	  { $result = Vector.make(n0, n1, n2); } ) )
	;

number returns [Double result]
	: ^(NUMBER Number) { $result = extractNumber($Number); }
	;
