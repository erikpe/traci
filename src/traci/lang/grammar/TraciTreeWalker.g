tree grammar TraciTreeWalker;

options {
    tokenVocab=Traci;
    ASTLabelType=CommonTree;
}

@header {
package traci.lang.parser;

import traci.lang.interpreter.TraciValue;
import traci.lang.interpreter.functions.BuiltinFunctions;
import traci.lang.interpreter.functions.FunctionSet;
import traci.lang.interpreter.node.AssignNode;
import traci.lang.interpreter.node.BBoxNode;
import traci.lang.interpreter.node.BinaryOpNode;
import traci.lang.interpreter.node.BlockNode;
import traci.lang.interpreter.node.ColorNode;
import traci.lang.interpreter.node.ConstNode;
import traci.lang.interpreter.node.ForNode;
import traci.lang.interpreter.node.FunctionCallNode;
import traci.lang.interpreter.node.FunctionNode;
import traci.lang.interpreter.node.IfElseNode;
import traci.lang.interpreter.node.LightNode;
import traci.lang.interpreter.node.Op;
import traci.lang.interpreter.node.RefNode;
import traci.lang.interpreter.node.ReturnNode;
import traci.lang.interpreter.node.ShapeNode;
import traci.lang.interpreter.node.TraciNode;
import traci.lang.interpreter.node.TransformationNode;
import traci.lang.interpreter.node.UnaryOpNode;
import traci.lang.interpreter.node.VectorNode;
import traci.lang.interpreter.node.WhileNode;
}

@members {
private FunctionSet functionsThisScope = BuiltinFunctions.getAll();

public void displayRecognitionError(String[] tokenNames,
                                    RecognitionException e) {
    String hdr = getErrorHeader(e);
    String msg = getErrorMessage(e, tokenNames);
}
}

scene
    : block EOF
    ;

block returns [BlockNode node]
@init {
final FunctionSet functionsOuterScope = functionsThisScope;
functionsThisScope = new FunctionSet(functionsOuterScope);
node = new BlockNode(functionsThisScope);
}
@after {
functionsThisScope = functionsOuterScope;
}
    : ^(BLOCK ( statement    { $node.addStatement($statement.node); }
              | function_def { functionsThisScope.put($function_def.node.id, $function_def.node); } )*)
    ;

function_def returns [FunctionNode node]
    : ^(DEF ID function_def_args block)
        { $node = new FunctionNode($ID.text, $function_def_args.argIDs, $block.node); }
    ;

function_def_args returns [List<String> argIDs]
@init {
argIDs = new ArrayList<String>();
}
    : ^(ARGS ( ID { $argIDs.add($ID.text); } )*)
    ;

function_call_args returns [List<TraciNode> nodes]
@init {
nodes = new ArrayList<TraciNode>();
}
    : ^(ARGS ( expr { $nodes.add($expr.node); } )*)
    ;

statement returns [TraciNode node]
    : assignable_statement
        { $node = $assignable_statement.node; }
    | ^(RETURN assignable_statement)
        { $node = new ReturnNode($assignable_statement.node); }
    | ^(IF expr a=block b=block?)
        { $node = new IfElseNode($expr.node, $a.node, $b.node, $IF.token); }
    | ^(WHILE expr block)
        { $node = new WhileNode($expr.node, $block.node, $WHILE.token); }
    | ^(FOR ID c=expr d=expr block)
        { $node = new ForNode($ID.text, $c.node, $d.node, $block.node, $FOR.token); }
    | ^(GLOBAL_ASSIGN ID assignable_statement)
        { $node = new AssignNode($ID.text, $assignable_statement.node, true); }
    | ^(ASSIGN ID assignable_statement)
        { $node = new AssignNode($ID.text, $assignable_statement.node, false); }
    ;

assignable_statement returns [TraciNode node]
    : ^(PRIMITIVE_SHAPE function_call_args? block?)
        { $node = new ShapeNode($PRIMITIVE_SHAPE.text, $function_call_args.nodes, $block.node, $PRIMITIVE_SHAPE.token); }
    | ^(CSG_SHAPE function_call_args? block?)
        { $node = new ShapeNode($CSG_SHAPE.text, $function_call_args.nodes, $block.node, $CSG_SHAPE.token); }
    | ^(BBOX function_call_args? block?)
        { $node = new BBoxNode($function_call_args.nodes, $block.node); }
    | ^(TRANSFORMATION expr)
        { $node = new TransformationNode($TRANSFORMATION.text, $expr.node, $TRANSFORMATION.token); }
//    | ^(TRANSFORMATION2 function_call_args)
//    	{ $node = new TransformationNode($TRANSFORMATION2.text, $function_call_args.nodes, $TRANSFORMATION2.token); }
    | ^(LIGHT function_call_args? block?)
        { $node = new LightNode($LIGHT.text, $function_call_args.nodes, $block.node, $LIGHT.token); }
    | expr
        { $node = $expr.node; }
    ;

expr returns [TraciNode node]
    : ^(op='+' a=expr b=expr)   { $node = new BinaryOpNode(Op.BINARY_ADD, $a.node, $b.node, $op.token); }
    | ^(op='-' a=expr b=expr)   { $node = new BinaryOpNode(Op.BINARY_SUB, $a.node, $b.node, $op.token); }
    | ^(op='*' a=expr b=expr)   { $node = new BinaryOpNode(Op.BINARY_MUL, $a.node, $b.node, $op.token); }
    | ^(op='/' a=expr b=expr)   { $node = new BinaryOpNode(Op.BINARY_DIV, $a.node, $b.node, $op.token); }
    | ^(op='<' a=expr b=expr)   { $node = new BinaryOpNode(Op.COMPARE_LT, $a.node, $b.node, $op.token); }
    | ^(op='<=' a=expr b=expr)  { $node = new BinaryOpNode(Op.COMPARE_LTE, $a.node, $b.node, $op.token); }
    | ^(op='>' a=expr b=expr)   { $node = new BinaryOpNode(Op.COMPARE_GT, $a.node, $b.node, $op.token); }
    | ^(op='>=' a=expr b=expr)  { $node = new BinaryOpNode(Op.COMPARE_GTE, $a.node, $b.node, $op.token); }
    | ^(op='==' a=expr b=expr)  { $node = new BinaryOpNode(Op.COMPARE_EQ, $a.node, $b.node, $op.token); }
    | ^(op='!=' a=expr b=expr)  { $node = new BinaryOpNode(Op.COMPARE_NEQ, $a.node, $b.node, $op.token); }
    | ^(UNARY_OP op='+' a=expr) { $node = new UnaryOpNode(Op.UNARY_PLUS, $a.node, $op.token); }
    | ^(UNARY_OP op='-' a=expr) { $node = new UnaryOpNode(Op.UNARY_NEG, $a.node, $op.token); }
    | ^(UNARY_OP op='!' a=expr) { $node = new UnaryOpNode(Op.UNARY_NOT, $a.node, $op.token); }
    | ^(REF ID block?)      { $node = new RefNode($ID.text, $block.node, $ID.token); }
    | ^(FUNCALL ID function_call_args block?)
        { $node = new FunctionCallNode($ID.text, $function_call_args.nodes, $block.node, $ID.token); }
    | ^(VECTOR a=expr b=expr c=expr)
        { $node = new VectorNode($a.node, $b.node, $c.node, $VECTOR.token); }
    | ^(COLOR a=expr b=expr c=expr)
        { $node = new ColorNode($a.node, $b.node, $c.node, $COLOR.token); }
    | INT                   { $node = new ConstNode(new TraciValue(Double.valueOf($INT.text))); }
    | FLOAT                 { $node = new ConstNode(new TraciValue(Double.valueOf($FLOAT.text))); }
    ;
