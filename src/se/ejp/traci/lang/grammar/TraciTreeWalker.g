tree grammar TraciTreeWalker;

options {
    tokenVocab=Traci;
    ASTLabelType=CommonTree;
}

@rulecatch {
catch (final RecognitionException e)
{
    throw e;
}
}

@header {
package se.ejp.traci.lang.parser;

import se.ejp.traci.lang.interpreter.TraciValue;
import se.ejp.traci.lang.interpreter.functions.BuiltinFunctions;
import se.ejp.traci.lang.interpreter.functions.FunctionSet;
import se.ejp.traci.lang.interpreter.node.AssignNode;
import se.ejp.traci.lang.interpreter.node.BinaryOpNode;
import se.ejp.traci.lang.interpreter.node.BlockNode;
import se.ejp.traci.lang.interpreter.node.ColorNode;
import se.ejp.traci.lang.interpreter.node.ConstNode;
import se.ejp.traci.lang.interpreter.node.ForNode;
import se.ejp.traci.lang.interpreter.node.FunctionCallNode;
import se.ejp.traci.lang.interpreter.node.FunctionNode;
import se.ejp.traci.lang.interpreter.node.IfElseNode;
import se.ejp.traci.lang.interpreter.node.ObjectNode;
import se.ejp.traci.lang.interpreter.node.Op;
import se.ejp.traci.lang.interpreter.node.RefNode;
import se.ejp.traci.lang.interpreter.node.ReturnNode;
import se.ejp.traci.lang.interpreter.node.TraciNode;
import se.ejp.traci.lang.interpreter.node.TransformationNode;
import se.ejp.traci.lang.interpreter.node.UnaryOpNode;
import se.ejp.traci.lang.interpreter.node.VectorNode;
import se.ejp.traci.lang.interpreter.node.WhileNode;

import java.util.Collections;
}

@members {
private FunctionSet functionsThisScope = BuiltinFunctions.getAll();
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
    : ^(ARGS (ID { $argIDs.add($ID.text); } )*)
    ;

function_call_args returns [List<TraciNode> nodes]
@init {
nodes = new ArrayList<TraciNode>();
}
    : ^(ARGS (expr { $nodes.add($expr.node); })*)
    ;

statement returns [TraciNode node]
    : expr
        { $node = $expr.node; }
    | ^(RETURN expr)
        { $node = new ReturnNode($expr.node); }
    | ^(IF expr a=block b=block?)
        { $node = new IfElseNode($expr.node, $a.node, $b.node, $IF.token); }
    | ^(WHILE expr block)
        { $node = new WhileNode($expr.node, $block.node, $WHILE.token); }
    | ^(FOR ID c=expr d=expr block)
        { $node = new ForNode($ID.text, $c.node, $d.node, $block.node, $FOR.token); }
    | ^(GLOBAL_ASSIGN ID expr)
        { $node = new AssignNode($ID.text, $expr.node, true); }
    | ^(ASSIGN ID expr)
        { $node = new AssignNode($ID.text, $expr.node, false); }
    ;

expr returns [TraciNode node]
    : ^(op=PLUS_OP a=expr b=expr)    { $node = new BinaryOpNode(Op.BINARY_ADD, $a.node, $b.node, $op.token); }
    | ^(op=MINUS_OP a=expr b=expr)   { $node = new BinaryOpNode(Op.BINARY_SUB, $a.node, $b.node, $op.token); }
    | ^(op=MUL_OP a=expr b=expr)     { $node = new BinaryOpNode(Op.BINARY_MUL, $a.node, $b.node, $op.token); }
    | ^(op=DIV_OP a=expr b=expr)     { $node = new BinaryOpNode(Op.BINARY_DIV, $a.node, $b.node, $op.token); }
    | ^(op=LT_OP a=expr b=expr)      { $node = new BinaryOpNode(Op.COMPARE_LT, $a.node, $b.node, $op.token); }
    | ^(op=LTE_OP a=expr b=expr)     { $node = new BinaryOpNode(Op.COMPARE_LTE, $a.node, $b.node, $op.token); }
    | ^(op=GT_OP a=expr b=expr)      { $node = new BinaryOpNode(Op.COMPARE_GT, $a.node, $b.node, $op.token); }
    | ^(op=GTE_OP a=expr b=expr)     { $node = new BinaryOpNode(Op.COMPARE_GTE, $a.node, $b.node, $op.token); }
    | ^(op=EQ_OP a=expr b=expr)      { $node = new BinaryOpNode(Op.COMPARE_EQ, $a.node, $b.node, $op.token); }
    | ^(op=NEQ_OP a=expr b=expr)     { $node = new BinaryOpNode(Op.COMPARE_NEQ, $a.node, $b.node, $op.token); }
    | ^(UNARY_OP op=PLUS_OP a=expr)  { $node = new UnaryOpNode(Op.UNARY_PLUS, $a.node, $op.token); }
    | ^(UNARY_OP op=MINUS_OP a=expr) { $node = new UnaryOpNode(Op.UNARY_NEG, $a.node, $op.token); }
    | ^(UNARY_OP op=NOT_OP a=expr)   { $node = new UnaryOpNode(Op.UNARY_NOT, $a.node, $op.token); }
    | ^(REF ID block?)               { $node = new RefNode($ID.text, $block.node, $ID.token); }
    | ^(FUNCALL ID function_call_args block?)
        { $node = new FunctionCallNode($ID.text, $function_call_args.nodes, $block.node, $ID.token); }
    | ^(VECTOR LBRACKET a=expr b=expr c=expr)
        { $node = new VectorNode($a.node, $b.node, $c.node, $LBRACKET.token); }
    | ^(COLOR a=expr b=expr c=expr)
        { $node = new ColorNode($a.node, $b.node, $c.node, $COLOR.token); }
    | ^(PRIMITIVE_SHAPE function_call_args block?)
        { $node = new ObjectNode($PRIMITIVE_SHAPE.text, $function_call_args.nodes, $block.node, $PRIMITIVE_SHAPE.token); }
    | ^(CSG_SHAPE block?)
        { $node = new ObjectNode($CSG_SHAPE.text, Collections.<TraciNode>emptyList(), $block.node, $CSG_SHAPE.token); }
    | ^(BBOX function_call_args block?)
        { $node = new ObjectNode($BBOX.text, $function_call_args.nodes, $block.node, $BBOX.token); }
    | ^(TRANSFORMATION function_call_args block?)
    	{ $node = new TransformationNode($TRANSFORMATION.text, $function_call_args.nodes, $block.node, $TRANSFORMATION.token); }
    | ^(LIGHT function_call_args block?)
        { $node = new ObjectNode($LIGHT.text, $function_call_args.nodes, $block.node, $LIGHT.token); }
    | ^(PIGMENT function_call_args block?)
        { $node = new ObjectNode($PIGMENT.text, $function_call_args.nodes, $block.node, $PIGMENT.token); }
    | ^(FINISH function_call_args)
        { $node = new ObjectNode($FINISH.text, $function_call_args.nodes, null, $FINISH.token); }
    | INT     { $node = new ConstNode(new TraciValue(Double.valueOf($INT.text))); }
    | FLOAT   { $node = new ConstNode(new TraciValue(Double.valueOf($FLOAT.text))); }
    | QSTRING { $node = new ConstNode(new TraciValue(ParserUtilities.unquoteQstring($QSTRING.text))); }
    ;
