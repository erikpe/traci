tree grammar TraciTreeWalker;

options {
    tokenVocab=Traci;
    ASTLabelType=CommonTree;
}

@header {
    package traci.lang.parser;

    import java.util.HashMap;
    import java.util.Map;
    import traci.math.Vector;
    import traci.model.shape.csg.*;
    import traci.model.shape.*;
    import traci.lang.*;
    import traci.lang.interpreter.*;
    import traci.lang.interpreter.node.*;
    import traci.lang.parser.*;
}

@members {
    private Map<String, Function> visibleFunctions = BuiltinFunctions.getAll();
}

scene
    : block EOF
    ;

block returns [BlockNode node]
@init {
    final Map<String, Function> outerFunctions = visibleFunctions;
    visibleFunctions = new HashMap<String, Function>(outerFunctions);
    node = new BlockNode(visibleFunctions);
}
@after {
    visibleFunctions = outerFunctions;
}
    : ^(BLOCK ( statement    { $node.addStatement($statement.node); }
              | function_def { visibleFunctions.put($function_def.node.id, $function_def.node); } )*)
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
        { $node = new IfElseNode($expr.node, $a.node, $b.node); }
    | ^(WHILE expr block)
        { $node = new WhileNode($expr.node, $block.node); }
    | ^(FOR ID c=expr d=expr block)
        { $node = new ForNode($ID.text, $c.node, $d.node, $block.node); }
    | ^(GLOBAL_ASSIGN ID assignable_statement)
        { $node = new AssignNode($ID.text, $assignable_statement.node, true); }
    | ^(ASSIGN ID assignable_statement)
        { $node = new AssignNode($ID.text, $assignable_statement.node, false); }
    ;

assignable_statement returns [TraciNode node]
    : ^(PRIMITIVE_SHAPE function_call_args? block?)
        { $node = new PrimitiveShapeNode($PRIMITIVE_SHAPE.text, $function_call_args.nodes, $block.node); }
    | ^(CSG_SHAPE function_call_args? block?)
        { $node = new CsgShapeNode($CSG_SHAPE.text, $function_call_args.nodes, $block.node); }
    | ^(BBOX function_call_args? block?)
        { $node = new BBoxNode($function_call_args.nodes, $block.node); }
    | ^(TRANSFORMATION expr)
        { $node = new TransformationNode($TRANSFORMATION.text, $expr.node); }
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
        { $node = new FunctionCallNode($ID.text, $function_call_args.nodes, $block.node); }
    | ^(VECTOR a=expr b=expr c=expr)
        { $node = new VectorNode($a.node, $b.node, $c.node); }
    | INT                   { $node = new ConstNode(new TraciValue(Double.valueOf($INT.text))); }
    | FLOAT                 { $node = new ConstNode(new TraciValue(Double.valueOf($FLOAT.text))); }
    ;
