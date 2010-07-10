package traci.main;

import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeNodeStream;

import traci.model.shape.Shape;
import traci.parser.TraciLexer;
import traci.parser.TraciParser;
import traci.parser.TraciTree;

public class TestParser
{
    private static final String testStr =
        "union { union { } translate { 11 12 13 } union { } }";
    
    public static void main(String[] args) throws Exception
    {
        ANTLRStringStream in = new ANTLRStringStream(testStr);
        TraciLexer lexer = new TraciLexer(in);
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        TraciParser parser = new TraciParser(tokens);
        TraciParser.program_return r = parser.program();
        CommonTree t = (CommonTree) r.getTree();
        CommonTreeNodeStream nodes = new CommonTreeNodeStream(t);
        nodes.setTokenStream(tokens);
        TraciTree treeParser = new TraciTree(nodes);
        Shape shape = treeParser.program();
    }
}
