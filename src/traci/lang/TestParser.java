package traci.lang;

import org.antlr.runtime.ANTLRFileStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeNodeStream;
import org.antlr.runtime.tree.Tree;

import traci.lang.interpreter.Context;
import traci.lang.interpreter.Entity;
import traci.lang.interpreter.node.BlockNode;
import traci.lang.parser.TraciLexer;
import traci.lang.parser.TraciParser;
import traci.lang.parser.TraciTreeWalker;

public class TestParser
{
    private static void prettyPrint(final Tree tree, final int indent, final StringBuilder sb)
    {
        sb.append('\n');
        for (int i = 0; i < indent; ++i)
        {
            sb.append("    ");
        }
        
        if (tree.getChildCount() > 0)
        {
            sb.append('(');
            sb.append(tree.getText());
            for (int i = 0; i < tree.getChildCount(); ++i)
            {
                prettyPrint(tree.getChild(i), indent + 1, sb);
            }
            sb.append(')');
        }
        else
        {
            sb.append(tree.getText());
        }
    }
    
    public static void main(String[] args) throws Exception
    {
        ANTLRFileStream input = new ANTLRFileStream("src/traci/lang/input2.txt");
        TraciLexer lexer = new TraciLexer(input);
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        TraciParser parser = new TraciParser(tokens);
        CommonTree tree = (CommonTree) parser.scene().getTree();
        System.out.println(tree.toStringTree());
        final StringBuilder sb = new StringBuilder();
        prettyPrint(tree, 0, sb);
        System.out.println(sb.toString());
        
        CommonTreeNodeStream nodes = new CommonTreeNodeStream(tree);
        TraciTreeWalker walker = new TraciTreeWalker(nodes);
        BlockNode bn = walker.block();
        final Entity.SceneEntity entity = new Entity.SceneEntity();
        bn.eval(Context.newRootContext(entity));
    }
}
