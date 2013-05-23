package se.ejp.traci.lang.interpreter.node;

import static org.junit.Assert.assertEquals;

import org.antlr.runtime.RecognitionException;
import org.junit.Test;

import se.ejp.traci.lang.parser.TraciTreeWalkerBase;

public class TreeWalkerColorTest extends TraciTreeWalkerBase
{
    @Test
    public void testColor() throws RecognitionException
    {
        runTreeWalker("color [.1, .2, .3];");
        assertEquals(1, rootNode.getStatements().size());
        TraciNode statement = rootNode.getStatements().get(0);
        assertEquals(ColorNode.class, statement.getClass());
        ColorNode colorNode = (ColorNode) statement;
        assertEquals(3, colorNode.nodes.size());
        assertEquals("color", colorNode.token.getText());
        for (final TraciNode child : colorNode.nodes)
        {
            assertEquals(ConstNode.class, child.getClass());
        }

        runTreeWalker("color [.1, .2, .3, .4];");
        assertEquals(1, rootNode.getStatements().size());
        statement = rootNode.getStatements().get(0);
        assertEquals(ColorNode.class, statement.getClass());
        colorNode = (ColorNode) statement;
        assertEquals(4, colorNode.nodes.size());
        assertEquals("color", colorNode.token.getText());
        for (final TraciNode child : colorNode.nodes)
        {
            assertEquals(ConstNode.class, child.getClass());
        }
    }
}
