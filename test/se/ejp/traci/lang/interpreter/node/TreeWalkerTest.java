package se.ejp.traci.lang.interpreter.node;

import static org.junit.Assert.assertEquals;

import org.antlr.runtime.RecognitionException;
import org.junit.Test;

import se.ejp.traci.lang.parser.TraciTreeWalkerBase;

public class TreeWalkerTest extends TraciTreeWalkerBase
{
    @Test
    public void testColor() throws RecognitionException
    {
        runTreeWalker("color [.1, .2, .3];");
        assertEquals(1, rootNode.getStatements().size());
        TraciNode statement = rootNode.getStatements().get(0);
        assertEquals(ObjectNode.class, statement.getClass());
        ObjectNode colorNode = (ObjectNode) statement;
        assertEquals(3, colorNode.argNodes.size());
        assertEquals("color", colorNode.token.getText());
        for (final TraciNode child : colorNode.argNodes)
        {
            assertEquals(ConstNode.class, child.getClass());
        }

        runTreeWalker("color [.1, .2, .3, .4];");
        assertEquals(1, rootNode.getStatements().size());
        statement = rootNode.getStatements().get(0);
        assertEquals(ObjectNode.class, statement.getClass());
        colorNode = (ObjectNode) statement;
        assertEquals(4, colorNode.argNodes.size());
        assertEquals("color", colorNode.token.getText());
        for (final TraciNode child : colorNode.argNodes)
        {
            assertEquals(ConstNode.class, child.getClass());
        }
    }

    @Test
    public void testInterior() throws RecognitionException
    {
        runTreeWalker("interior(1.1);");
        assertEquals(1, rootNode.getStatements().size());
        final TraciNode statement = rootNode.getStatements().get(0);
        assertEquals(ObjectNode.class, statement.getClass());
        final ObjectNode objectNode = (ObjectNode) statement;
        assertEquals(1, objectNode.argNodes.size());
        assertEquals("interior", objectNode.token.getText());
        assertEquals(ConstNode.class, objectNode.argNodes.get(0).getClass());
    }
}
