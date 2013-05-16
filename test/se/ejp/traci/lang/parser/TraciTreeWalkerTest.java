package se.ejp.traci.lang.parser;

import static org.junit.Assert.assertEquals;

import org.antlr.runtime.RecognitionException;
import org.junit.Test;

import se.ejp.traci.lang.interpreter.TraciValue.Type;
import se.ejp.traci.lang.interpreter.node.BinaryOpNode;
import se.ejp.traci.lang.interpreter.node.ConstNode;
import se.ejp.traci.lang.interpreter.node.Op;
import se.ejp.traci.lang.interpreter.node.TraciNode;

public class TraciTreeWalkerTest extends TraciTreeWalkerBase
{
    @Test
    public void testTreeWalker() throws RecognitionException
    {
        runTreeWalker("17+23;");
        assertEquals(1, rootNode.getStatements().size());
        final TraciNode statement = rootNode.getStatements().get(0);
        assertEquals(BinaryOpNode.class, statement.getClass());
        final BinaryOpNode binOpNode = (BinaryOpNode) statement;
        assertEquals(Op.BINARY_ADD, binOpNode.op);
        assertEquals(ConstNode.class, binOpNode.aNode.getClass());
        final ConstNode aNode = (ConstNode) binOpNode.aNode;
        assertEquals(ConstNode.class, binOpNode.bNode.getClass());
        final ConstNode bNode = (ConstNode) binOpNode.bNode;
        assertEquals(Type.NUMBER, aNode.value.getType());
        assertEquals(17, aNode.value.getNumber(), 0);
        assertEquals(Type.NUMBER, bNode.value.getType());
        assertEquals(23, bNode.value.getNumber(), 0);
    }

    @Test
    public void testString() throws RecognitionException
    {
        runTreeWalker("\"foo/bar.hej\";");
        assertEquals(1, rootNode.getStatements().size());
        final TraciNode statement = rootNode.getStatements().get(0);
        assertEquals(ConstNode.class, statement.getClass());
        final ConstNode constNode = (ConstNode) statement;
        assertEquals(Type.STRING, constNode.value.getType());
        assertEquals("foo/bar.hej", constNode.value.getString());
    }
}
