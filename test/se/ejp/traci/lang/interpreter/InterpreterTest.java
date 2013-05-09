package se.ejp.traci.lang.interpreter;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;

import org.antlr.runtime.ANTLRFileStream;
import org.antlr.runtime.CharStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeNodeStream;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import se.ejp.traci.lang.interpreter.Entities.Entity;
import se.ejp.traci.lang.interpreter.TraciValue.Type;
import se.ejp.traci.lang.interpreter.exceptions.FunctionReturnException;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import se.ejp.traci.lang.interpreter.node.BlockNode;
import se.ejp.traci.lang.parser.TraciLexer;
import se.ejp.traci.lang.parser.TraciParser;
import se.ejp.traci.lang.parser.TraciTreeWalker;
import se.ejp.traci.model.Scene;
import se.ejp.traci.model.shape.csg.Union;

public class InterpreterTest
{
    private final static String TESTCODE_DIR = "testcode";

    private Scene scene;

    @Before
    public void setUp() throws Exception
    {
    }

    @After
    public void tearDown() throws Exception
    {
        scene = null;
    }

    private TraciValue runInterpreter(final String testfile) throws InterpreterRuntimeException
    {
        final String file = TESTCODE_DIR + File.separator + testfile;
        CharStream input = null;

        try
        {
            input = new ANTLRFileStream(file);
        }
        catch (final IOException e)
        {
            fail("Unable to open input file: " + file + "\n" + e.getMessage());
        }

        final TraciLexer lexer = new TraciLexer(input);
        final TraciParser parser = new TraciParser(new CommonTokenStream(lexer));

        CommonTree tree = null;
        try
        {
            tree = (CommonTree) parser.scene().getTree();
        }
        catch (final RecognitionException e)
        {
            fail(e.getMessage());
        }

        assertFalse(lexer.getLexerErrors().iterator().hasNext());
        assertFalse(parser.getParseErrors().iterator().hasNext());

        final TraciTreeWalker treeWalker = new TraciTreeWalker(new CommonTreeNodeStream(tree));
        BlockNode blockNode = null;
        try
        {
            blockNode = treeWalker.block();
        }
        catch (final RecognitionException e)
        {
            fail(e.getMessage());
        }

        final Union rootUnion = new Union();
        final Entity entity = Entities.makeEntity(rootUnion);
        scene = new Scene();

        try
        {
            blockNode.eval(Context.newRootContext(scene, entity));
        }
        catch (final FunctionReturnException e)
        {
            return e.value;
        }

        return null;
    }

    @Test
    public void testFibonacci() throws InterpreterRuntimeException
    {
        final TraciValue val = runInterpreter("fibonacci.traci");
        assertEquals(Type.NUMBER, val.getType());
        assertEquals(55, val.getNumber(), 0);
    }

    @Test
    public void testFunctionscope() throws InterpreterRuntimeException
    {
        final TraciValue val = runInterpreter("nested-function.traci");
        assertEquals(Type.NUMBER, val.getType());
        assertEquals(25, val.getNumber(), 0);
    }

    @Test
    public void testGlobalValue() throws InterpreterRuntimeException
    {
        final TraciValue val = runInterpreter("global-value.traci");
        assertEquals(Type.NUMBER, val.getType());
        assertEquals(44, val.getNumber(), 0);
    }
}
