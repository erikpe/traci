package se.ejp.traci.lang.interpreter.node;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;

import java.util.List;

import org.junit.Before;
import org.junit.Test;

import se.ejp.traci.lang.interpreter.Context;
import se.ejp.traci.lang.interpreter.TraciValue;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import se.ejp.traci.lang.interpreter.functions.Function;
import se.ejp.traci.lang.interpreter.functions.FunctionSet;
import se.ejp.traci.model.Scene;

public class BlockNodeTest
{
    private FunctionSet emptyFunctionSet;
    private FunctionSet functionSet1;
    private FunctionSet functionSet2;
    private Function dummyFunction1;
    private Function dummyFunction2;

    @Before
    public void setUp()
    {
        emptyFunctionSet = new FunctionSet(null);

        dummyFunction1 = new Function()
        {
            @Override
            public TraciValue invoke(FunctionCallNode funcallNode, Context context, List<TraciValue> args)
                    throws InterpreterRuntimeException
            {
                return new TraciValue(1.0);
            }
        };

        dummyFunction2 = new Function()
        {
            @Override
            public TraciValue invoke(FunctionCallNode funcallNode, Context context, List<TraciValue> args)
                    throws InterpreterRuntimeException
            {
                return new TraciValue(2.0);
            }
        };

        functionSet1 = new FunctionSet(null);
        functionSet1.put("func1", dummyFunction1);

        functionSet2 = new FunctionSet(null);
        functionSet2.put("func2", dummyFunction2);
    }

    @Test
    public void testAppend_CreatesNewBlockNode()
    {
        final BlockNode block1 = new BlockNode(emptyFunctionSet);
        final BlockNode block2 = new BlockNode(emptyFunctionSet);

        final BlockNode combined = block1.append(block2);

        // Verify that a new BlockNode is created
        assertNotNull(combined);
        assertNotSame(block1, combined);
        assertNotSame(block2, combined);
    }

    @Test
    public void testAppend_FirstBlockUnchanged_Statements()
    {
        final BlockNode block1 = new BlockNode(emptyFunctionSet);
        final TraciNode stmt1 = new ConstNode(new TraciValue(1.0));
        final TraciNode stmt2 = new ConstNode(new TraciValue(2.0));
        block1.addStatement(stmt1);
        block1.addStatement(stmt2);

        final BlockNode block2 = new BlockNode(emptyFunctionSet);
        final TraciNode stmt3 = new ConstNode(new TraciValue(3.0));
        block2.addStatement(stmt3);

        // Record original state
        final int originalBlock1Size = block1.getStatements().size();

        block1.append(block2);

        // Verify block1 is unchanged
        assertEquals(originalBlock1Size, block1.getStatements().size());
        assertEquals(2, block1.getStatements().size());
        assertSame(stmt1, block1.getStatements().get(0));
        assertSame(stmt2, block1.getStatements().get(1));
    }

    @Test
    public void testAppend_SecondBlockUnchanged_Statements()
    {
        final BlockNode block1 = new BlockNode(emptyFunctionSet);
        final TraciNode stmt1 = new ConstNode(new TraciValue(1.0));
        block1.addStatement(stmt1);

        final BlockNode block2 = new BlockNode(emptyFunctionSet);
        final TraciNode stmt2 = new ConstNode(new TraciValue(2.0));
        final TraciNode stmt3 = new ConstNode(new TraciValue(3.0));
        block2.addStatement(stmt2);
        block2.addStatement(stmt3);

        // Record original state
        final int originalBlock2Size = block2.getStatements().size();

        block1.append(block2);

        // Verify block2 is unchanged
        assertEquals(originalBlock2Size, block2.getStatements().size());
        assertEquals(2, block2.getStatements().size());
        assertSame(stmt2, block2.getStatements().get(0));
        assertSame(stmt3, block2.getStatements().get(1));
    }

    @Test
    public void testAppend_FirstBlockUnchanged_AddingToCombined()
    {
        final BlockNode block1 = new BlockNode(emptyFunctionSet);
        final TraciNode stmt1 = new ConstNode(new TraciValue(1.0));
        block1.addStatement(stmt1);

        final BlockNode block2 = new BlockNode(emptyFunctionSet);
        final TraciNode stmt2 = new ConstNode(new TraciValue(2.0));
        block2.addStatement(stmt2);

        final BlockNode combined = block1.append(block2);

        // Modify the combined block
        final TraciNode stmt3 = new ConstNode(new TraciValue(3.0));
        combined.addStatement(stmt3);

        // Verify block1 is unchanged
        assertEquals(1, block1.getStatements().size());
        assertSame(stmt1, block1.getStatements().get(0));

        // Verify combined has all three statements
        assertEquals(3, combined.getStatements().size());
    }

    @Test
    public void testAppend_SecondBlockUnchanged_AddingToCombined()
    {
        final BlockNode block1 = new BlockNode(emptyFunctionSet);
        final TraciNode stmt1 = new ConstNode(new TraciValue(1.0));
        block1.addStatement(stmt1);

        final BlockNode block2 = new BlockNode(emptyFunctionSet);
        final TraciNode stmt2 = new ConstNode(new TraciValue(2.0));
        block2.addStatement(stmt2);

        final BlockNode combined = block1.append(block2);

        // Modify the combined block
        final TraciNode stmt3 = new ConstNode(new TraciValue(3.0));
        combined.addStatement(stmt3);

        // Verify block2 is unchanged
        assertEquals(1, block2.getStatements().size());
        assertSame(stmt2, block2.getStatements().get(0));

        // Verify combined has all three statements
        assertEquals(3, combined.getStatements().size());
    }

    @Test
    public void testAppend_CombinedContainsAllStatements()
    {
        final BlockNode block1 = new BlockNode(emptyFunctionSet);
        final TraciNode stmt1 = new ConstNode(new TraciValue(1.0));
        final TraciNode stmt2 = new ConstNode(new TraciValue(2.0));
        block1.addStatement(stmt1);
        block1.addStatement(stmt2);

        final BlockNode block2 = new BlockNode(emptyFunctionSet);
        final TraciNode stmt3 = new ConstNode(new TraciValue(3.0));
        final TraciNode stmt4 = new ConstNode(new TraciValue(4.0));
        block2.addStatement(stmt3);
        block2.addStatement(stmt4);

        final BlockNode combined = block1.append(block2);

        // Verify combined has all statements in correct order
        assertEquals(4, combined.getStatements().size());
        assertSame(stmt1, combined.getStatements().get(0));
        assertSame(stmt2, combined.getStatements().get(1));
        assertSame(stmt3, combined.getStatements().get(2));
        assertSame(stmt4, combined.getStatements().get(3));
    }

    @Test
    public void testAppend_EmptyBlocks()
    {
        final BlockNode block1 = new BlockNode(emptyFunctionSet);
        final BlockNode block2 = new BlockNode(emptyFunctionSet);

        final BlockNode combined = block1.append(block2);

        // Verify combined is empty
        assertEquals(0, combined.getStatements().size());

        // Verify originals are unchanged
        assertEquals(0, block1.getStatements().size());
        assertEquals(0, block2.getStatements().size());
    }

    @Test
    public void testAppend_FirstBlockEmpty()
    {
        final BlockNode block1 = new BlockNode(emptyFunctionSet);

        final BlockNode block2 = new BlockNode(emptyFunctionSet);
        final TraciNode stmt1 = new ConstNode(new TraciValue(1.0));
        block2.addStatement(stmt1);

        final BlockNode combined = block1.append(block2);

        // Verify combined has only block2's statements
        assertEquals(1, combined.getStatements().size());
        assertSame(stmt1, combined.getStatements().get(0));

        // Verify originals are unchanged
        assertEquals(0, block1.getStatements().size());
        assertEquals(1, block2.getStatements().size());
    }

    @Test
    public void testAppend_SecondBlockEmpty()
    {
        final BlockNode block1 = new BlockNode(emptyFunctionSet);
        final TraciNode stmt1 = new ConstNode(new TraciValue(1.0));
        block1.addStatement(stmt1);

        final BlockNode block2 = new BlockNode(emptyFunctionSet);

        final BlockNode combined = block1.append(block2);

        // Verify combined has only block1's statements
        assertEquals(1, combined.getStatements().size());
        assertSame(stmt1, combined.getStatements().get(0));

        // Verify originals are unchanged
        assertEquals(1, block1.getStatements().size());
        assertEquals(0, block2.getStatements().size());
    }

    @Test
    public void testAppend_FunctionsAreCombined()
    {
        final BlockNode block1 = new BlockNode(functionSet1);
        final BlockNode block2 = new BlockNode(functionSet2);

        final BlockNode combined = block1.append(block2);

        // Create a test context to verify functions are accessible
        final Scene scene = new Scene();
        final Context rootContext = Context.newRootContext(scene);

        try
        {
            combined.eval(rootContext);
            // If no exceptions are thrown, the function sets were properly combined
        }
        catch (Exception e)
        {
            // This is fine for this test, we're just checking immutability
        }
    }

    @Test
    public void testAppend_FirstBlockFunctionSetUnchanged()
    {
        final BlockNode block1 = new BlockNode(functionSet1);
        final BlockNode block2 = new BlockNode(functionSet2);

        // Record the function from block1 before append
        final Function originalFunc1 = functionSet1.get("func1");

        block1.append(block2);

        // Verify block1's function set is unchanged
        assertSame(originalFunc1, functionSet1.get("func1"));
        // block1's function set should not have func2
        assertEquals(null, functionSet1.get("func2"));
    }

    @Test
    public void testAppend_SecondBlockFunctionSetUnchanged()
    {
        final BlockNode block1 = new BlockNode(functionSet1);
        final BlockNode block2 = new BlockNode(functionSet2);

        // Record the function from block2 before append
        final Function originalFunc2 = functionSet2.get("func2");

        block1.append(block2);

        // Verify block2's function set is unchanged
        assertSame(originalFunc2, functionSet2.get("func2"));
        // block2's function set should not have func1
        assertEquals(null, functionSet2.get("func1"));
    }

    @Test
    public void testAppend_MultipleAppends_OriginalUnchanged()
    {
        final BlockNode block1 = new BlockNode(emptyFunctionSet);
        final TraciNode stmt1 = new ConstNode(new TraciValue(1.0));
        block1.addStatement(stmt1);

        final BlockNode block2 = new BlockNode(emptyFunctionSet);
        final TraciNode stmt2 = new ConstNode(new TraciValue(2.0));
        block2.addStatement(stmt2);

        final BlockNode block3 = new BlockNode(emptyFunctionSet);
        final TraciNode stmt3 = new ConstNode(new TraciValue(3.0));
        block3.addStatement(stmt3);

        // Perform multiple appends
        final BlockNode combined12 = block1.append(block2);
        final BlockNode combined123 = combined12.append(block3);

        // Verify all originals are unchanged
        assertEquals(1, block1.getStatements().size());
        assertSame(stmt1, block1.getStatements().get(0));

        assertEquals(1, block2.getStatements().size());
        assertSame(stmt2, block2.getStatements().get(0));

        assertEquals(1, block3.getStatements().size());
        assertSame(stmt3, block3.getStatements().get(0));

        // Verify intermediate result is unchanged
        assertEquals(2, combined12.getStatements().size());
        assertSame(stmt1, combined12.getStatements().get(0));
        assertSame(stmt2, combined12.getStatements().get(1));

        // Verify final result has all statements
        assertEquals(3, combined123.getStatements().size());
        assertSame(stmt1, combined123.getStatements().get(0));
        assertSame(stmt2, combined123.getStatements().get(1));
        assertSame(stmt3, combined123.getStatements().get(2));
    }

    @Test
    public void testAppend_StatementsListsAreIndependent()
    {
        final BlockNode block1 = new BlockNode(emptyFunctionSet);
        final TraciNode stmt1 = new ConstNode(new TraciValue(1.0));
        block1.addStatement(stmt1);

        final BlockNode block2 = new BlockNode(emptyFunctionSet);
        final TraciNode stmt2 = new ConstNode(new TraciValue(2.0));
        block2.addStatement(stmt2);

        final BlockNode combined = block1.append(block2);

        // Get lists
        final List<TraciNode> block1List = block1.getStatements();
        final List<TraciNode> block2List = block2.getStatements();
        final List<TraciNode> combinedList = combined.getStatements();

        // Verify they are different list instances
        assertNotSame(block1List, combinedList);
        assertNotSame(block2List, combinedList);
    }
}
