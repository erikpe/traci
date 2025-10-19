package se.ejp.traci.lang.interpreter.functions;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;

import java.util.List;

import org.junit.Before;
import org.junit.Test;

import se.ejp.traci.lang.interpreter.Context;
import se.ejp.traci.lang.interpreter.TraciValue;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import se.ejp.traci.lang.interpreter.node.FunctionCallNode;

public class FunctionSetTest
{
    private Function dummyFunction1;
    private Function dummyFunction2;
    private Function dummyFunction3;

    @Before
    public void setUp()
    {
        // Create dummy functions for testing
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

        dummyFunction3 = new Function()
        {
            @Override
            public TraciValue invoke(FunctionCallNode funcallNode, Context context, List<TraciValue> args)
                    throws InterpreterRuntimeException
            {
                return new TraciValue(3.0);
            }
        };
    }

    @Test
    public void testClone_SimpleFunctionSet()
    {
        final FunctionSet original = new FunctionSet(null);
        original.put("func1", dummyFunction1);
        original.put("func2", dummyFunction2);

        final FunctionSet cloned = original.clone();

        // Verify the clone has the same functions
        assertNotNull(cloned.get("func1"));
        assertNotNull(cloned.get("func2"));
        assertSame(dummyFunction1, cloned.get("func1"));
        assertSame(dummyFunction2, cloned.get("func2"));

        // Verify original and cloned are different objects
        assertNotSame(original, cloned);
    }

    @Test
    public void testClone_OriginalUnchangedWhenClonedIsModified()
    {
        final FunctionSet original = new FunctionSet(null);
        original.put("func1", dummyFunction1);
        original.put("func2", dummyFunction2);

        final FunctionSet cloned = original.clone();

        // Modify the cloned set
        cloned.put("func3", dummyFunction3);

        // Verify original is unchanged
        assertNotNull(original.get("func1"));
        assertNotNull(original.get("func2"));
        assertNull(original.get("func3"));

        // Verify cloned has all functions
        assertNotNull(cloned.get("func1"));
        assertNotNull(cloned.get("func2"));
        assertNotNull(cloned.get("func3"));
    }

    @Test
    public void testClone_ClonedUnchangedWhenOriginalIsModified()
    {
        final FunctionSet original = new FunctionSet(null);
        original.put("func1", dummyFunction1);
        original.put("func2", dummyFunction2);

        final FunctionSet cloned = original.clone();

        // Modify the original set
        original.put("func3", dummyFunction3);

        // Verify cloned is unchanged
        assertNotNull(cloned.get("func1"));
        assertNotNull(cloned.get("func2"));
        assertNull(cloned.get("func3"));

        // Verify original has all functions
        assertNotNull(original.get("func1"));
        assertNotNull(original.get("func2"));
        assertNotNull(original.get("func3"));
    }

    @Test
    public void testClone_WithOuterScope()
    {
        final FunctionSet outer = new FunctionSet(null);
        outer.put("outerFunc", dummyFunction1);

        final FunctionSet original = new FunctionSet(outer);
        original.put("innerFunc", dummyFunction2);

        final FunctionSet cloned = original.clone();

        // Verify cloned can access both inner and outer functions
        assertNotNull(cloned.get("innerFunc"));
        assertNotNull(cloned.get("outerFunc"));
        assertSame(dummyFunction2, cloned.get("innerFunc"));
        assertSame(dummyFunction1, cloned.get("outerFunc"));
    }

    @Test
    public void testClone_OuterScopeIsAlsoCloned()
    {
        final FunctionSet outer = new FunctionSet(null);
        outer.put("outerFunc", dummyFunction1);

        final FunctionSet original = new FunctionSet(outer);
        original.put("innerFunc", dummyFunction2);

        final FunctionSet cloned = original.clone();

        // Modify the original outer scope
        outer.put("newOuterFunc", dummyFunction3);

        // Verify cloned outer scope is unchanged
        assertNotNull(cloned.get("outerFunc"));
        assertNull(cloned.get("newOuterFunc"));

        // Verify original can access the new function through outer scope
        assertNotNull(original.get("newOuterFunc"));
    }

    @Test
    public void testClone_EmptyFunctionSet()
    {
        final FunctionSet original = new FunctionSet(null);
        final FunctionSet cloned = original.clone();

        assertNotNull(cloned);
        assertNotSame(original, cloned);
        assertNull(cloned.get("anyFunction"));
    }

    @Test
    public void testMergeFrom_SimpleMerge()
    {
        final FunctionSet target = new FunctionSet(null);
        target.put("func1", dummyFunction1);

        final FunctionSet source = new FunctionSet(null);
        source.put("func2", dummyFunction2);
        source.put("func3", dummyFunction3);

        target.mergeFrom(source);

        // Verify target has all functions
        assertNotNull(target.get("func1"));
        assertNotNull(target.get("func2"));
        assertNotNull(target.get("func3"));
        assertSame(dummyFunction1, target.get("func1"));
        assertSame(dummyFunction2, target.get("func2"));
        assertSame(dummyFunction3, target.get("func3"));
    }

    @Test
    public void testMergeFrom_SourceUnchanged()
    {
        final FunctionSet target = new FunctionSet(null);
        target.put("func1", dummyFunction1);

        final FunctionSet source = new FunctionSet(null);
        source.put("func2", dummyFunction2);

        target.mergeFrom(source);

        // Modify target after merge
        target.put("func3", dummyFunction3);

        // Verify source is unchanged
        assertNotNull(source.get("func2"));
        assertNull(source.get("func1"));
        assertNull(source.get("func3"));
    }

    @Test
    public void testMergeFrom_Overwrite()
    {
        final FunctionSet target = new FunctionSet(null);
        target.put("func1", dummyFunction1);
        target.put("func2", dummyFunction2);

        final FunctionSet source = new FunctionSet(null);
        source.put("func2", dummyFunction3); // Overwrite func2

        target.mergeFrom(source);

        // Verify func2 was overwritten
        assertNotNull(target.get("func1"));
        assertNotNull(target.get("func2"));
        assertSame(dummyFunction1, target.get("func1"));
        assertSame(dummyFunction3, target.get("func2")); // Should be the new one
    }

    @Test
    public void testMergeFrom_NullSource()
    {
        final FunctionSet target = new FunctionSet(null);
        target.put("func1", dummyFunction1);

        target.mergeFrom(null);

        // Verify target is unchanged
        assertNotNull(target.get("func1"));
        assertSame(dummyFunction1, target.get("func1"));
    }

    @Test
    public void testMergeFrom_EmptySource()
    {
        final FunctionSet target = new FunctionSet(null);
        target.put("func1", dummyFunction1);

        final FunctionSet source = new FunctionSet(null);

        target.mergeFrom(source);

        // Verify target is unchanged
        assertNotNull(target.get("func1"));
        assertSame(dummyFunction1, target.get("func1"));
    }

    @Test
    public void testMergeFrom_OnlyMergesDirectFunctions()
    {
        final FunctionSet outerSource = new FunctionSet(null);
        outerSource.put("outerFunc", dummyFunction1);

        final FunctionSet source = new FunctionSet(outerSource);
        source.put("innerFunc", dummyFunction2);

        final FunctionSet target = new FunctionSet(null);

        target.mergeFrom(source);

        // Verify only direct functions from source are merged, not outer scope
        assertNotNull(target.get("innerFunc"));
        assertNull(target.get("outerFunc"));
    }

    @Test
    public void testToString_ContainsAllFunctionIds()
    {
        final FunctionSet outer = new FunctionSet(null);
        outer.put("outerFunc", dummyFunction1);

        final FunctionSet functionSet = new FunctionSet(outer);
        functionSet.put("innerFunc1", dummyFunction2);
        functionSet.put("innerFunc2", dummyFunction3);

        final String result = functionSet.toString();

        // Verify all function IDs are present
        assertNotNull(result);
        assert(result.contains("outerFunc"));
        assert(result.contains("innerFunc1"));
        assert(result.contains("innerFunc2"));
    }
}
